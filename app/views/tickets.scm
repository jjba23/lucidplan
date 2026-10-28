;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views tickets)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 regex)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan html)
  #:use-module (kracht prelude)
  #:use-module (lucidplan attachments)
  #:use-module (app views epics)
  #:use-module (lucidplan domain)
  #:use-module (lucidplan domain ticket-comment)
  #:use-module (hygguile hygguile)
  #:use-module (artanis security nss)
  #:export (render-ticket render-ticket-listitem
                          html-from-tickets
                          html-new-ticket-form
                          html-edit-ticket-form
                          html-ticket-form
                          html-ticket-collaboration
                          render-quiet-notice))

;; =============================================================================
;; Reusable Styles
;; =============================================================================

(define %card-face
  (mk-class (append '(flex flex-col
                           gap-2
                           px-3
                           sm:px-4
                           md:px-6
                           py-4
                           sm:py-6
                           md:py-8
                           rounded-xl
                           md:rounded-2xl
                           dark:border-onyx-100/75
                           border-stone-300/75
                           shadow-md
                           border-2) frosted-tint-classes)))

;; What a field of a card is called: quiet and set apart from what it holds,
;; so a reader scanning the card reads the values and finds the names when
;; they look for them.  A card of controls reads as a form either way; the
;; small, spaced capitals are what keep the names from competing with the
;; answers under them.
(define %label-face
  (mk-class '(font-sans text-xs font-medium uppercase tracking-wide
                        text-onyx-500 dark:text-stone-500)))

;; What a ticket's own reference is written in: the project and the number,
;; told apart from the title and from every other line by its face rather than
;; by an indent, so a column of tickets lines up.  It is a fact about the
;; ticket, not a heading, so it is quieter than the title it sits under.
(define %mono-face
  (mk-class '(font-mono text-xs sm:text-sm
                        text-onyx-500 dark:text-stone-500)))

;; The card a ticket's row is the first line of.  It is one card from top to
;; bottom: the row a reader clicks, and -- beneath it -- the ticket itself,
;; uncovered rather than inserted (see `lp-disclosure' in
;; `pub/css/lucidplan-luxe.css').  The surface lives here, on the card, and not
;; on the row, so a closed row and an open one are the same object in two
;; states rather than two different objects.
(define %list-item-face
  (mk-class (append '(flex flex-col
                           rounded-xl
                           md:rounded-2xl
                           dark:border-onyx-100/75
                           border-stone-300/75
                           border-2
                           shadow-md
                           lp-disclosure)
                     frosted-tint-classes)))

;; One line, always: the grip, the type's glyph, the title, the link and the
;; mark that says the row opens.  Nothing else rides here -- the triage pills
;; belong to the card the row unfolds into -- because a wider row would either
;; wrap, and stop reading as a line, or squeeze the title it exists to show
;; down to a few characters.
(define %list-header-face
  (mk-class '(lp-disclosure-summary)))

(define %list-chevron-face
  (mk-class '(text-onyx-700 dark:text-stone-300 text-lg shrink-0
              lp-disclosure-chevron)))

(define %list-title-face
  (mk-class '(font-sans text-sm
                        sm:text-md
                        font-bold
                        wrap-break-word
                        grow
                        leading-5
                        line-clamp-2
                        sm:line-clamp-none)))

(define %list-link-face
  (mk-class '(flex flex-row
                   flex-wrap
                   gap-2
                   items-center
                   justify-start
                   no-underline)))

;; =============================================================================
;;  UI Helper Components
;; =============================================================================

;; =============================================================================
;;  The time a ticket has cost
;; =============================================================================

;; One line of the time a ticket has cost: how long, and how that reads in
;; hours.  The two are said together because a team reports in hours and logs
;; in hours and minutes, and neither form is the one the other is written in.
(define (render-logged-time G_ minutes)
  `(span (@ (class ,(mk-class '(text-sm text-onyx-700 dark:text-stone-300))))
         (strong ,(format-duration minutes))
         " "
         (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                               dark:text-stone-500))))
                ,(format #f "(~a h)" (format-hours minutes)))))


;; =============================================================================
;;  The two ladders a ticket is triaged with
;; =============================================================================

;; The form and the ticket's own page offer the same rows, so the rows are
;; built in one place -- and the rows a ladder offers are the statuses the
;; page was built with (`specs': a project's own list, or the instance's) and
;; the values the schema holds for types, with the label and the glyph a
;; reader is shown hung off them here.  A value with no label of its own is
;; shown as the value itself, so a ladder that grows still renders a row
;; rather than dropping one.
;;
;; A row's label and its mark are resolved by the shared helpers in
;; `(lucidplan html)' -- `status-label' is the label a reader sees, translated
;; when it is one of the shipped labels, and `status-icon-node' is the mark --
;; and both read the ladder the page was built with, so this file only has to
;; say which ladder that is.
;;
;; A row's glyph is a character rather than an icon component: a native menu
;; lays out the text of a row and renders nothing nested inside it, so the
;; character comes from the ladder the page was built with (see
;; `ticket-status-glyph').
(define (ticket-type-label G_ type)
  (cond
    ((equal? type "task")
     (G_ "Task"))
    ((equal? type "bug")
     (G_ "Bug"))
    ((equal? type "story")
     (G_ "Story"))
    ((equal? type "spike")
     (G_ "Spike"))
    ((equal? type "time-box")
     (G_ "Time Box"))
    (else type)))

;; The rows of the epic menu: the absence of an epic first -- a ticket belongs
;; to none far more often than it belongs to one -- then each of the project's
;; epics under the name a reader knows it by.  The value a row submits is the
;; epic's slug, which is what the route that writes it resolves.
;; A label, as the word it is.  It is not a link and not a button: what a
;; reader does with a label is filter by it, and the filter is the panel above
;; the list (see `epic-filter-row'), which is where the same word appears once
;; per project rather than once per ticket.
(define (label-chip label)
  `(span (@ (class ,(mk-class '(lp-label-chip))))
         ,label))

(define (epic-choices G_ epics selected)
  (cons (option #:value ""
                #:content (G_ "no epic")
                #:selected-value (if selected
                                     (assq-ref selected
                                               'human-id)
                                     ""))
        (map (lambda (epic)
               (option #:value (assq-ref epic
                                         'human-id)
                       #:content (assq-ref epic
                                           'title)
                       #:selected-value (if selected
                                            (assq-ref selected
                                                      'human-id)
                                            "")))
             epics)))

(define (ticket-choices G_ values label-for glyph-for selected)
  (map (lambda (value)
         (glyph-option #:value value
                       #:glyph (glyph-for value)
                       #:content (label-for G_ value)
                       #:selected-value selected)) values))

(define (ticket-status-choices G_ specs selected)
  (map (lambda (value)
         (glyph-option #:value value
                       #:glyph (ticket-status-glyph specs value)
                       #:content (status-label G_ specs value)
                       #:selected-value selected))
       (ticket-status-values specs)))

(define (ticket-type-choices G_ selected)
  (ticket-choices G_ ticket-types ticket-type-label glyph-for-ticket-type
                  selected))

;; The two estimation menus.  Their first row is not a value but the absence
;; of one: an unestimated ticket is a ticket nobody has looked at yet, and
;; the menus say so in words rather than offering a zero that would read as
;; an estimate somebody made.
(define (ticket-point-choices G_ selected)
  (cons (option #:value ""
                #:content (G_ "unestimated")
                #:selected-value selected)
        (map (lambda (points)
               (option #:value (number->string points)
                       #:content (format #f "~a" points)
                       #:selected-value selected)) ticket-points)))

(define (ticket-size-choices G_ selected)
  (cons (option #:value ""
                #:content (G_ "no size")
                #:selected-value selected)
        (map (lambda (size)
               (option #:value size
                       #:content (ticket-size-label size)
                       #:selected-value selected)) ticket-sizes)))

;; =============================================================================
;;  Changing a ticket in place
;; =============================================================================

;; The card a change is confirmed with lives in the page as an inert
;; `template' until it is needed: it is hygguile's own frosted notification
;; card, so the confirmation looks like every other notice in the framework
;; rather than like a second card built to match.  The script that puts it in
;; the tray is in `pub/js/tickets.js'.
;;
;; What the card says travels with it as data -- the template holds the
;; wording of both a change that was written and one that was not, and the
;; field that was changed lends its own name to the sentence -- so no
;; sentence is written into the script, where it could not be translated.
(define %quiet-notice-template-id
  "lucidplan-quiet-notice")

(define (render-quiet-notice G_)
  `((template (@ (id ,%quiet-notice-template-id)
                 (data-saved-label ,(G_ "saved"))
                 (data-failed-label ,(G_ "not saved"))
                 (data-failed-body ,(G_ "the ticket was left as it was")))
              ,@(notification-card))))

(define* (render-inline-choice G_
                               #:key project
                               human-id
                               field
                               value
                               options
                               (id-suffix ""))
  "A field of a ticket as the small control it is on the ticket's own page.

The control is a native menu wearing a tint rather than a box, so what it
shows is the value the ticket holds and a change is one gesture rather than
a trip through the edit form.  It posts as soon as it is changed and is
confirmed by a card in the tray.  The label is there for assistive
technology -- the column above the control already says which field this is.

The menu is named and identified after the field it changes, which is what
the control needs on a page holding one of them.  A page holding one per
row -- the backlog, where every row carries its own pair -- passes
@var{id-suffix} to tell the rows apart: an id names one element, and a page
of thirty-three controls sharing one name is a page whose labels all point
at the first of them."
  `(div (@ (class ,(mk-class (append '(px-2 py-1 w-fit rounded-lg)
                                     chip-tint-classes)))
           ;; What the script needs is here rather than inferred from the
           ;; markup: which field this is, the name it answers to in the
           ;; notice, and the value the ticket held before the menu was
           ;; touched, which is what a change that never reached the server is
           ;; put back to.
           (data-inline-field ,field)
           (data-field-label ,(G_ field))
           (data-current-value ,value))
        ,@(select #:name (string->symbol (format #f "inline-~a~a" field
                                                 id-suffix))
                  #:on-change (format #f
                               "updateTicketField(this, '~a', '~a', '~a')"
                               project human-id field)
                  #:label (G_ field)
                  #:label-css '(sr-only)
                  #:options options
                  #:select-classes '(w-auto max-w-full
                                            bg-transparent
                                            border-0
                                            rounded-full
                                            px-0
                                            py-0
                                            text-sm
                                            font-medium
                                            font-sans
                                            text-onyx-700
                                            dark:text-stone-300
                                            cursor-pointer
                                            outline-none
                                            focus:ring-2)
                  #:select-flex-css '(flex flex-row items-center gap-1 my-0
                                           w-fit))))

;; One fact about a ticket, as the chip a reader scans rather than the pair of
;; lines a form would give it: what the fact is called and what it holds stand
;; on one line, so a card can carry six of them without becoming six
;; paragraphs.  The value is normally text -- a priority, a name, a size -- and
;; a fact that is a mark instead (a status is a glyph *and* a word) passes the
;; nodes it is drawn from, so both kinds sit in the same chip.
(define (render-fact-chip label value)
  `(span (@ (class ,(mk-class (append '(inline-flex flex-row gap-2
                                                 items-center
                                                 px-2 py-1 rounded-lg)
                                        chip-tint-classes))))
         (small (@ (class ,(mk-class '(uppercase tracking-wide text-xs
                                                 text-onyx-500
                                                 dark:text-stone-500))))
                ,label)
         (span (@ (class ,(mk-class '(flex flex-row gap-1 items-center
                                              text-sm font-medium
                                              text-onyx-700
                                              dark:text-stone-300))))
               ,@(cond
                   ((string? value) (list value))
                   ((list? value) value)
                   (else (list (format #f "~a" value)))))))

(define (render-toggle-button label js-action)
  `(button (@ (class ,(mk-class '(text-sm font-semibold
                                          underline
                                          cursor-pointer
                                          text-onyx-700
                                          dark:text-stone-300
                                          hover:text-onyx-900
                                          dark:hover:text-stone-100)))
              (onclick ,js-action))
           ,label))

(define* (render-ticket G_
                        #:key project
                        ticket
                        (assignee #f)
                        ;; Whether this card says what the ticket is called.
                        ;; The card that is a row's unfolded body does not: the
                        ;; row above it is the ticket's name, and repeating it
                        ;; one line lower would make the card read as a second
                        ;; thing rather than as the rest of the first.
                        (headline? #t)
                        (editable? #f)
                        ;; Whether this card is the ticket's own page, where the
                        ;; team can be asked to estimate it at a table.  A row
                        ;; in a backlog is a row and not a page, so the control
                        ;; is offered by the one card that has room to say what
                        ;; opening a table means.
                        (planning-table? #f)
                        ;; The epics of this ticket's project, so the card can
                        ;; say which body of work it is a piece of and offer
                        ;; the one menu that changes it.
                        (epics '())
                        (statuses (default-ticket-statuses))
                        (logged-minutes 0)
                        (today (today-iso))
                        (id-suffix ""))
  (let* ((t-ref (lambda (k)
                  (assq-ref ticket k)))
         (id (t-ref 'id))
         (human-id (t-ref 'human-id))
         (title (t-ref 'title))
         (status (t-ref 'ticket-status))
         (type (t-ref 'ticket-type))
         (desc (t-ref 'description))
         (priority (t-ref 'priority))
         (points (t-ref 'estimate-points))
         (size (t-ref 'estimate-size))
         (due-on (t-ref 'due-on))
         (epic-id (or (t-ref 'epic-id) ""))
         ;; The epic this ticket is a piece of, found among the epics the page
         ;; was handed -- or nothing, which is the ordinary ticket.
         (epic (find (lambda (e)
                       (equal? (format #f "~a"
                                       (assq-ref e
                                                 'id))
                               (format #f "~a" epic-id)))
                     epics))

         ;; The ladder a table opened from here would deal: the one this ticket
         ;; is already estimated on, where it is estimated on one and not the
         ;; other, and story points otherwise -- which is the answer a reader
         ;; opening the table would have given anyway.
         (opened-deck (if (and (string? size)
                               (not (string-null? size))
                               (not points))
                          "sizes" "points"))

         (edit-url (format #f "/tickets/~a/id/~a/edit" project human-id))
         (delete-url (format #f "/tickets/~a/delete" project)))

    `(div (@ (class ,%card-face))

          ;; Header: what the ticket is called, which ticket it is.  The card
          ;; that is a row's unfolded body says the reference alone -- the row
          ;; above it is the ticket's name, and the row is what folds it away
          ;; again -- so this is drawn only where the card is the whole page.
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            items-start
                                            justify-between))))
               (div (@ (class ,(mk-class '(flex flex-col gap-1 min-w-0 grow))))
                    ,@(if headline?
                          `((h4 (@ (class ,(mk-class '(text-xl sm:text-2xl
                                                             font-bold
                                                             leading-8
                                                             wrap-break-word))))
                                ,(org2html (html-unescape title))))
                          '())
                    (small (@ (class ,%mono-face))
                           ,(format #f "~a-~s" project human-id))))

          ;; Meta Info (Status & Type).  Where the reader can change them each
          ;; of the two is the control that posts its own change; where they
          ;; cannot -- a reader who is not signed in -- they stay the
          ;; read-only badges they read as at a glance.  Either way what they
          ;; say is the label a reader knows the value by, never the value
          ;; itself: `work-in-progress' and `time-box' are how the schema
          ;; spells a state, not how a reader names it.
          (div (@ (class ,(mk-class '(grid grid-cols-2 sm:grid-cols-4
                                           gap-x-6 gap-y-4 items-start))))
               ,@(if editable?
                     `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
                            (small (@ (class ,%label-face))
                                   ,(G_ "status"))
                            ,(render-inline-choice G_
                                                   #:project project
                                                   #:human-id human-id
                                                   #:field "status"
                                                   #:value status
                                                   #:id-suffix id-suffix
                                                   #:options (ticket-status-choices
                                                              G_ statuses
                                                              status)))
                       (div (@ (class ,(mk-class '(flex flex-col gap-2))))
                            (small (@ (class ,%label-face))
                                   ,(G_ "type"))
                            ,(render-inline-choice G_
                                                   #:project project
                                                   #:human-id human-id
                                                   #:field "type"
                                                   #:value type
                                                   #:id-suffix id-suffix
                                                   #:options (ticket-type-choices
                                                              G_ type)))
                       ;; The two estimates are changed the same way the two
                       ;; ladders above are: a menu that posts what it shows,
                       ;; from the ticket's own page.
                       (div (@ (class ,(mk-class '(flex flex-col gap-2))))
                            (small (@ (class ,%label-face))
                                   ,(G_ "story points"))
                            ,(render-inline-choice G_
                                                   #:project project
                                                   #:human-id human-id
                                                   #:field "estimate-points"
                                                   #:value (if points
                                                               (format #f "~a"
                                                                       points)
                                                               "")
                                                   #:id-suffix id-suffix
                                                   #:options (ticket-point-choices
                                                              G_
                                                              (if points
                                                                  (format #f
                                                                   "~a" points)
                                                                  ""))))
                       (div (@ (class ,(mk-class '(flex flex-col gap-2))))
                            (small (@ (class ,%label-face))
                                   ,(G_ "size"))
                            ,(render-inline-choice G_
                                                   #:project project
                                                   #:human-id human-id
                                                   #:field "estimate-size"
                                                   #:value (or size "")
                                                   #:id-suffix id-suffix
                                                   #:options (ticket-size-choices
                                                              G_
                                                              (or size "")))))
                     ;; A reader who cannot change them reads the same two
                     ;; facts as chips, drawn the way this card's other facts
                     ;; are drawn, rather than as a second kind of badge that
                     ;; only ever appears on the card nobody can edit.
                     (list (render-fact-chip
                            (G_ "status")
                            (append (status-icon-node statuses status)
                                    (list (status-label G_ statuses status))))
                           (render-fact-chip
                            (G_ "type")
                            (append (emoji-for-ticket-type type)
                                    (list (ticket-type-label G_ type)))))))

          ;; Which body of work this ticket is a piece of.  It is its own row
          ;; rather than a fifth cell of the grid above, because it is the one
          ;; fact about a ticket that is about other tickets: a reader who can
          ;; change it gets the menu every other fact is changed with, and a
          ;; reader who cannot sees the epic's own colour and name.
          ,@(if (pair? epics)
                `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
                       (small (@ (class ,%label-face))
                              ,(G_ "epic"))
                       ,@(if editable?
                             (list (render-inline-choice G_
                                                         #:project project
                                                         #:human-id human-id
                                                         #:field "epic"
                                                         #:value (if epic
                                                                     (assq-ref
                                                                      epic
                                                                      'human-id)
                                                                     "")
                                                         #:id-suffix id-suffix
                                                         #:options (epic-choices
                                                                    G_ epics
                                                                    epic)))
                             (list (if epic
                                       (epic-chip epic)
                                       `(span (@ (class ,(mk-class '(text-sm
                                                                     text-onyx-500
                                                                     dark:text-stone-500))))
                                              ,(G_ "no epic")))))))
                '())

          ;; The words this ticket is tagged with.  A ticket with none is a
          ;; ticket with no row, which is the ordinary one.
          ,@(let ((labels (ticket-labels ticket)))
              (if (pair? labels)
                  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                        items-center gap-1))))
                         ,@(map label-chip labels)))
                  '()))

          ;; What else is known about the ticket, as one row of chips: where it
          ;; stands in the team's plan -- the deadline read as a state rather
          ;; than as a date -- and, as each of them has something to say, what
          ;; it has cost in hours, who is on it, and how important it is.  A
          ;; fact with nothing to say is left out rather than drawn empty, so
          ;; a ticket with nothing to add has no empty row.
          ,@(let ((facts (append (render-due-badge G_ due-on today)
                                 (if (> logged-minutes 0)
                                     (list (render-fact-chip
                                            (G_ "time logged")
                                            (render-logged-time G_
                                                                logged-minutes)))
                                     '())
                                 (if (and (not (nil? priority))
                                          (> priority 0))
                                     (list (render-fact-chip (G_ "priority")
                                                             priority))
                                     '())
                                 (if (not (nil? assignee))
                                     (list (render-fact-chip (G_ "assignee")
                                                             assignee))
                                     '())
                                 (if (and (not editable?) points)
                                     (list (render-fact-chip
                                            (G_ "story points") points))
                                     '())
                                 (if (and (not editable?)
                                          (not (nil? size)))
                                     (list (render-fact-chip
                                            (G_ "size")
                                            (ticket-size-label size)))
                                     '()))))
              (if (pair? facts)
                  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                        gap-2 items-center))))
                         ,@facts))
                  '()))

          ;; Description, set as prose rather than as another value typed into
          ;; the card: a rule down its side and a readable measure, so a long
          ;; one reads like the note it is.
          ,(render-if-nestring (lambda (d)
                                 `((div (@ (class ,(mk-class '(flex flex-col
                                                                    gap-1
                                                                    my-1))))
                                        (small (@ (class ,%label-face))
                                               ,(G_ "description"))
                                        (div (@ (class ,(mk-class '(border-l-2
                                                                    border-stone-300/75
                                                                    dark:border-onyx-100/75
                                                                    pl-3 sm:pl-4
                                                                    text-md
                                                                    leading-7
                                                                    max-w-prose
                                                                    whitespace-pre-wrap
                                                                    text-onyx-700
                                                                    dark:text-stone-300))))
                                             ,(org2html (html-unescape d)))))) desc)

          ;; What a reader can do with the ticket: ask the team for an
          ;; estimate, change it, delete it.  The rule above the section is
          ;; what sets the doing apart from everything the card says about the
          ;; ticket, and the table is opened from here rather than from the
          ;; table's own page because this is the page that knows how the
          ;; ticket is estimated.
          ;;
          ;; The three sit on one line that wraps -- the meeting first, where
          ;; a reader reads, and the two edits at the end, where a hand goes
          ;; looking for them -- rather than in a column of their own, which
          ;; would leave the card's last third empty on a wide page and push
          ;; the meeting below the fold on a narrow one.
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-x-4 gap-y-3
                                             items-end justify-between
                                             mt-2 pt-4
                                             border-t-2
                                             border-stone-300/50
                                             dark:border-onyx-100/50))))
               ;; Estimating a ticket is the one thing a reader does with one
               ;; that a page of its own cannot do: it is a meeting.  The table
               ;; is opened *for this ticket*, so every round played at it is
               ;; estimated for this ticket and no number is typed into the
               ;; push at the end.
               ,@(if planning-table?
                     (list `(div (@ (class ,(mk-class '(flex flex-col
                                                                gap-1))))
                                 (small (@ (class ,%label-face))
                                        ,(G_ "estimate with a table"))
                                 (form (@ (enctype
                                           application/x-www-form-urlencoded)
                                          (method POST)
                                          (action ,(format #f
                                                           "/tickets/~a/id/~a/planning-table"
                                                           project human-id)))
                                       (div (@ (class ,(mk-class '(flex flex-row
                                                                         flex-wrap
                                                                         gap-2
                                                                         items-center))))
                                            ,(select #:label
                                                     (G_ "how the table estimates")
                                                     #:label-css '(sr-only)
                                                     #:name "deck"
                                                     #:select-flex-css
                                                     '(my-0 flex flex-col gap-1
                                                             w-full sm:w-fit)
                                                     #:options
                                                     (map (lambda (deck)
                                                            (option #:value deck
                                                                    #:content
                                                                    (deck-label
                                                                     G_ deck)
                                                                    #:selected-value
                                                                    opened-deck))
                                                          poker-decks))
                                            ,(submit-button
                                              (G_ "open the table"))))))
                     '())

               ;; The buttons: changing the ticket, and taking it away.  Both
               ;; are quiet -- the card is already a surface, and a surface
               ;; with a red plate on it is a card that is shouting -- but the
               ;; one that cannot be taken back keeps its red ink, which is
               ;; what tells it apart without a box.
               (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-1
                                                 items-center shrink-0
                                                 -ml-2))))
                    ,(quiet-link #:icon 'pencil
                                 #:label (G_ "edit")
                                 #:href edit-url)
                    (form (@ (enctype application/x-www-form-urlencoded)
                             (method POST)
                             (action ,delete-url))
                          (input (@ (hidden "")
                                    (name "id")
                                    (value ,id)))
                          ,(quiet-submit
                            `(span (@ (class ,(mk-class '(items-center
                                                           flex
                                                           flex-row
                                                           gap-1))))
                                   ,(icon #:type 'trash
                                          #:icon-css '(text-md))
                                   ,(G_ "delete")))))))))

;; A row reads as one line -- the grip, the type's glyph, the title and the
;; link -- and unfolds into the ticket itself, which is where it is triaged:
;; the card a row opens carries the same two controls the ticket's own page
;; does, for the same reader the ticket's own page asks (a reader who is
;; signed in).  A reader who is not signed in gets the card as it reads at a
;; glance, which is what the row's expanded body has always been.
(define* (render-ticket-listitem G_
                                 #:key project
                                 ticket
                                 ticket-id-to-human-id
                                 ticket-assignee
                                 user-names
                                 (statuses (default-ticket-statuses))
                                 ;; The epics of the project, so a row can wear
                                 ;; the one its ticket belongs to.
                                 (epics '())
                                 (editable? #f)
                                 (reorderable? #f))
  (let* ((t-ref (lambda (k)
                  (assq-ref ticket k)))
         (id (t-ref 'id))
         (human-id (t-ref 'human-id))
         (title (t-ref 'title))
         (type (t-ref 'ticket-type))
         (priority (or (t-ref 'priority) 0))

         (item-id (format #f "listitem-~a" id))
         (details-id (format #f "ticket-body-~a" id))
         (ticket-url (format #f "/tickets/~a/id/~a" project human-id))
         (assignee-user (find (lambda (u)
                                (equal? (assq-ref u
                                                  'id) ticket-assignee))
                              user-names)))

    `(div (@ (class ,%list-item-face)
             (id ,item-id)
             (data-ticket-id ,human-id)
             (data-priority ,(number->string priority)))

          ;; The row: the whole of the card while it is closed, and the whole
          ;; of what a reader clicks, tabs to and presses Enter on.  The script
          ;; is what turns it into a control (see `pub/js/lucidplan-luxe.js'),
          ;; so without the script it is a line of text above the ticket it
          ;; belongs to -- which is what it was before it could be clicked.
          (div (@ (class ,%list-header-face))
               ;; The tick that puts this row in a bulk change.  It is a
               ;; control inside the row, which the shared script leaves alone:
               ;; a click on it selects the ticket rather than opening the
               ;; card, and nothing else in the row behaves differently
               ;; because it is there.
               ,@(if editable?
                     (list `(input (@ (type "checkbox")
                                      (class "lp-bulk-check h-4 w-4
                                              cursor-pointer shrink-0")
                                      (aria-label ,(format #f "~a ~a"
                                                           (G_ "select")
                                                           (t-ref 'human-id)))
                                      (data-ticket-id ,(t-ref 'human-id)))))
                     '())

               ;; The grip is what makes a row draggable, and dragging a row
               ;; reorders the backlog -- a change to the tickets.  It is read
               ;; by the endpoint the row reorders, so it is offered only to a
               ;; reader that endpoint accepts, and only while the list is in
               ;; an order a drop can hold: a rearrangement is written as the
               ;; tickets' priorities, and under any other sort the row would
               ;; slide back into place on the next visit.  Which sorts those
               ;; are is decided by the page, which knows the list's order.
               ,@(if reorderable? grab-indicator
                     '())
               ,(emoji-for-ticket-type type)
               (h5 (@ (class ,%list-title-face))
                   ,(org2html (html-unescape title)))

               ;; The words it is tagged with, in the row as well as in the
               ;; card: a row is what a reader scans, and a tag nobody sees
               ;; until they open the card is a tag nobody filters by.
               ,@(map label-chip (ticket-labels ticket))

               ;; The body of work this ticket is a piece of, if any: a
               ;; colour and a name, in the one place a row has room for a
               ;; fact that is not about the ticket alone.
               ,@(if (pair? epics)
                     (let ((mine (find (lambda (e)
                                         (equal? (format #f "~a"
                                                         (assq-ref e
                                                                   'id))
                                                 (format #f "~a"
                                                         (or (t-ref
                                                              'epic-id)
                                                             ""))))
                                       epics)))
                       (if mine
                           (list (epic-chip mine))
                           '()))
                     '())

               ;; Direct Link Icon
               (div (a (@ (class ,%list-link-face)
                          (href ,ticket-url))
                       ,(icon #:type 'link
                              #:icon-css '(text-md text-onyx-700
                                           dark:text-stone-300
                                           hover:text-asparagus-500
                                           dark:hover:text-asparagus-300))))

               ;; The mark that says the row opens, and turns over when it has.
               ,(icon #:type 'caret-down
                      #:icon-css '(text-onyx-700 dark:text-stone-300 text-lg
                                   lp-disclosure-chevron)))

          ;; The rest of the card: the ticket itself, uncovered rather than
          ;; inserted.
          (div (@ (id ,details-id)
                  (class ,(mk-class '(lp-disclosure-body))))
               (div (@ (class ,(mk-class '(lp-disclosure-inner))))
                    ,(render-ticket G_
                                    #:project project
                                    #:ticket ticket
                                    ;; The card is inside a list, so its two
                                    ;; controls are told apart from the pairs
                                    ;; the other rows carry; the page says
                                    ;; which row by naming it.
                                    #:editable? editable?
                                    #:statuses statuses
                                    ;; The row above already says what the
                                    ;; ticket is called, and the row is what
                                    ;; folds this card away again; so the card
                                    ;; says the ticket's reference only.
                                    #:headline? #f
                                    #:id-suffix (format #f "-row-~a" human-id)
                                    #:assignee (if (nil? assignee-user) #f
                                              (assq-ref assignee-user
                                                        'username))))))))

;; The epic panel: one link per body of work, so that a backlog can be read a
;; group at a time.  It is a row of links rather than a menu because reading a
;; backlog by epic is something a reader does over and over, and a menu would
;; cost two gestures every time.  The link of the epic being read wears
;; `aria-current', which is both what a screen reader says and what the sheet
;; draws as chosen.
;; The label panel: one link per word the project's tickets are tagged with,
;; so that a backlog can be read by tag.  It is the epic panel's twin on
;; purpose -- two ways of narrowing a backlog that are used the same way are
;; drawn the same way, or a reader has to learn the page twice.
;;
;; The labels are collected from the tickets the page was handed rather than
;; queried: a label only exists because a ticket carries it, so the list is
;; the tickets themselves read one field further.
(define (label-filter-row G_ project-human-id tickets selected)
  (let ((labels (sort (delete-duplicates (append-map ticket-labels tickets)
                                         string=?)
                      string<?)))
    (if (null? labels)
        '()
        `(div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                            gap-1 py-1))))
              (a (@ (class ,(mk-class '(text-xs no-underline rounded-lg
                                                 py-1 px-2 lp-quiet)))
                    (href ,(format #f "/tickets/~a" project-human-id)))
                 ,(G_ "every label"))
              ,@(map (lambda (label)
                       `(a (@ (class ,(mk-class '(no-underline flex
                                                               items-center)))
                             (href ,(format #f "/tickets/~a?label=~a"
                                            project-human-id
                                            (uri-encode label)))
                             ,@(if (and selected
                                        (equal? (string-downcase label)
                                                (string-downcase selected)))
                                   '((aria-current "page"))
                                   '()))
                          ,(label-chip label)))
                    labels)))))

;; A field a reader types one word into, drawn quiet.
;;
;; It is deliberately not hygguile's `input'.  That one is the field of a form
;; -- a label on a line of its own above a plate four times the height of the
;; words around it -- which is right when filling the field in is what the page
;; is for, and wrong here: this field sits in a row of links and menus that say
;; how to read the list, and a plate among them turns one row of small
;; decisions into a column of three.  So the field keeps the small frame the
;; menu beside it wears, says its name in its placeholder and in `aria-label'
;; for a reader who cannot see the placeholder, and takes no height of its own.
(define* (quiet-field label #:key name value (required? #f))
  `(input (@ (type "text")
             (name ,name)
             (value ,value)
             ,@(if required? '((required "required")) '())
             (placeholder ,label)
             (aria-label ,label)
             (class ,(mk-class '(text-sm rounded-lg px-2 py-1 w-40
                                 bg-transparent border-2
                                 border-stone-500/25
                                 dark:border-onyx-100/25
                                 text-onyx-700 dark:text-stone-300))))))

;; The saved filters: narrowings of this backlog that somebody named.
;;
;; A saved filter is a name and the query string it stands for (see
;; `(lucidplan domain saved-filter)'), so a link to one is a link to the
;; backlog with that narrowing already applied.  The field beside them saves
;; the narrowing being read -- the page hands over the query string it was
;; asked with -- so keeping one costs a name and a click rather than an
;; excursion into a settings page.
(define* (saved-filter-panel G_ project-human-id filters
                             #:key (current-query "") (can-edit? #f))

  ;; One saved view: its name, which is a link to the narrowing it stands for,
  ;; and -- for a reader who may triage -- the way to forget it.
  ;;
  ;; The two are one `div' because the second is a `form', and a `form' is not
  ;; phrasing content: a browser that meets one inside a `span' closes the
  ;; `span' first and reparents whatever came after it, which is how the delete
  ;; control ended up outside the form it was meant to submit and the form
  ;; itself ended up empty.
  ;;
  ;; The cross is quiet until it is wanted.  A row of saved views should read
  ;; as the names it is made of, and the same mark after every name is noise
  ;; rather than an offer.
  (define (saved-view filter)
    (let ((name (assq-ref filter
                          'name))
          (query (or (assq-ref filter
                               'query) ""))
          (id (assq-ref filter
                        'id)))
      (append
       (list `(a (@ (class ,(mk-class '(text-xs no-underline rounded-lg py-1
                                               px-2 lp-quiet)))
                     (href ,(format #f
                                    "/tickets/~a~a"
                                    project-human-id
                                    (if (string-null? query) ""
                                        (format #f "?~a" query)))))
                  ,name))
       (if (not can-edit?) '()
           (list `(form (@ (method "POST")
                           (action ,(format #f
                                            "/tickets/~a/filters/delete"
                                            project-human-id))
                           (class ,(mk-class '(inline flex items-center))))
                        (input (@ (type "hidden") (name "id") (value ,id)))
                        (button (@ (class ,(mk-class '(lp-quiet cursor-pointer
                                                               text-xs px-1)))
                                   (type "submit")
                                   (aria-label ,(format #f
                                                       "~a ~a"
                                                       (G_ "remove")
                                                       name)))
                                ,(icon #:type 'x
                                       #:icon-css '(text-xs)))))))))

  `(div (@ (class ,(mk-class '(flex flex-col gap-2 py-1))))
        ,@(if (pair? filters)
              (list `(div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                        items-center gap-1))))
                          ,@(map (lambda (filter)
                                   `(div (@ (class ,(mk-class
                                                     '(flex flex-row
                                                            items-center))))
                                         ,@(saved-view filter)))
                                 filters)))
              '())
        ;; The field that keeps the narrowing being read, and the word that
        ;; keeps it.  It is unquoted because `quiet-field' (like every
        ;; hygguile component) is a procedure rather than a tag: left unquoted
        ;; its whole call is printed as text, and the page says the words of
        ;; the call instead of offering a field.
        ,@(if can-edit?
              (list `(form (@ (method "POST")
                              (action ,(format #f
                                               "/tickets/~a/filters/new"
                                               project-human-id))
                              (class ,(mk-class
                                       '(flex flex-row flex-wrap
                                              items-center gap-2))))
                           (input (@ (type "hidden")
                                     (name "query")
                                     (value ,current-query)))
                           ,(quiet-field (G_ "name this view")
                                         #:name "name"
                                         #:value ""
                                         #:required? #t)
                           (button (@ (class ,(mk-class
                                               '(js-only text-xs
                                                         font-semibold
                                                         underline
                                                         cursor-pointer
                                                         lp-quiet px-2 py-1
                                                         text-onyx-700
                                                         dark:text-stone-300)))
                                      (type "submit"))
                                   ,(G_ "save this view"))))
              '())))

;; The bulk bar: what a reader can do to the rows they have ticked.
;;
;; It is one form for the whole page rather than a form around the list,
;; because a row already holds forms of its own -- the delete control inside the
;; card it opens into -- and a form inside a form is not a document any browser
;; will parse.  The ticked rows are therefore collected when the form is
;; submitted, by the script this page already ships
;; (`lucidplanBulkSubmit' in `pub/js/tickets.js').
;;
;; What is being set is named twice: once by the menu that says what kind of
;; change this is, and once by the word in front of the value chosen.  The
;; second is what the endpoint trusts, because a menu cannot be a promise and
;; a value that arrived without its kind is a value whose meaning is anybody's
;; guess; the first is there so that the list of values can be read in groups
;; -- an epic, a status and a person are different kinds of thing and a flat
;; list of all three would be three lists in a trench coat.
;;
;; The menu opens on a choice that chooses nothing.  Every other value in it
;; writes something over every ticked row, and two of them -- no epic, nobody
;; -- write the *absence* of something, which is exactly the kind of change a
;; reader makes by accident: tick a row, glance at the field beside the menu,
;; press apply, and the epics they never mentioned are gone.  So the absence is
;; spelled out as a change like any other, and the first thing the menu offers
;; is to change nothing.
;;
;; The label is the one change that is not in the menu, because it is free
;; text: a label is a word nobody had to anticipate, so it is typed rather than
;; chosen, and it is typed in its own field.  That field is not a second kind of
;; change -- it is the label of a change of kind, so it is added whatever the
;; menu was left at, and left alone when it is empty; which is what makes
;; tagging a handful of rows possible without restating their epic or their
;; status as a side effect.
(define* (bulk-bar G_ project-human-id #:key statuses epics assignees
                   (can-edit? #f))
  (if (not can-edit?)
      '()
      `(form (@ (class ,(mk-class '(js-only flex flex-row flex-wrap
                                              items-center gap-2 py-2)))
                (method "POST")
                (action ,(format #f "/tickets/~a/bulk" project-human-id))
                (onsubmit "return lucidplanBulkSubmit(this)"))
             (input (@ (type "hidden") (name "ids") (value "")))
             (label (@ (class ,(mk-class '(flex flex-row items-center gap-2
                                                    text-xs text-onyx-700
                                                    dark:text-stone-300))))
                    (input (@ (type "checkbox")
                              (class "h-4 w-4 cursor-pointer")
                              (onclick "lucidplanBulkSelectAll(this)")))
                    ,(G_ "select all"))
             (select (@ (name "value")
                        (aria-label ,(G_ "change"))
                        (class ,(mk-class '(text-sm rounded-lg px-2 py-1
                                                   max-w-full
                                                   bg-transparent border-2
                                                   border-stone-500/25
                                                   dark:border-onyx-100/25
                                                   text-onyx-700
                                                   dark:text-stone-300))))
                     ;; The choice that chooses nothing, and the one the menu
                     ;; opens on, so that pressing apply with only a label
                     ;; typed changes the labels and nothing else.
                     ,(option #:value "" #:content (G_ "change nothing"))
                     ,(option #:value "epic:"
                              #:content (format #f
                                                "~a: ~a"
                                                (G_ "clear")
                                                (G_ "epic")))
                     ,@(map (lambda (epic)
                              (option #:value (format #f "epic:~a"
                                                       (assq-ref epic
                                                                 'human-id))
                                      #:content (format #f "~a: ~a"
                                                         (G_ "epic")
                                                         (assq-ref epic
                                                                   'title))))
                            epics)
                     ,@(map (lambda (status)
                              (option #:value (format #f "status:~a" status)
                                      #:content (format #f "~a: ~a"
                                                         (G_ "status")
                                                         (status-label G_
                                                                       statuses
                                                                       status))))
                            (ticket-status-values statuses))
                     ,(option #:value "assignee:"
                              #:content (format #f
                                                "~a: ~a"
                                                (G_ "clear")
                                                (G_ "assignee")))
                     ,@(map (lambda (entry)
                              (option #:value (format #f "assignee:~a"
                                                       (car entry))
                                      #:content (format #f "~a: ~a"
                                                         (G_ "assignee")
                                                         (cdr entry))))
                            assignees))
             ;; As in the saved-view panel, a component is a call and not a
             ;; tag: the field is unquoted so that it runs, and it is the quiet
             ;; field so that it stands on the same line as the menu it is
             ;; applied with rather than opening a column of its own.
             ,(quiet-field (G_ "add a label") #:name "label" #:value "")
             (button (@ (class ,(mk-class '(cursor-pointer text-sm
                                                           font-semibold
                                                           underline
                                                           text-onyx-700
                                                           dark:text-stone-300)))
                        (type "submit"))
                     ,(G_ "apply")))))

(define (epic-filter-row G_ project-human-id epics selected)
  (if (null? epics)
      '()
      `(div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-1 py-1))))
            (a (@ (class ,(mk-class '(text-xs no-underline rounded-lg
                                               py-1 px-2 lp-quiet)))
                  (href ,(format #f "/tickets/~a" project-human-id)))
               ,(G_ "every epic"))
            ,@(map (lambda (epic)
                     (let* ((slug (assq-ref epic
                                             'human-id))
                            (current? (and selected
                                           (equal? slug
                                                   (assq-ref selected
                                                             'human-id))))
                            (attributes (if current?
                                            '((aria-current "page"))
                                            '())))
                       `(a (@ (class ,(mk-class '(no-underline flex
                                                               items-center)))
                              (href ,(format #f "/tickets/~a?epic=~a"
                                             project-human-id slug))
                              ,@attributes)
                           ,(epic-chip epic))))
                  epics))))

;; The two controls that open and close every card at once.
;;
;; They carry an attribute each rather than a line of script, because the fold
;; is not a `hidden' attribute any more: a card is opened by a class the shared
;; script toggles (see `pub/js/lucidplan-luxe.js'), so the controls have to ask
;; that script to do it rather than reach into the document themselves.  That is
;; what these attributes are for, and why the projects page's controls have
;; always worked while these did not.
(define (expand-and-tips G_)
  `(div (@ (class ,(mk-class '(flex flex-row
                                    flex-wrap
                                    justify-between
                                    items-center
                                    my-1
                                    gap-4))))
        (p (@ (class ,(mk-class '(text-sm text-onyx-500 dark:text-stone-500))))
           ,(G_
             "Tip: Use Tab to navigate tickets and Enter/Space to expand or collapse cards."))

        (div (@ (class ,(mk-class '(flex flex-row gap-4))))
             (button (@ (class ,(mk-class '(js-only text-sm font-semibold
                                                    underline
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300
                                                    hover:text-onyx-900
                                                    dark:hover:text-stone-100)))
                        (type "button")
                        (data-lp-expand-all ""))
                     ,(G_ "Expand All"))
             (button (@ (class ,(mk-class '(js-only text-sm font-semibold
                                                    underline
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300
                                                    hover:text-onyx-900
                                                    dark:hover:text-stone-100)))
                        (type "button")
                        (data-lp-collapse-all ""))
                     ,(G_ "Collapse All")))))

(define* (html-from-tickets G_
                            #:key is-logged-in
                            (can-triage? #f)
                            (current-sort #f)
                            projects
                            project-details
                            tickets
                            ticket-id-to-human-id
                            user-names
                            ticket-assignee
                            with-done
                            with-rejected
                            (statuses (default-ticket-statuses))
                            ;; The epics of this project, and the one the reader
                            ;; has asked to see the backlog of -- the slug from
                            ;; the query string, or #f for all of them.
                            (epics '())
                            (epic-filter #f)
                            (label-filter #f)
                            ;; The named narrowings of this backlog, and the
                            ;; query string this page was asked with -- which
                            ;; is what the field beside them saves.
                            (saved-filters '())
                            (current-query "")
                            search-query)
  (let* ( ;Core Data
          ;; The epic the reader asked to read the backlog by, if any, and the
          ;; tickets of it.  The filter is a link, so this page has to do it:
          ;; it is the same list, narrowed, which is why the counts beside the
          ;; buckets and the rows themselves can never disagree.
          (epic-selected (and epic-filter
                              (find (lambda (e)
                                      (equal? (assq-ref e
                                                        'human-id)
                                              epic-filter))
                                    epics)))
          ;; The backlog exactly as the repository handed it over, before either
          ;; narrowing.  The label panel is built from this list and not from
          ;; the narrowed one, because the panel is how a reader leaves the
          ;; narrowing they are in: a panel that only lists the label already
          ;; chosen is a panel with nothing left to say.
          (read-tickets tickets)
          (tickets (if (and (string? label-filter)
                            (not (string-null? label-filter)))
                       (filter (lambda (t)
                                 (ticket-has-label? t label-filter))
                               tickets)
                       tickets))
          (tickets (if epic-selected
                       (filter (lambda (t)
                                 (equal? (format #f "~a"
                                                 (or (assq-ref t
                                                               'epic-id) ""))
                                         (format #f "~a"
                                                 (assq-ref epic-selected
                                                           'id))))
                               tickets)
                       tickets))
          (proj-human-id (assq-ref project-details
                                   'human-id))
         (proj-title (assq-ref project-details
                               'title))
         (proj-desc (assq-ref project-details
                              'description))

         ;; CSS Faces
         (header-face (mk-class '(flex flex-row gap-2 items-center)))
         (btn-row-face (mk-class '(flex flex-row flex-wrap gap-1 items-center)))
         (label-face (mk-class '(text-onyx-100 dark:text-stone-300 text-sm)))
         (icon-face '(text-onyx-700 dark:text-stone-300 text-2xl))

         ;; JS Handlers
         (done-js (string-append "window.location.href = `/tickets/"
                   proj-human-id
                   "?with-done=${document.getElementById('should-show-done').checked}`;"))
         (rejected-js (string-append "window.location.href = `/tickets/"
                       proj-human-id
                       "?with-rejected=${document.getElementById('should-show-rejected').checked}`;"))

         ;; Search Component
         (search-param-components `((div (@ (class ,(mk-class '(flex flex-row
                                                                flex-wrap
                                                                gap-1 sm:gap-2
                                                                justify-between))))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-start
                                                                     items-center))))
                                              ,(sort-select G_
                                                            #:project-id
                                                            proj-human-id
                                                            #:current-sort
                                                            current-sort))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-end
                                                                     items-center))))
                                              ,(search-box G_
                                                           #:search-query
                                                           search-query)))))

         ;; A reader who may change a ticket triages one from the row it is
         ;; listed in, exactly as they do from the ticket's own page: the
         ;; control is offered to the same reader the endpoint would accept
         ;; -- one who holds the ticket-create permission -- and it posts to
         ;; that same endpoint.
         (triage? can-triage?)

         ;; A row's grip writes the ticket's priority, and the backlog can only
         ;; be shown in the order those priorities describe while it is sorted
         ;; by them.  Under any other sort a row the reader moved would slide
         ;; back into place on the next visit, so the grip is offered under
         ;; exactly the sorts a drop can hold -- the two priority ones -- to
         ;; the same reader the endpoint behind them accepts.
         (reorderable? (and can-triage?
                            (or (equal? "priority-desc" current-sort)
                                (equal? "priority-asc" current-sort))))

         ;; List Generators
         ;; Keep the list as it arrived before it is rebound: the binding below
         ;; narrows `tickets' to the still-open ones, so the done and rejected
         ;; partitions must be taken from the untouched list.
         (all-tickets tickets)
         ;; Which bucket a ticket belongs to is its own status's business, as
         ;; the page's ladder describes it; a status the ladder does not name
         ;; counts as open, so a ticket carrying one stays reachable.
         (all-in-bucket (lambda (bucket)
                          (filter (lambda (t)
                                    (ticket-status-in-bucket? statuses
                                                              (assq-ref t
                                                                        'ticket-status)
                                                              bucket))
                                  all-tickets)))
         (tickets (all-in-bucket "open"))
         (done-tickets (all-in-bucket "done"))
         (rejected-tickets (all-in-bucket "rejected"))
         (ticket-list `((div (@ (id lucidplan-tickets-list)
                                (data-project-id ,proj-human-id)
                                ;; The number a drop writes is the one that
                                ;; puts the row back where it now sits under
                                ;; the order being shown, so the list says
                                ;; which order that is.
                                (data-sort ,(if current-sort current-sort ""))
                                ;; The list answers the drag on the way in as
                                ;; well as on the way across it: a drop target
                                ;; is the element whose last `dragenter' or
                                ;; `dragover' was answered, and Chromium reads
                                ;; the release against that one element.
                                (ondragenter "acceptDrag(event)")
                                (ondragover "allowDrop(event)")
                                (ondrop "dropTicketsItem(event)")
                                (class ,(mk-class '(flex flex-col gap-2))))
                             ,(map (lambda (t)
                                     (let* ((tt-assignee (find (lambda (a)
                                                                 (equal? (assq-ref
                                                                          t
                                                                          'id)
                                                                         (assq-ref
                                                                          a
                                                                          'ticket-id)))
                                                          ticket-assignee))
                                            (ttt-assignee (if (nil?
                                                               tt-assignee) #f
                                                              (assq-ref
                                                               tt-assignee
                                                               'user-id))))
                                       (render-ticket-listitem G_
                                        #:project proj-human-id
                                        #:ticket t
                                        #:ticket-id-to-human-id
                                        ticket-id-to-human-id
                                        #:ticket-assignee ttt-assignee
                                        #:user-names user-names
                                        #:statuses statuses
                                        #:epics epics
                                        #:editable? triage?
                                        #:reorderable? reorderable?))) tickets))))

         (done-ticket-list (if with-done
                               `(div (@ (id lucidplan-done-tickets-list)
                                        (class ,(mk-class '(my-4))))
                                     (hr)
                                     (h2 (@ (class ,(mk-class '(text-lg
                                                                font-sans
                                                                font-black
                                                                text-onyx-700
                                                                dark:text-stone-300
                                                                flex
                                                                flex-row
                                                                gap-2
                                                                my-4))))
                                         ,(bucket-icon-node statuses "done")
                                         (span ,(bucket-heading G_ statuses
                                                                "done")))
                                     (div (@ (class ,(mk-class '(flex flex-col
                                                                 gap-2))))
                                          ,(map (lambda (t)
                                                  (render-ticket-listitem G_
                                                   #:project proj-human-id
                                                   #:ticket t
                                                   #:ticket-id-to-human-id
                                                   ticket-id-to-human-id
                                                   #:ticket-assignee
                                                   ticket-assignee
                                                   #:user-names user-names
                                                   #:statuses statuses
                                                   #:epics epics
                                                   #:editable? triage?))
                                                done-tickets)))
                               '()))

         (rejected-ticket-list (if with-rejected
                                   `(div (@ (id
                                             lucidplan-rejected-tickets-list)
                                            (class ,(mk-class '(my-4))))
                                         (hr)
                                         (h2 (@ (class ,(mk-class '(text-lg
                                                                    font-sans
                                                                    font-black
                                                                    text-onyx-700
                                                                    dark:text-stone-300
                                                                    flex
                                                                    flex-row
                                                                    gap-2
                                                                    my-4))))
                                             ,(bucket-icon-node statuses
                                                                "rejected")
                                             (span ,(bucket-heading G_
                                                     statuses "rejected")))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-col
                                                                     gap-2))))
                                              ,(map (lambda (t)
                                                      (render-ticket-listitem
                                                       G_
                                                       #:project proj-human-id
                                                       #:ticket t
                                                       #:ticket-id-to-human-id
                                                       ticket-id-to-human-id
                                                       #:ticket-assignee
                                                       ticket-assignee
                                                       #:user-names user-names
                                                       #:statuses statuses
                                                       #:editable? triage?))
                                                    rejected-tickets)))
                                   '()))

         (fallback-no-ticket-list `(,(p (G_ "Wow.. it's really empty here"))))

         ;; A one-glance summary of the project: how much is still open, how
         ;; much is done, and a completion bar -- the friendliest way to see
         ;; where a project stands without reading every row.
         (glance (let* ((open-count (length tickets))
                        (done-count (length done-tickets))
                        (rejected-count (length rejected-tickets))
                        (total (+ open-count done-count rejected-count)))
                   (glass-panel `((div (@ (class ,(mk-class '(flex flex-row
                                                              flex-wrap gap-8
                                                              items-start my-1))))
                                       ,(stat-card #:value (format #f "~a"
                                                                   open-count)
                                                   #:label (G_ "open"))
                                       ,(stat-card #:value (format #f "~a"
                                                                   done-count)
                                                   #:label (G_ "done"))
                                       ,(stat-card #:value (format #f "~a"
                                                            rejected-count)
                                                   #:label (G_ "rejected"))
                                       ,(stat-card #:value (format #f "~a"
                                                                   total)
                                                   #:label (G_ "total")))
                                  (div (@ (class ,(mk-class '(my-1))))
                                       ,(progress #:value done-count
                                                  #:max (max total 1)
                                                  #:label (G_ "completion")
                                                  #:show-value #t)))
                                #:frost 3
                                #:padding '(p-5 sm:p-6)
                                #:classes '(flex flex-col gap-2)))))

    ;; Build the final HTML View
    (html-template G_
                   #:title (format #f "backlog of ~a" proj-title)
                   #:canonical-path (format #f "/tickets/~a" proj-human-id)
                   #:is-logged-in is-logged-in
                   #:proj projects
                   #:body `(div (@ (class ,(mk-class '(px-2 sm:px-4
                                                            md:px-6
                                                            pt-1
                                                            sm:pt-2
                                                            pb-3
                                                            sm:pb-5))))
                                ((div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-2
                                                                  sm:gap-4
                                                                  grow))))

                                      ;; Header Section
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-wrap
                                                                  flex-row
                                                                  gap-2
                                                                  sm:gap-4
                                                                  items-center
                                                                  justify-between))))
                                           (div (@ (class ,header-face))
                                                ,(icon #:type 'scroll
                                                       #:icon-css icon-face)
                                                ,(h2 (format #f
                                                             "backlog of ~a"
                                                             proj-title)))

                                           (div (@ (class ,btn-row-face))
                                                (div (a (@ (href ,(format #f
                                                                   "/kanban/~a"
                                                                   proj-human-id)))
                                                        ,(icon-button #:icon 'kanban
                                                                      #:content
                                                                      (G_
                                                                       "kanban")
                                                                      #:icon-css '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300))))
                                                ;; The plan the backlog feeds:
                                                ;; the sprints the work is
                                                ;; committed to, and the
                                                ;; milestones it counts
                                                ;; toward.  Both are pages of
                                                ;; the project, so they sit
                                                ;; beside its other two.
                                                (div (a (@ (href ,(format #f
                                                                   "/projects/~a/sprints"
                                                                   proj-human-id)))
                                                        ,(icon-button #:icon 'calendar-check
                                                                      #:content
                                                                      (G_
                                                                       "sprints")
                                                                      #:icon-css '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300))))
                                                (div (a (@ (href ,(format #f
                                                                   "/projects/~a/milestones"
                                                                   proj-human-id)))
                                                        ,(icon-button #:icon 'target
                                                                      #:content
                                                                      (G_
                                                                       "milestones")
                                                                      #:icon-css '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300))))
                                                (a (@ (href ,(format #f
                                                              "/tickets/~a/new"
                                                              proj-human-id)))
                                                   ,(icon-button #:icon 'plus
                                                                 #:content (G_
                                                                            "new ticket")
                                                                 #:icon-css '(text-md
                                                                              text-onyx-700
                                                                              dark:text-stone-300)))))

                                      ;; Description
                                      ,(render-if-nestring (lambda (x)
                                                             `(em (@ (class ,(mk-class '
                                                                              (text-sm
                                                                               text-onyx-700
                                                                               font-sans
                                                                               dark:text-stone-300
                                                                               wrap-break-word
                                                                               line-clamp-3
                                                                               sm:line-clamp-none
                                                                               max-w-full))))
                                                                  ,x))
                                                           proj-desc)

                                      ;; Project at a glance
                                      ,glance

                                      ;; Options (Checkboxes)
                                      (div (details (summary (@ (class ,(mk-class '
                                                                         (text-onyx-100
                                                                          dark:text-stone-300
                                                                          text-sm
                                                                          font-sans
                                                                          cursor-pointer
                                                                          py-2))))
                                                             ,(G_
                                                               "backlog options"))
                                                    (div (@ (class ,(mk-class '
                                                                              (flex
                                                                               flex-wrap
                                                                               flex-row
                                                                               gap-2
                                                                               sm:gap-4
                                                                               my-3))))

                                                         ;; Done Checkbox
                                                         (input (@ (id
                                                                    "should-show-done")
                                                                   (type
                                                                    checkbox)
                                                                   ,(if
                                                                     with-done
                                                                     '(checked
                                                                       true)
                                                                     '(not-checked
                                                                       true))
                                                                   (onchange ,done-js)))
                                                         (span (@ (class ,label-face))
                                                               ,(G_
                                                                 "show done?"))

                                                         ;; Rejected Checkbox
                                                         (input (@ (id
                                                                    "should-show-rejected")
                                                                   (type
                                                                    checkbox)
                                                                   ,(if
                                                                     with-rejected
                                                                     '(checked
                                                                       true)
                                                                     '(not-checked
                                                                       true))
                                                                   (onchange ,rejected-js)))
                                                         (span (@ (class ,label-face))
                                                               ,(G_
                                                                 "show rejected?")))))

                                      ;; Sub-Components & Lists
                                      ,search-param-components
                                      ,(bulk-bar G_ proj-human-id
                                                 #:statuses statuses
                                                 #:epics epics
                                                 #:assignees (map (lambda (u)
                                                                   (cons (assq-ref
                                                                          u
                                                                          'id)
                                                                         (assq-ref
                                                                          u
                                                                          'username)))
                                                                 user-names)
                                                 #:can-edit? can-triage?)
                                      ,(saved-filter-panel G_ proj-human-id
                                                           saved-filters
                                                           #:current-query
                                                           current-query
                                                           #:can-edit?
                                                           can-triage?)
                                      ,(epic-filter-row G_ proj-human-id
                                                        epics epic-selected)
                                      ,(label-filter-row G_ proj-human-id
                                                         read-tickets
                                                         label-filter)
                                      ,(expand-and-tips G_)
                                      ,(if (nil? tickets)
                                           fallback-no-ticket-list ticket-list)
                                      ,done-ticket-list
                                      ,rejected-ticket-list))
                                ;; The card a change made from a row is
                                ;; confirmed with, once for the page: the
                                ;; rows share it, and it is inert until the
                                ;; script has something to say.  See
                                ;; `render-quiet-notice'.
                                ,@(render-quiet-notice G_)))))

(define* (html-new-ticket-form G_
                               #:key projects
                               project
                               users
                               is-logged-in
                               (statuses (default-ticket-statuses)))
  (html-ticket-form G_
                    #:is-logged-in is-logged-in
                    #:project project
                    #:projects projects
                    #:statuses statuses
                    #:users users
                    #:page-title (G_ "new ticket")
                    #:canonical-path (format #f "/tickets/~a/new" project)
                    #:action (format #f "/tickets/~a/new" project)))

(define* (html-edit-ticket-form G_
                                #:key projects
                                project
                                t
                                assignee
                                users
                                is-logged-in
                                (statuses (default-ticket-statuses)))
  (html-ticket-form G_
                    #:is-logged-in is-logged-in
                    #:project project
                    #:is-edit #t
                    #:statuses statuses
                    #:title (assq-ref t
                                      'title)
                    #:description (assq-ref t
                                            'description)
                    #:labels (labels-text (or (assq-ref t
                                                       'labels)
                                             ""))
                    #:ticket-type (assq-ref t
                                            'ticket-type)
                    #:ticket-status (assq-ref t
                                              'ticket-status)
                    #:assignee assignee
                    #:users users
                    #:priority (assq-ref t
                                         'priority)
                    #:estimate-points (assq-ref t
                                                'estimate-points)
                    #:estimate-size (assq-ref t
                                              'estimate-size)
                    #:page-title (G_ "edit ticket")
                    #:canonical-path (format #f "/tickets/~a" project)
                    #:action (format #f "/tickets/~a/id/~a/edit" project
                                     (assq-ref t
                                               'human-id))
                    #:projects projects))

(define* (html-ticket-form G_
                           #:key (project #f)
                           (title "")
                           (description "")
                           ;; The words the ticket is tagged with, as the
                           ;; comma-separated line the form edits.
                           (labels "")
                           (ticket-status "")
                           (statuses (default-ticket-statuses))
                           (ticket-type "")
                           (priority 0)
                           (estimate-points #f)
                           (estimate-size #f)
                           (assignee "")
                           (action "")
                           (is-edit #f)
                           (projects '())
                           (users '())
                           (page-title "")
                           (canonical-path "/tickets")
                           (is-logged-in #f))

  (let* ( ;Consolidate Reusable CSS Classes
          (form-face (mk-class '(flex flex-col gap-6)))
         (field-face (mk-class '(max-w-128)))
         (label-face (mk-class '(text-onyx-700 dark:text-stone-300)))

         ;; Format Data Safely
         (safe-title (html-unescape title))
         (safe-desc (html-unescape description))
         (safe-prio (or priority 0))

         ;; The two ladders every triage control in the app shares
         (status-opts (ticket-status-choices G_ statuses ticket-status))

         (type-opts (ticket-type-choices G_ ticket-type))

         ;; The estimates are menus for the same reason the ladders are:
         ;; a story-point estimate not on the team's scale, or a size nobody
         ;; uses, is a value nothing else in the app knows how to count.
         (points-value (if estimate-points
                           (format #f "~a" estimate-points) ""))
         (point-opts (ticket-point-choices G_ points-value))
         (size-opts (ticket-size-choices G_
                                         (or estimate-size "")))

         ;; The assignee is chosen from the accounts that exist rather than
         ;; typed, so a ticket cannot name somebody the instance has never
         ;; heard of.  The value submitted is the user's id -- which is what
         ;; the assignment row records -- and the row shown is their name.
         ;; Nobody is the first row, and it is the one an unassigned ticket
         ;; opens on.
         (assignee-opts (cons (option #:value ""
                                      #:content (G_ "unassigned")
                                      #:selected-value assignee)
                              (map (lambda (u)
                                     (option #:value (assq-ref u
                                                               'id)
                                             #:content (assq-ref u
                                                                 'username)
                                             #:selected-value assignee)) users)))

         ;; Build the Form SXML
         (form-sxml `(form (@ (enctype "application/x-www-form-urlencoded")
                              (method POST)
                              (action ,action))

                           (div (@ (class ,form-face))

                                ;; Title
                                ,(input #:label (G_ "title")
                                        #:name "title"
                                        #:value safe-title
                                        #:required #t)

                                ;; Status
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "status"))
                                     (div ,(select #:name 'ticket-status
                                                   #:on-change ""
                                                   #:options status-opts)))

                                ;; Type
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "type"))
                                     (div ,(select #:name 'ticket-type
                                                   #:on-change ""
                                                   #:options type-opts)))

                                ;; Priority
                                (div (@ (class ,field-face))
                                     ,(input #:label (G_ "priority")
                                             #:name "priority"
                                             #:required #f
                                             #:type 'number
                                             #:value safe-prio))

                                ;; Story points
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "story points"))
                                     (div ,(select #:name 'estimate-points
                                                   #:on-change ""
                                                   #:options point-opts)))

                                ;; T-shirt size
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "size"))
                                     (div ,(select #:name 'estimate-size
                                                   #:on-change ""
                                                   #:options size-opts)))

                                ;; Assignee
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "assignee"))
                                     (div ,(select #:name 'assignee
                                                   #:on-change ""
                                                   #:options assignee-opts)))

                                ;; Labels: the words this project's own
                                ;; vocabulary is made of, as one
                                ;; comma-separated line.  A list that can only
                                ;; be chosen from is a list somebody else
                                ;; wrote, and the point of a label is that
                                ;; nobody had to anticipate it.
                                ,(input #:label (G_ "labels")
                                        #:name "labels"
                                        #:required #f
                                        #:value (labels-text labels))
                                (p (@ (class ,(mk-class '(text-sm
                                                          text-onyx-500
                                                          dark:text-stone-500))))
                                   ,(G_
                                     "Words separated by commas: bug, needs-design, blocked-on-ops."))

                                ;; Description
                                ,(textarea #:label (G_ "description")
                                           #:name "description"
                                           #:required #f
                                           #:value safe-desc
                                           #:textarea-css '(leading-7 p-4
                                                            rounded-xl
                                                            border-stone-700
                                                            dark:border-onyx-100
                                                            dark:text-stone-300
                                                            dark:bg-onyx-700/75
                                                            bg-stone-300/75
                                                            border-2
                                                            min-h-72
                                                            min-w-90
                                                            shadow-md
                                                            min-w-prose))

                                ;; Submit Button
                                (div ,(submit-button (G_ "submit")))))))

    ;; Render final template
    (html-template G_
                   #:title (if (string-null? page-title) #f page-title)
                   #:canonical-path canonical-path
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4 py-4))))
                                 ,form-sxml))
                   #:proj projects)))

;; =============================================================================
;;  What a ticket carries with it
;; =============================================================================

;; The ticket's own page used to end with the card.  This is everything that
;; grew around it: the discussion, the files, what happened to it, the
;; versions of what it said, and the way to hand it to another project.  They
;; are drawn as one list of sections so the controller only has to say which
;; ticket it is showing; which of them are offered is decided here, from the
;; same `editable?' answer the endpoints behind the controls are guarded with.

;; A stamp in a ticket's own record is the second it happened, which is what
;; the schema keeps and what the trail is replayed from -- and which no reader
;; can read.  It is said here as the date and the time of day, in the order
;; every other date of the application is written in -- the deadline a ticket
;; carries, the day an hour was spent, the days a burndown plots -- so a page
;; of stamps reads as one page rather than as one for each writer.  A row that
;; somehow carries no stamp says nothing rather than a stray number.
(define (recorded-at value)
  (let ((seconds (cond
                   ((number? value) value)
                   ((and (string? value)
                         (string->number value)) (string->number value))
                   (else #f))))
    (if seconds
        (strftime "%Y-%m-%d %H:%M" (localtime seconds))
        (if value (format #f "~a" value) ""))))

(define (short-value value)
  "Answer VALUE as one line short enough for a history row."
  (let ((text (if (string? value) value
                  (format #f "~a" value))))
    (if (> (string-length text) 160)
        (string-append (substring text 0 157) "...") text)))

(define (value-or-dash value)
  (if (or (not value)
          (and (string? value)
               (string-null? value))) "—"
      (short-value value)))

(define (event-action-label G_ action)
  (cond
    ((equal? action "created")
     (G_ "created"))
    ((equal? action "changed")
     (G_ "changed"))
    ((equal? action "moved")
     (G_ "moved"))
    ((equal? action "restored")
     (G_ "restored"))
    ((equal? action "commented")
     (G_ "commented"))
    ((equal? action "comment-removed")
     (G_ "removed a comment"))
    ((equal? action "attachment-added")
     (G_ "attached"))
    ((equal? action "attachment-removed")
     (G_ "removed"))
    ((equal? action "time-logged")
     (G_ "logged time"))
    ((equal? action "time-removed")
     (G_ "removed time"))
    ((equal? action "sprint-added")
     (G_ "took into a sprint"))
    ((equal? action "sprint-removed")
     (G_ "took out of a sprint"))
    ((equal? action "deleted")
     (G_ "deleted"))
    (else action)))

;; A change of a sprint or a milestone is recorded with the id of the thing
;; that moved, because that is what the burndown replays and an id survives a
;; rename.  A reader, on the other hand, knows a sprint by its name, so the
;; page is handed the plan it already has -- an alist of id to title -- and
;; says the name where it can.
(define (plan-name plan id)
  (let ((row (and plan id
                  (assoc (format #f "~a" id) plan))))
    (if row (cdr row) id)))

(define* (render-event G_ event users #:key (plan '()))
  (let* ((actor (assq-ref event
                          'actor-id))
         (actor-name (let ((u (and actor
                                   (find (lambda (row)
                                           (equal? (assq-ref row
                                                             'id) actor))
                                         users))))
                       (if u
                           (assq-ref u
                                     'username) "")))
         (action (assq-ref event
                           'action))
         (field (assq-ref event
                          'field))
         (named? (and field
                      (member (string-downcase (format #f "~a" field))
                              '("sprint" "milestone"))))
         (old (let ((v (assq-ref event
                                 'old-value)))
                (if (and v named?) (plan-name plan v) v)))
         (new (let ((v (assq-ref event
                                 'new-value)))
                (if (and v named?) (plan-name plan v) v))))
    `(div (@ (class ,(mk-class '(flex flex-col
                                      gap-1
                                      border-2
                                      border-stone-300/50
                                      dark:border-onyx-100/50
                                      rounded-lg
                                      px-3
                                      py-2))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                           items-baseline))))
               (strong ,(if (string-null? actor-name)
                            (G_ "somebody") actor-name))
               (span (@ (class ,(mk-class '(text-sm font-medium))))
                     ,(event-action-label G_ action))
               ,@(if field
                     `((span (@ (class ,(mk-class '(text-sm))))
                             ,(G_ field)))
                     '()))
          ,@(if (and field
                     (or old new))
                `((div (@ (class ,(mk-class '(text-sm font-mono
                                                      wrap-break-word))))
                       (span ,(value-or-dash old)) " → "
                       (span ,(value-or-dash new))))
                '())
          (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                                dark:text-stone-500))))
                 ,(recorded-at (assq-ref event
                                         'created-at))))))

(define (render-comment-node G_
                             comment
                             comments
                             users
                             editable?
                             project
                             human-id
                             depth
                             seen
                             viewer-id
                             moderator?)
  (let* ((id (assq-ref comment
                       'id))
         (author (assq-ref comment
                           'author-id))
         (author-name (let ((u (and author
                                    (find (lambda (row)
                                            (equal? (assq-ref row
                                                              'id) author))
                                          users))))
                        (if u
                            (assq-ref u
                                      'username) "")))
         (deleted? (assq-ref comment
                             'deleted-at))
         (body (assq-ref comment
                         'body))
         (children (comment-children comments id))
         ;; Removing a comment is offered by the same rule the endpoint
         ;; applies it by: its author may take it back, and a moderator may
         ;; take anybody's -- so a reader is never shown a control that
         ;; could only ever be refused.
         (removable? (and editable?
                          (not deleted?)
                          (or moderator?
                              (and viewer-id author
                                   (equal? (format #f "~a" viewer-id)
                                           (format #f "~a" author)))))))
    `(div (@ (class ,(mk-class '(flex flex-col gap-2
                                        border-2
                                        border-stone-300/50
                                        dark:border-onyx-100/50
                                        rounded-lg
                                        px-3
                                        py-2
                                        my-1)))
             ;; A reply is one step in from the comment it answers, and how
             ;; deep the thread runs is how many steps it has taken.  The
             ;; indent is what says which comment is being answered, so it is
             ;; drawn rather than described; a comment of the page is at the
             ;; left edge of the panel, under the form that writes one.
             (style ,(format #f "margin-left: ~apx"
                             (* 18 depth))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                           items-baseline))))
               (strong ,(if (string-null? author-name)
                            (G_ "somebody") author-name))
               (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                                     dark:text-stone-500))))
                      ,(recorded-at (assq-ref comment
                                              'created-at))))
          ,@(if deleted?
                `((em (@ (class ,(mk-class '(text-sm text-onyx-500
                                                     dark:text-stone-500))))
                      ,(G_ "[removed]")))
                `((div (@ (class ,(mk-class '(text-md leading-7 max-w-prose
                                                      wrap-break-word))))
                       ,(org2html (html-unescape body)))))
          ,@(if (and editable?
                     (not deleted?))
                `((details (summary (@ (class ,(mk-class '(text-sm
                                                           cursor-pointer
                                                           font-medium))))
                                    ,(G_ "reply"))
                           (form (@ (enctype
                                     "application/x-www-form-urlencoded")
                                    (method POST)
                                    (action ,(format #f
                                              "/tickets/~a/id/~a/comments"
                                              project human-id)))
                                 (input (@ (type hidden)
                                           (name "parent-id")
                                           (value ,id)))
                                 ,(textarea #:label (G_ "your reply")
                                           #:name "body"
                                           #:required #t
                                           #:rows 3
                                           #:textarea-css '(leading-7 p-3
                                                            rounded-xl
                                                            border-stone-700
                                                            dark:border-onyx-100
                                                            dark:text-stone-300
                                                            dark:bg-onyx-700/75
                                                            bg-stone-300/75
                                                            border-2
                                                            min-h-24
                                                            w-full
                                                            max-w-prose
                                                            shadow-md))
                                 (div ,(submit-button (G_ "reply"))))))
                '())
          ,@(if removable?
                `((form (@ (enctype "application/x-www-form-urlencoded")
                           (method POST)
                           (action ,(format #f
                                     "/tickets/~a/id/~a/comments/~a/delete"
                                     project human-id id)))
                        ,(danger-submit-button (G_ "remove"))))
                '())
          ,@(map (lambda (child)
                   (render-comment-node G_
                                        child
                                        comments
                                        users
                                        editable?
                                        project
                                        human-id
                                        (+ depth 1)
                                        (cons (assq-ref child
                                                        'id) seen)
                                        viewer-id
                                        moderator?))
                 (filter (lambda (child)
                           (not (member (assq-ref child
                                                  'id) seen))) children)))))

(define (section-title G_ icon-name text count)
  "Answer the heading a panel of the ticket's record is opened with.

The heading is what sets one panel's business apart from the next one's: it
names the panel, says how many rows the panel holds, and is ruled off from
them -- the rule is the panel's edge as well as the heading's underline, so
a panel of many rows reads as one section of the ticket rather than as a
run of cards."
  `(h3 (@ (class ,(mk-class '(flex flex-row
                                   flex-wrap
                                   gap-2
                                   items-center
                                   w-full
                                   text-lg
                                   font-bold
                                   pb-2
                                   mb-1
                                   border-b-2
                                   border-stone-300/50
                                   dark:border-onyx-100/50))))
       ,(icon #:type icon-name
              #:icon-css '(text-md text-onyx-700
                           dark:text-stone-300))
       (span ,text)
       (span (@ (class ,(mk-class '(text-sm font-light
                                            text-onyx-500
                                            dark:text-stone-500))))
             ,(format #f " (~a)" count))))

;; =============================================================================
;;  The hours a ticket has cost
;; =============================================================================

;; One entry reads as one line -- how long, which day, and who -- with what was
;; done under it.  The comment is drawn as the text it is rather than through
;; the org renderer the ticket bodies go through: a note about the hour just
;; spent is a sentence, not a document, and rendering it as org would turn a
;; stray asterisk or dash in it into a list nobody wrote.
;;
;; Taking an entry back is offered by the same rule the endpoint applies it by
;; -- its author, or a moderator -- so a reader is never shown a control that
;; could only ever be refused.
(define* (render-time-entry G_
                            #:key
                            project
                            human-id
                            entry
                            users
                            (editable? #f)
                            (viewer-id #f)
                            (moderator? #f))
  (let* ((entry-id (assq-ref entry
                             'id))
         (author (assq-ref entry
                           'author-id))
         (author-name (let ((u (and author
                                    (find (lambda (row)
                                            (equal? (assq-ref row
                                                              'id) author))
                                          users))))
                        (if u
                            (assq-ref u
                                      'username) "")))
         (minutes (assq-ref entry
                            'duration-minutes))
         (spent-on (assq-ref entry
                             'spent-on))
         (comment (assq-ref entry
                            'comment))
         (removable? (and editable?
                          (or moderator?
                              (and viewer-id author
                                   (equal? (format #f "~a" viewer-id)
                                           (format #f "~a" author)))))))
    `(div (@ (class ,(mk-class '(flex flex-col gap-1 border-2
                                        border-stone-300/50
                                        dark:border-onyx-100/50
                                        rounded-lg px-3 py-2 my-1))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            items-baseline justify-between))))
               (span (@ (class ,(mk-class '(text-sm font-bold))))
                     ,(format-duration minutes))
               (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                                     dark:text-stone-500))))
                      ,(format #f "~a h · ~a"
                               (format-hours minutes)
                               (or spent-on ""))))
          (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                               dark:text-stone-500))))
                 ,(if (string-null? author-name)
                      (G_ "somebody") author-name))
          ,@(if (and (string? comment)
                     (not (string-null? comment)))
                `((div (@ (class ,(mk-class '(text-sm leading-6
                                                      max-w-prose
                                                      whitespace-pre-wrap))))
                       ,comment))
                '())
          ,@(if removable?
                `((form (@ (enctype "application/x-www-form-urlencoded")
                           (method POST)
                           (action ,(format #f
                                     "/tickets/~a/id/~a/time/~a/delete"
                                     project human-id entry-id)))
                        ,(danger-submit-button (G_ "remove"))))
                '()))))

(define* (render-time-panel G_
                            #:key
                            project
                            human-id
                            entries
                            total-minutes
                            users
                            (editable? #f)
                            (viewer-id #f)
                            (moderator? #f)
                            (today #f))
  (let* ((time-form (if editable?
                        `((form (@ (enctype
                                    "application/x-www-form-urlencoded")
                                   (method POST)
                                   (action ,(format #f
                                             "/tickets/~a/id/~a/time" project
                                             human-id)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-end))))
                                   (div ,(input #:label (G_ "how long")
                                                #:name "duration"
                                                #:required #t
                                                #:placeholder "1h 30m"
                                                #:help-text (G_
                                                             "1.5h, 90m, 1h30m or 1:30 all mean the same")))
                                   (div ,(date-input (G_ "which day")
                                                     "spent-on"
                                                     (or today
                                                         (today-iso)))))
                              ,(textarea #:label (G_ "what was done")
                                        #:name "comment"
                                        #:rows 2
                                        #:help-text (G_
                                                     "the note that says what the hours bought")
                                        #:textarea-css '(leading-7 p-3
                                                         rounded-xl
                                                         border-stone-700
                                                         dark:border-onyx-100
                                                         dark:text-stone-300
                                                         dark:bg-onyx-700/75
                                                         bg-stone-300/75
                                                         border-2
                                                         min-h-24
                                                         w-full
                                                         max-w-prose
                                                         shadow-md))
                              (div ,(submit-button (G_ "log time")))))
                        '())))
    `(,@(if (pair? time-form)
            (append time-form '()) '())
      ,@(if (pair? entries)
            (map (lambda (entry)
                   (render-time-entry G_
                                      #:project project
                                      #:human-id human-id
                                      #:entry entry
                                      #:users users
                                      #:editable? editable?
                                      #:viewer-id viewer-id
                                      #:moderator? moderator?))
                 entries)
            `((p (@ (class ,(mk-class '(text-sm text-onyx-500
                                               dark:text-stone-500))))
                 ,(G_ "no time logged yet")))))))

;; =============================================================================
;;  Where the ticket sits in the plan
;; =============================================================================

;; The deadline, the sprint and the milestone are one form because they are one
;; question: when is this due, and where does it count.  All three may be
;; cleared, so the form always posts all three and what it says is written as
;; it stands -- there is no "leave this one alone" row, because the page is
;; showing the whole answer and the reader is answering all of it.
(define* (render-planning-panel G_
                                #:key
                                project
                                human-id
                                ticket
                                sprints
                                milestones
                                (editable? #f)
                                (today #f))
  (let* ((due-on (assq-ref ticket
                           'due-on))
         (sprint-id (let ((value (assq-ref ticket
                                            'sprint-id)))
                      (and value
                           (format #f "~a" value))))
         (milestone-id (let ((value (assq-ref ticket
                                               'milestone-id)))
                         (and value
                              (format #f "~a" value))))
         (sprint (and sprint-id
                      (find (lambda (s)
                              (equal? (format #f "~a"
                                              (assq-ref s
                                                        'id))
                                      sprint-id)) sprints)))
         (milestone (and milestone-id
                          (find (lambda (m)
                                  (equal? (format #f "~a"
                                                  (assq-ref m
                                                            'id))
                                          milestone-id)) milestones)))
         (plan-form (if editable?
                        `((form (@ (enctype "application/x-www-form-urlencoded")
                                   (method POST)
                                   (action ,(format #f
                                             "/tickets/~a/id/~a/planning"
                                             project human-id)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-end))))
                                   (div ,(date-input (G_ "due on")
                                                     "due-on"
                                                     due-on))
                                   (div ,(select #:name 'sprint-id
                                                 #:label (G_ "sprint")
                                                 #:on-change ""
                                                 #:options (cons (option #:value ""
                                                                         #:content (G_
                                                                                    "no sprint")
                                                                         #:selected-value
                                                                         (or sprint-id
                                                                             ""))
                                                                 (map (lambda (s)
                                                                        (option #:value
                                                                                (format #f
                                                                                 "~a"
                                                                                 (assq-ref s
                                                                                           'id))
                                                                                #:content
                                                                                (assq-ref s
                                                                                          'title)
                                                                                #:selected-value
                                                                                (or sprint-id
                                                                                    "")))
                                                                      sprints))))
                                   (div ,(select #:name 'milestone-id
                                                 #:label (G_ "milestone")
                                                 #:on-change ""
                                                 #:options (cons (option #:value ""
                                                                         #:content (G_
                                                                                    "no milestone")
                                                                         #:selected-value
                                                                         (or milestone-id
                                                                             ""))
                                                                 (map (lambda (m)
                                                                        (option #:value
                                                                                (format #f
                                                                                 "~a"
                                                                                 (assq-ref m
                                                                                           'id))
                                                                                #:content
                                                                                (assq-ref m
                                                                                          'title)
                                                                                #:selected-value
                                                                                (or milestone-id
                                                                                    "")))
                                                                      milestones)))))
                              (div ,(submit-button (G_ "save the plan")))))
                        '())))
    `(,@(if (pair? plan-form)
            (append plan-form '())
            `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
                  (div ,@(render-due-badge G_ due-on today))
                  (div (@ (class ,(mk-class '(text-sm))))
                       (strong ,(G_ "sprint")) ": "
                       ,(if sprint
                            (assq-ref sprint
                                      'title)
                            (G_ "none")))
                  (div (@ (class ,(mk-class '(text-sm))))
                       (strong ,(G_ "milestone")) ": "
                       ,(if milestone
                            (assq-ref milestone
                                      'title)
                            (G_ "none")))))))))

(define* (html-ticket-collaboration G_
                                    #:key project
                                    ticket
                                    projects
                                    statuses
                                    users
                                    comments
                                    attachments
                                    events
                                    revisions
                                    (time-entries '())
                                    (logged-minutes 0)
                                    (sprints '())
                                    (milestones '())
                                    (today #f)
                                    (editable? #f)
                                    (viewer-id #f)
                                    (moderator? #f))
  (let* ((human-id (assq-ref ticket
                             'human-id))
         (comment-form (if editable?
                           `((form (@ (enctype
                                       "application/x-www-form-urlencoded")
                                      (method POST)
                                      (action ,(format #f
                                                "/tickets/~a/id/~a/comments"
                                                project human-id)))
                                   ,(textarea #:label (G_ "add a comment")
                                             #:name "body"
                                             #:required #t
                                             #:rows 3
                                             #:textarea-css '(leading-7 p-3
                                                              rounded-xl
                                                              border-stone-700
                                                              dark:border-onyx-100
                                                              dark:text-stone-300
                                                              dark:bg-onyx-700/75
                                                              bg-stone-300/75
                                                              border-2
                                                              min-h-24
                                                              w-full
                                                              max-w-prose
                                                              shadow-md))
                                   (div ,(submit-button (G_ "comment")))))
                           '()))
         (attachment-form (if editable?
                              `((form (@ (enctype "multipart/form-data")
                                         (method POST)
                                         (action ,(format #f
                                                   "/tickets/~a/id/~a/attachments"
                                                   project human-id)))
                                      (label (@ (class ,(mk-class '(text-sm
                                                                    font-medium))))
                                             ,(G_ "attach a file"))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-row
                                                                  flex-wrap
                                                                  gap-2
                                                                  items-center
                                                                  my-2))))
                                           (input (@ (type file)
                                                     (name "file")
                                                     (required "")
                                                     (class ,(mk-class '(text-sm
                                                                         font-sans)))))
                                           ,(submit-button (G_ "attach")))))
                              '()))
         (move-section (if (and editable?
                                (> (length projects) 1))
                           `((details (summary (@ (class ,(mk-class '(text-sm
                                                                      font-medium
                                                                      cursor-pointer))))
                                               ,(G_
                                                 "move ticket to another project"))
                                      (form (@ (enctype
                                                "application/x-www-form-urlencoded")
                                               (method POST)
                                               (action ,(format #f
                                                         "/tickets/~a/id/~a/move"
                                                         project human-id)))
                                            (div (@ (class ,(mk-class '(flex
                                                                        flex-row
                                                                        flex-wrap
                                                                        gap-2
                                                                        items-end
                                                                        my-2))))
                                                 (div ,(select #:name 'target-project
                                                               #:label (G_
                                                                        "project")
                                                               #:on-change ""
                                                               #:options (map (lambda
                                                                                      (p)

                                                                                (option
                                                                                 #:value
                                                                                 (assq-ref
                                                                                  p
                                                                                  'human-id)
                                                                                 #:content
                                                                                 (assq-ref
                                                                                  p
                                                                                  'title)))

                                                                              (filter (lambda
                                                                                              (p)

                                                                                        (not
                                                                                         (equal?
                                                                                          (assq-ref
                                                                                           p
                                                                                           'human-id)
                                                                                          project)))
                                                                               projects))))
                                                 (div ,(submit-button (G_
                                                                       "move")))))))
                           '()))
         (latest-version (if (pair? revisions)
                             (assq-ref (car revisions)
                                       'version) 0))
         ;; The names of the things a ticket's history can name by id: the
         ;; sprint it was taken into and the milestone it was pointed at.  The
         ;; trail records the id -- that is what the burndown replays -- and
         ;; this turns it back into the name a reader knows.
         (plan (append (map (lambda (s)
                              (cons (format #f "~a"
                                            (assq-ref s
                                                      'id))
                                    (assq-ref s
                                              'title)))
                            sprints)
                       (map (lambda (m)
                              (cons (format #f "~a"
                                            (assq-ref m
                                                      'id))
                                    (assq-ref m
                                              'title)))
                            milestones))))
    (append
     ;; -- The plan ---------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'calendar-check
                                          (G_ "plan")
                                          (length (filter (lambda (value)
                                                            value)
                                                          (list (assq-ref
                                                                 ticket
                                                                 'due-on)
                                                                (assq-ref
                                                                 ticket
                                                                 'sprint-id)
                                                                (assq-ref
                                                                 ticket
                                                                 'milestone-id)))))
                          ,@(render-planning-panel G_
                                                   #:project project
                                                   #:human-id human-id
                                                   #:ticket ticket
                                                   #:sprints sprints
                                                   #:milestones milestones
                                                   #:today today
                                                   #:editable? editable?))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Time -------------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'hourglass
                                          (G_ "time logged")
                                          (length time-entries))
                          ,@(if (> logged-minutes 0)
                                `((div (@ (class ,(mk-class '(my-1))))
                                       ,(render-logged-time G_ logged-minutes)))
                                '())
                          ,@(render-time-panel G_
                                               #:project project
                                               #:human-id human-id
                                               #:entries time-entries
                                               #:total-minutes logged-minutes
                                               #:users users
                                               #:today today
                                               #:editable? editable?
                                               #:viewer-id viewer-id
                                               #:moderator? moderator?))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Discussion -------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'chat-circle
                                          (G_ "discussion")
                                          (length comments)) ,@comment-form
                          ,@(if (pair? comments)
                                (map (lambda (comment)
                                       (render-comment-node G_
                                                            comment
                                                            comments
                                                            users
                                                            editable?
                                                            project
                                                            human-id
                                                            0
                                                            (list (assq-ref
                                                                   comment
                                                                   'id))
                                                            viewer-id
                                                            moderator?))
                                     (filter (lambda (comment)
                                               (not (assq-ref comment
                                                              'parent-id)))
                                             comments))
                                `((p (@ (class ,(mk-class '(text-sm
                                                            text-onyx-500
                                                            dark:text-stone-500))))
                                     ,(G_ "no comments yet")))))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Files ------------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'paperclip
                                          (G_ "attachments")
                                          (length attachments)) ,@attachment-form
                          ,@(if (pair? attachments)
                                (map (lambda (attachment)
                                       (let ((url (format #f
                                                   "/tickets/~a/id/~a/attachments/~a"
                                                   project human-id
                                                   (assq-ref attachment
                                                             'id))))
                                         `(div (@ (class ,(mk-class '(flex
                                                                      flex-row
                                                                      flex-wrap
                                                                      gap-2
                                                                      items-center
                                                                      justify-between
                                                                      border-2
                                                                      border-stone-300/50
                                                                      dark:border-onyx-100/50
                                                                      rounded-lg
                                                                      px-3
                                                                      py-2
                                                                      my-1))))
                                               (div (@ (class ,(mk-class '(flex
                                                                           flex-row
                                                                           flex-wrap
                                                                           gap-2
                                                                           items-center))))
                                                    (span ,(icon #:type 'file
                                                                 #:icon-css '(text-md)))
                                                    (a (@ (href ,url))
                                                       ,(assq-ref attachment
                                                                  'filename))
                                                    (small (@ (class ,(mk-class '
                                                                       (text-xs
                                                                        text-onyx-500
                                                                        dark:text-stone-500))))
                                                           ,(attachment-size-label
                                                             (assq-ref
                                                              attachment
                                                              'size))))
                                               ,@(if editable?
                                                     `((form (@ (method POST)
                                                                (action ,(format
                                                                          #f
                                                                          "~a/delete"
                                                                          url)))
                                                             ,(danger-submit-button
                                                               (G_ "remove"))))
                                                     '())))) attachments)
                                `((p (@ (class ,(mk-class '(text-sm
                                                            text-onyx-500
                                                            dark:text-stone-500))))
                                     ,(G_ "no attachments yet")))))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Audit trail ------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'clock-counter-clockwise
                                          (G_ "activity")
                                          (length events)) ,@(if (pair? events)
                                                                 (map (lambda
                                                                              (event)
                                                                        (render-event
                                                                         G_
                                                                         event
                                                                         users
                                                                         #:plan
                                                                         plan))
                                                                      events)
                                                                 `((p (@ (class ,
                                                                          (mk-class '
                                                                           (text-sm
                                                                            text-onyx-500
                                                                            dark:text-stone-500))))
                                                                      ,(G_
                                                                        "nothing recorded yet")))))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Versions ---------------------------------------------------------
     (list (glass-panel `(,(section-title G_ 'git-branch
                                          (G_ "versions")
                                          (length revisions)) ,@(if (pair?
                                                                     revisions)
                                                                    (map (lambda
                                                                                 (revision)
                                                                           (let*
                                                                                 (
                                                                                  (version
                                                                                   (assq-ref
                                                                                    revision
                                                                                    'version))

                                                                                  (previous (let loop

                                                                                              (
                                                                                               (rest
                                                                                                revisions))
                                                                                              (cond

                                                                                                (
                                                                                                 (null?
                                                                                                  rest)
                                                                                                 #f)

                                                                                                (
                                                                                                 (equal?
                                                                                                  (assq-ref
                                                                                                   (car
                                                                                                    rest)
                                                                                                   'version)
                                                                                                  version)

                                                                                                 (and
                                                                                                  (pair?
                                                                                                   (cdr
                                                                                                    rest))

                                                                                                  (cadr
                                                                                                   rest)))

                                                                                                (else
                                                                                                 (loop
                                                                                                  (cdr
                                                                                                   rest))))))

                                                                                  (changed
                                                                                   (if
                                                                                    previous

                                                                                    (map
                                                                                     cadr

                                                                                     (ticket-changes
                                                                                      previous
                                                                                      revision))
                                                                                    '()))

                                                                                  (author
                                                                                   (assq-ref
                                                                                    revision
                                                                                    'author-id))

                                                                                  (author-name (let
                                                                                                    (
                                                                                                     (u
                                                                                                      (and
                                                                                                       author

                                                                                                       (find (lambda
                                                                                                                     (row)

                                                                                                               (equal?
                                                                                                                (assq-ref
                                                                                                                 row
                                                                                                                 'id)
                                                                                                                author))
                                                                                                        users))))

                                                                                                 (if
                                                                                                  u

                                                                                                  (assq-ref
                                                                                                   u
                                                                                                   'username)
                                                                                                  ""))))
                                                                             `
                                                                             (details
                                                                              (@
                                                                               (class ,
                                                                                (mk-class '
                                                                                 (border-2
                                                                                  border-stone-300/50
                                                                                  dark:border-onyx-100/50
                                                                                  rounded-lg
                                                                                  px-3
                                                                                  py-2
                                                                                  my-1))))

                                                                              (summary
                                                                               (@
                                                                                (class ,
                                                                                 (mk-class '
                                                                                  (text-sm
                                                                                   font-medium
                                                                                   cursor-pointer))))
                                                                               ,
                                                                               (format
                                                                                #f
                                                                                "v~a — ~a — ~a"
                                                                                version

                                                                                (recorded-at
                                                                                 (assq-ref
                                                                                  revision
                                                                                  'created-at))

                                                                                (if
                                                                                 (string-null?
                                                                                  author-name)

                                                                                 (G_
                                                                                  "somebody")
                                                                                 author-name)))
                                                                              ,@
                                                                              (if
                                                                               (pair?
                                                                                changed)
                                                                               `
                                                                               (
                                                                                (small
                                                                                 (@
                                                                                  (class ,
                                                                                   (mk-class '
                                                                                    (text-xs
                                                                                     text-onyx-500
                                                                                     dark:text-stone-500))))
                                                                                 ,
                                                                                 (format
                                                                                  #f
                                                                                  "~a: ~a"

                                                                                  (G_
                                                                                   "changed")

                                                                                  (string-join
                                                                                   changed
                                                                                   ", "))))
                                                                               '())

                                                                              (div
                                                                               (@
                                                                                (class ,
                                                                                 (mk-class '
                                                                                  (text-sm
                                                                                   leading-7
                                                                                   max-w-prose
                                                                                   wrap-break-word))))

                                                                               (strong ,
                                                                                (G_
                                                                                 "title"))
                                                                               ": "
                                                                               ,
                                                                               (or
                                                                                (assq-ref
                                                                                 revision
                                                                                 'title)
                                                                                ""))

                                                                              (div
                                                                               (@
                                                                                (class ,
                                                                                 (mk-class '
                                                                                  (text-sm))))

                                                                               (strong ,
                                                                                (G_
                                                                                 "status"))
                                                                               ": "
                                                                               ,
                                                                               (status-label
                                                                                G_
                                                                                statuses

                                                                                (or
                                                                                 (assq-ref
                                                                                  revision
                                                                                  'ticket-status)
                                                                                 "")))

                                                                              (div
                                                                               (@
                                                                                (class ,
                                                                                 (mk-class '
                                                                                  (text-sm))))

                                                                               (strong ,
                                                                                (G_
                                                                                 "type"))
                                                                               ": "
                                                                               ,
                                                                               (ticket-type-label
                                                                                G_

                                                                                (or
                                                                                 (assq-ref
                                                                                  revision
                                                                                  'ticket-type)
                                                                                 "")))
                                                                              ,@
                                                                              (if
                                                                               (assq-ref
                                                                                revision
                                                                                'priority)
                                                                               `
                                                                               (
                                                                                (div
                                                                                 (@
                                                                                  (class ,
                                                                                   (mk-class '
                                                                                    (text-sm))))

                                                                                 (strong ,
                                                                                  (G_
                                                                                   "priority"))
                                                                                 ": "
                                                                                 ,
                                                                                 (format
                                                                                  #f
                                                                                  "~a"

                                                                                  (assq-ref
                                                                                   revision
                                                                                   'priority))))
                                                                               '())
                                                                              ,@
                                                                              (if
                                                                               (assq-ref
                                                                                revision
                                                                                'estimate-points)
                                                                               `
                                                                               (
                                                                                (div
                                                                                 (@
                                                                                  (class ,
                                                                                   (mk-class '
                                                                                    (text-sm))))

                                                                                 (strong ,
                                                                                  (G_
                                                                                   "story points"))
                                                                                 ": "
                                                                                 ,
                                                                                 (format
                                                                                  #f
                                                                                  "~a"

                                                                                  (assq-ref
                                                                                   revision
                                                                                   'estimate-points))))
                                                                               '())
                                                                              ,@
                                                                              (if
                                                                               (assq-ref
                                                                                revision
                                                                                'estimate-size)
                                                                               `
                                                                               (
                                                                                (div
                                                                                 (@
                                                                                  (class ,
                                                                                   (mk-class '
                                                                                    (text-sm))))

                                                                                 (strong ,
                                                                                  (G_
                                                                                   "size"))
                                                                                 ": "
                                                                                 ,
                                                                                 (ticket-size-label
                                                                                  (assq-ref
                                                                                   revision
                                                                                   'estimate-size))))
                                                                               '())
                                                                              ,@
                                                                              (if
                                                                               (assq-ref
                                                                                revision
                                                                                'description)
                                                                               `
                                                                               (
                                                                                (div
                                                                                 (@
                                                                                  (class ,
                                                                                   (mk-class '
                                                                                    (text-md
                                                                                     leading-7
                                                                                     max-w-prose
                                                                                     wrap-break-word))))
                                                                                 ,
                                                                                 (org2html
                                                                                  (html-unescape
                                                                                   (assq-ref
                                                                                    revision
                                                                                    'description)))))
                                                                               '())
                                                                              ,@
                                                                              (if
                                                                               (and
                                                                                editable?

                                                                                (not
                                                                                 (equal?
                                                                                  version
                                                                                  latest-version)))
                                                                               `
                                                                               (
                                                                                (form
                                                                                 (@
                                                                                  (method
                                                                                   POST)

                                                                                  (action ,
                                                                                   (format
                                                                                    #f
                                                                                    "/tickets/~a/id/~a/revisions/~a/restore"
                                                                                    project
                                                                                    human-id
                                                                                    version)))

                                                                                 (div ,
                                                                                  (submit-button
                                                                                   (G_
                                                                                    "restore this version")))))
                                                                               '()))))
                                                                     revisions)
                                                                    `((p (@ (class ,
                                                                             (mk-class '
                                                                              (text-sm
                                                                               text-onyx-500
                                                                               dark:text-stone-500))))
                                                                         ,(G_
                                                                           "no versions yet")))))
                        #:frost 3
                        #:padding '(p-4 sm:p-6)
                        #:classes '(flex flex-col gap-3)))
     ;; -- Moving -----------------------------------------------------------
     (if (pair? move-section)
         (list (glass-panel move-section
                            #:frost 3
                            #:padding '(p-4 sm:p-6)
                            #:classes '(flex flex-col gap-3)))
         '()))))
