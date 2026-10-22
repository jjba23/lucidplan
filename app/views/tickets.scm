;;; lucidplan - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

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

(define %label-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-md font-light)))

(define %val-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-md ml-4)))

(define %mono-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-sm ml-4
                        font-mono)))

(define %list-wrapper-face
  (mk-class '(flex flex-col gap-2)))

(define %list-header-face
  ;; One line, always: the grip, the type's glyph, the title and the link
  ;; stand in a row, and the title takes whatever is left.  Nothing else
  ;; rides here -- the triage pills belong to the card the row unfolds into
  ;; -- because a wider row would either wrap, and stop reading as a line, or
  ;; squeeze the title it exists to show down to a few characters.
  (mk-class (append '(flex flex-row
                           gap-2
                           items-center
                           justify-start
                           dark:border-onyx-100/75
                           border-stone-300/75
                           shadow-md
                           border-2
                           py-2
                           px-4
                           cursor-pointer
                           rounded-xl) frosted-tint-classes)))

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

(define (render-meta-badge label icon value)
  `(div (@ (class ,(mk-class '(flex flex-col gap-3))))
        (small (@ (class ,%label-face))
               ,label)
        (div (@ (class ,(mk-class '(ml-4 flex gap-2 items-center))))
             (span (@ (class ,(mk-class '(text-2xl))))
                   ,icon)
             (p (@ (class ,%label-face))
                ,value))))

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

The control is a native menu wearing a frosted pill, so what it shows is the
value the ticket holds and a change is one gesture rather than a trip
through the edit form.  It posts as soon as it is changed and is confirmed
by a card in the tray.  The label is there for assistive technology -- the
column above the pill already says which field this is.

The menu is named and identified after the field it changes, which is what
the control needs on a page holding one of them.  A page holding one per
row -- the backlog, where every row carries its own pair -- passes
@var{id-suffix} to tell the rows apart: an id names one element, and a page
of thirty-three controls sharing one name is a page whose labels all point
at the first of them."
  `(div (@ (class ,(mk-class (append '(px-2 py-1 w-fit rounded-full)
                                     frosted-tint-classes)))
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

(define (render-field label value val-class)
  `((small (@ (class ,%label-face))
           ,label)
    (small (@ (class ,val-class))
           ,(format #f "~a" value))))

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
                        (editable? #f)
                        (statuses (default-ticket-statuses))
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

         (toggle-js (format #f
                     "document.getElementById('details-~a').toggleAttribute('hidden')"
                     id))
         (edit-url (format #f "/tickets/~a/id/~a/edit" project human-id))
         (delete-url (format #f "/tickets/~a/delete" project)))
    
    `(div (@ (class ,%card-face))

          ;; Header
          (div (@ (class ,(mk-class '(flex flex-row justify-between))))
               ,(h4 (org2html (html-unescape title)))
               (div (@ (class ,(mk-class '(flex justify-end))))
                    (span (@ (class ,(mk-class '(cursor-pointer)))
                             (onclick ,toggle-js))
                          ,(icon #:type 'x
                                 #:icon-css '(text-onyx-700
                                              dark:text-stone-300 text-xl)))))

          ;; Meta Info (Status & Type).  Where the reader can change them each
          ;; of the two is the control that posts its own change; where they
          ;; cannot -- a reader who is not signed in -- they stay the
          ;; read-only badges they read as at a glance.  Either way what they
          ;; say is the label a reader knows the value by, never the value
          ;; itself: `work-in-progress' and `time-box' are how the schema
          ;; spells a state, not how a reader names it.
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-6
                                           justify-start))))
               ,@(if editable?
                     `((div (@ (class ,(mk-class '(flex flex-col gap-3))))
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
                       (div (@ (class ,(mk-class '(flex flex-col gap-3))))
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
                       (div (@ (class ,(mk-class '(flex flex-col gap-3))))
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
                       (div (@ (class ,(mk-class '(flex flex-col gap-3))))
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
                     `(,(render-meta-badge (G_ "status")
                                           (status-icon-node statuses status)
                                           (status-label G_ statuses status)) ,
                       (render-meta-badge (G_ "type")
                                          (emoji-for-ticket-type type)
                                          (ticket-type-label G_ type)))))

          ;; Description
          ,(render-if-nestring (lambda (d)
                                 `((small (@ (class ,%label-face))
                                          ,(G_ "description"))
                                   (span (@ (class ,(mk-class '(text-md py-3
                                                                text-onyx-700
                                                                dark:text-stone-300
                                                                font-medium
                                                                font-sans
                                                                max-w-prose
                                                                whitespace-pre-wrap
                                                                leading-7
                                                                ml-4))))
                                         ,(org2html (html-unescape d))))) desc)

          ;; Ticket Details
          (small (@ (class ,%mono-face))
                 ,(format #f "~a-~s" project human-id))

          ,@(if (and (not (nil? priority))
                     (> priority 0))
                (render-field (G_ "priority") priority %val-face)
                '())

          ,@(if (not (nil? assignee))
                (render-field (G_ "assignee") assignee %val-face)
                '())

          ;; Where the reader cannot change them, the estimates are the two
          ;; plain lines they read as.  Where they can, they are the menus at
          ;; the top of the card, and showing them twice would only make the
          ;; card longer.
          ,@(if (and (not editable?) points)
                (render-field (G_ "story points") points %val-face)
                '())
          ,@(if (and (not editable?)
                     (not (nil? size)))
                (render-field (G_ "size")
                              (ticket-size-label size) %val-face)
                '())

          ;; Action Buttons
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4 py-3))))
               (a (@ (href ,edit-url))
                  ,(icon-button #:icon 'pencil
                                #:content (G_ "edit ticket")
                                #:icon-css '(text-md text-onyx-700
                                                     dark:text-stone-300)))

               (form (@ (enctype application/x-www-form-urlencoded)
                        (method POST)
                        (action ,delete-url))
                     (input (@ (hidden "")
                               (name "id")
                               (value ,id)))
                     ,(danger-submit-button `(span (@ (class ,(mk-class '(items-center
                                                                          flex
                                                                          flex-row
                                                                          gap-1))))
                                                   ,(icon #:type 'trash
                                                          #:icon-css '(text-stone-300
                                                                       dark:text-stone-300
                                                                       text-lg))
                                                   ,(G_ "delete"))))))))

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
         (details-id (format #f "details-~a" id))
         (toggle-js (format #f
                     "document.getElementById('~a').toggleAttribute('hidden')"
                     details-id))
         (ticket-url (format #f "/tickets/~a/id/~a" project human-id))
         (assignee-user (find (lambda (u)
                                (equal? (assq-ref u
                                                  'id) ticket-assignee))
                              user-names)))
    
    `(div (@ (class ,%list-wrapper-face)
             (id ,item-id)
             (data-ticket-id ,human-id)
             (data-priority ,(number->string priority)))

          ;; Clickable Header Row
          (div (@ (onclick ,toggle-js)
                  (tabindex "0")
                  (onkeydown
                   "if(event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }")
                  (class ,%list-header-face))
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

               ;; Direct Link Icon
               (div (a (@ (class ,%list-link-face)
                          (href ,ticket-url))
                       ,(icon #:type 'link
                              #:icon-css '(text-md text-onyx-700
                                           dark:text-stone-300
                                           hover:text-asparagus-500
                                           dark:hover:text-asparagus-300)))))

          ;; Expandable Details Container
          (div (@ (id ,details-id)
                  (class ,(mk-class '(ml-1 sm:ml-3)))
                  (hidden ""))
               ,(render-ticket G_
                               #:project project
                               #:ticket ticket
                               ;; The card is inside a list, so its two
                               ;; controls are told apart from the pairs the
                               ;; other rows carry; the page says which row
                               ;; by naming it.
                               #:editable? editable?
                               #:statuses statuses
                               #:id-suffix (format #f "-row-~a" human-id)
                               #:assignee (if (nil? assignee-user) #f
                                              (assq-ref assignee-user
                                                        'username)))))))

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
             ,(render-toggle-button (G_ "Expand All")
               "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = false)")

             ,(render-toggle-button (G_ "Collapse All")
               "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = true)"))))

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
                            search-query)
  (let* ( ;Core Data
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
    ((equal? action "deleted")
     (G_ "deleted"))
    (else action)))

(define (render-event G_ event users)
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
         (old (assq-ref event
                        'old-value))
         (new (assq-ref event
                        'new-value)))
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
                 ,(or (assq-ref event
                                'created-at) "")))))

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
    `(div (@ (class ,(mk-class '(flex flex-col gap-1 py-2)))
             (style ,(format #f "margin-left: ~apx"
                             (* 18 depth))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                           items-baseline))))
               (strong ,(if (string-null? author-name)
                            (G_ "somebody") author-name))
               (small (@ (class ,(mk-class '(text-xs text-onyx-500
                                                     dark:text-stone-500))))
                      ,(or (assq-ref comment
                                     'created-at) "")))
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
                                 (textarea #:label (G_ "your reply")
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
                                    (editable? #f)
                                    (viewer-id #f)
                                    (moderator? #f))
  (let* ((human-id (assq-ref ticket
                             'human-id))
         (section-title (lambda (icon-name text count)
                          `(h3 (@ (class ,(mk-class '(flex flex-row
                                                           flex-wrap
                                                           gap-2
                                                           items-center
                                                           text-lg
                                                           font-bold
                                                           my-2))))
                               ,(icon #:type icon-name
                                      #:icon-css '(text-md text-onyx-700
                                                   dark:text-stone-300))
                               (span ,text)
                               (span (@ (class ,(mk-class '(text-sm font-light
                                                            text-onyx-500
                                                            dark:text-stone-500))))
                                     ,(format #f " (~a)" count)))))
         (comment-form (if editable?
                           `((form (@ (enctype
                                       "application/x-www-form-urlencoded")
                                      (method POST)
                                      (action ,(format #f
                                                "/tickets/~a/id/~a/comments"
                                                project human-id)))
                                   (textarea #:label (G_ "add a comment")
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
                                       'version) 0)))
    (append
     ;; -- Discussion -------------------------------------------------------
     (list (glass-panel `(,(section-title 'chat-circle
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
                        #:padding '(p-4 sm:p-5)
                        #:classes '(flex flex-col gap-2 my-3)))
     ;; -- Files ------------------------------------------------------------
     (list (glass-panel `(,(section-title 'paperclip
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
                        #:padding '(p-4 sm:p-5)
                        #:classes '(flex flex-col gap-2 my-3)))
     ;; -- Audit trail ------------------------------------------------------
     (list (glass-panel `(,(section-title 'clock-counter-clockwise
                                          (G_ "activity")
                                          (length events)) ,@(if (pair? events)
                                                                 (map (lambda 
                                                                              (event)
                                                                        (render-event
                                                                         G_
                                                                         event
                                                                         users))
                                                                      events)
                                                                 `((p (@ (class ,
                                                                          (mk-class '
                                                                           (text-sm
                                                                            text-onyx-500
                                                                            dark:text-stone-500))))
                                                                      ,(G_
                                                                        "nothing recorded yet")))))
                        #:frost 3
                        #:padding '(p-4 sm:p-5)
                        #:classes '(flex flex-col gap-1 my-3)))
     ;; -- Versions ---------------------------------------------------------
     (list (glass-panel `(,(section-title 'git-branch
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
                                                                                
                                                                                (or
                                                                                 (assq-ref
                                                                                  revision
                                                                                  'created-at)
                                                                                 "")
                                                                                
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
                        #:padding '(p-4 sm:p-5)
                        #:classes '(flex flex-col gap-1 my-3)))
     ;; -- Moving -----------------------------------------------------------
     (if (pair? move-section)
         (list (glass-panel move-section
                            #:frost 3
                            #:padding '(p-4 sm:p-5)
                            #:classes '(flex flex-col gap-2 my-3)))
         '()))))
