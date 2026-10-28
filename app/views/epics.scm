;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The page a project's epics are read on, and the form that writes one.
;;;
;;; An epic is the middle height of a plan: the project says what the whole
;;; thing is, a ticket says what one person does next, and an epic says what a
;;; group of those tickets is *for*.  This page is where that middle height is
;;; read, so it does the two things the middle height is for -- it says how far
;;; each body of work has got, and it says what is in it.
;;;
;;; Progress is counted, not kept.  An epic has no status of its own (see
;;; `(lucidplan domain epic)'), so the bar beside its name is the tickets that
;;; point at it, of which some number sit in a bucket the project calls done.
;;; A page that counted something else would be a page that could disagree with
;;; the backlog, and there is no version of that worth having.
;;;
;;; The epics are drawn as the same unfolding cards a backlog is drawn with --
;;; one card, a line while it is closed, the rest of it when it is opened --
;;; because they are the same kind of thing: a group a reader opens to see what
;;; is in it.

(define-module (app views epics)
  #:use-module (lucidplan html)
  #:use-module (lucidplan config)
  #:use-module (lucidplan domain)
  #:use-module (lucidplan avatars)
  #:use-module (kracht prelude)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (epics-page epic-form-page epic-chip epic-hue
                       epic-progress epic-slug))

;;; ===== What an epic is made of =====

(define (epic-hue epic)
  "Answer the hue EPIC is drawn in.

It is derived from the epic's name exactly the way a project's letter avatar is
derived from the project's name (`avatar-hue'), which is the point: nobody has
to choose a colour, two epics are unlikely to share one by accident, and a page
that draws both marks them the same way."
  (avatar-hue (assq-ref epic
                        'title)))

(define (epic-tickets epic tickets)
  "Answer the tickets of TICKETS that belong to EPIC."
  (let ((id (format #f "~a"
                    (assq-ref epic
                              'id))))
    (filter (lambda (ticket)
              (equal? (format #f "~a"
                              (or (assq-ref ticket
                                            'epic-id) "")) id))
            tickets)))

(define* (epic-progress epic tickets statuses)
  "Answer how far EPIC has got, as @code{(done . total)}.

TOTAL is the tickets that point at it; DONE is how many of those sit in a
bucket the project calls done.  An epic with no tickets is @code{(0 . 0)} --
neither finished nor started -- which the page says in words rather than
drawing a bar of nothing."
  (let* ((mine (epic-tickets epic tickets))
         (done-values (ticket-status-values-in-bucket statuses "done"))
         (done (length (filter (lambda (ticket)
                                 (member (assq-ref ticket
                                                   'ticket-status)
                                         done-values))
                               mine))))
    (cons done (length mine))))

(define (epic-chip epic)
  "Answer the pill EPIC is named by wherever a ticket is drawn, or nothing.

It is not a control: which epic a ticket belongs to is changed from the menu
beside it, where every other fact about a ticket is changed, so the pill
carries no link and no button."
  (if (or (not epic)
          (nil? epic))
      '()
      `(span (@ (class ,(mk-class '(lp-epic-chip)))
                (style ,(format #f "--lp-epic-hue: ~a" (epic-hue epic))))
             ,(assq-ref epic
                        'title))))

;;; ===== The page =====

(define* (epics-page G_
                     #:key project epics tickets statuses
                     projects is-logged-in (can-edit? #f))
  "Answer the page a project's epics are read on."
  (let* ((human-id (assq-ref project
                             'human-id))
         (title (assq-ref project
                          'title)))
    (html-template G_
                   #:title (format #f "~a · ~a"
                                   (G_ "epics") title)
                   #:canonical-path (format #f "/epics/~a" human-id)
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-3 md:px-6 py-6
                                                              max-w-256
                                                              mx-auto
                                                              flex flex-col
                                                              gap-4))))
                                 (div (@ (class ,(mk-class '(flex flex-row
                                                                    flex-wrap
                                                                    items-center
                                                                    justify-between
                                                                    gap-3))))
                                      (div (@ (class ,(mk-class '(flex flex-row
                                                                        items-center
                                                                        gap-2
                                                                        min-w-0))))
                                           ,(icon #:type 'stack
                                                  #:icon-css '(text-2xl
                                                               text-onyx-700
                                                               dark:text-stone-300))
                                           ,(h2 (G_ "epics")))
                                      (div (@ (class ,(mk-class '(flex flex-row
                                                                        flex-wrap
                                                                        items-center
                                                                        gap-2))))
                                           ,(quiet-link #:icon 'scroll
                                                        #:label (G_ "backlog")
                                                        #:href (format #f
                                                                "/tickets/~a"
                                                                human-id))
                                           ,(quiet-link #:icon 'kanban
                                                        #:label (G_ "board")
                                                        #:href (format #f
                                                                "/kanban/~a"
                                                                human-id))
                                           ,@(if can-edit?
                                                 `((a (@ (href ,(format #f
                                                                "/epics/~a/new"
                                                                human-id)))
                                                       ,(icon-button
                                                         #:icon 'plus
                                                         #:content (G_ "new epic")
                                                         #:size '(text-sm))))
                                                 '())))
                                 ,@(if (pair? epics)
                                       (map (lambda (epic)
                                              (epic-card G_ epic
                                                         #:project project
                                                         #:tickets tickets
                                                         #:statuses statuses
                                                         #:can-edit? can-edit?))
                                            epics)
                                       (list `(div (@ (class ,(mk-class (append
                                                                         frosted-tint-classes
                                                                         '(rounded-2xl
                                                                           p-6
                                                                           text-center
                                                                           flex
                                                                           flex-col
                                                                           gap-2)))))
                                                   ,(p (G_
                                                        "An epic is one body of work: a group of tickets that is finished when they are.")
                                                      #:weight '(font-light))
                                                   ,(p (G_
                                                        "Nothing is grouped yet. Open a ticket and name the epic it belongs to, or start one from here.")
                                                      #:weight '(font-light)
                                                      #:size '(text-sm))))))))))

;; One epic, as the card a reader opens to see what is in it.
;;
;; What the card holds is built as a list before it is laid out, rather than
;; spliced in three conditional branches inside a quasiquote: the branches are
;; the part of a view that is hardest to read and easiest to miscount, and a
;; list says what it is.
(define* (epic-card G_ epic
                    #:key project tickets statuses (can-edit? #f))
  (let* ((human-id (assq-ref epic
                             'human-id))
         (title (assq-ref epic
                          'title))
         (description (or (assq-ref epic
                                    'description) ""))
         (progress (epic-progress epic tickets statuses))
         (mine (epic-tickets epic tickets))
         (project-human-id (assq-ref project
                                     'human-id))
         (body-id (format #f "epic-body-~a" human-id))
         (contents (append (if (string-null? description)
                               '()
                               (list `(p (@ (class ,(mk-class '(text-sm
                                                                font-light
                                                                leading-6
                                                                max-w-prose))))
                                         ,description)))
                           (if (pair? mine)
                               (list `(div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  gap-1))))
                                           ,@(map (lambda (ticket)
                                                    (epic-ticket-row G_ project
                                                                     ticket
                                                                     statuses))
                                                  mine)))
                               (list `(p (@ (class ,(mk-class '(text-sm
                                                                text-onyx-500
                                                                dark:text-stone-500))))
                                         ,(G_ "no tickets in this epic yet"))))
                           (if can-edit?
                               (list `(div (@ (class ,(mk-class '(flex
                                                                  flex-row
                                                                  flex-wrap
                                                                  items-center
                                                                  gap-1))))
                                           ,(quiet-link #:icon 'pencil
                                                        #:label (G_ "edit")
                                                        #:href (format #f
                                                                "/epics/~a/id/~a/edit"
                                                                project-human-id
                                                                human-id))
                                           (form (@ (method "POST")
                                                    (action ,(format #f
                                                              "/epics/~a/delete"
                                                              project-human-id)))
                                                 (input (@ (type "hidden")
                                                           (name "id")
                                                           (value ,(assq-ref epic
                                                                             'id))))
                                                 ,(quiet-submit (G_ "delete")))))
                               '()))))
    `(div (@ (class ,(mk-class (append '(w-full rounded-2xl border-2
                                                   border-stone-300/75
                                                   dark:border-onyx-100/75
                                                   shadow-md
                                                   lp-disclosure)
                                        frosted-tint-classes)))
            ;; The hue this epic is drawn in, named once on the card: the dot,
            ;; the bar and anything else it ever holds read it from here.
            (style ,(format #f "--lp-epic-hue: ~a" (epic-hue epic))))
          (div (@ (class ,(mk-class '(lp-disclosure-summary))))
               (span (@ (class ,(mk-class '(lp-epic-dot))))
                     "")
               (h3 (@ (class ,(mk-class '(font-sans text-onyx-700
                                                  dark:text-stone-300
                                                  text-md sm:text-lg
                                                  font-bold truncate grow))))
                   ,title)
               ,(epic-progress-node G_ (car progress) (cdr progress))
               ,(icon #:type 'caret-down
                      #:icon-css '(text-onyx-700 dark:text-stone-300 text-lg
                                   lp-disclosure-chevron)))
          (div (@ (class ,(mk-class '(lp-disclosure-body)))
                  (id ,body-id))
               (div (@ (class ,(mk-class '(lp-disclosure-inner))))
                    (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                         ,@contents))))))

;; One ticket inside an epic: its status's mark and its name, a link to the
;; ticket itself.
(define (epic-ticket-row G_ project ticket statuses)
  (let ((project-human-id (assq-ref project
                                    'human-id))
        (human-id (assq-ref ticket
                            'human-id)))
    `(a (@ (class ,(mk-class '(text-sm no-underline lp-quiet py-1 px-2
                                       rounded-lg flex flex-row items-center
                                       gap-2)))
          (href ,(format #f "/tickets/~a/id/~a" project-human-id human-id)))
        ,(status-icon-node statuses (assq-ref ticket
                                               'ticket-status))
        (span ,(org2html (html-unescape (assq-ref ticket
                                                   'title)))))))

;; How far an epic has got, as a bar and a count.  An epic nobody has put a
;; ticket in says so in words instead of drawing a bar of nothing.
(define (epic-progress-node G_ done total)
  (if (zero? total)
      `(span (@ (class ,(mk-class '(text-xs text-onyx-500
                                            dark:text-stone-500
                                            shrink-0))))
             ,(G_ "empty"))
      `(span (@ (class ,(mk-class '(flex flex-row items-center gap-2
                                            shrink-0))))
             (span (@ (class ,(mk-class '(lp-epic-track)))
                      (role "img")
                      (aria-label ,(format #f "~a ~a ~a" done (G_ "of") total)))
                   (span (@ (class ,(mk-class '(lp-epic-bar)))
                            (style ,(format #f "width: ~a%"
                                            (inexact->exact
                                             (round (* 100 (/ done total)))))))))
             (small (@ (class ,(mk-class '(text-xs tabular-nums
                                                  text-onyx-500
                                                  dark:text-stone-500))))
                    ,(format #f "~a/~a" done total)))))

;;; ===== The form =====

(define* (epic-form-page G_
                         #:key project projects is-logged-in
                         (epic #f)
                         (title "")
                         (human-id "")
                         (description ""))
  "Answer the form that starts or edits one epic of PROJECT."
  (let* ((project-human-id (assq-ref project
                                     'human-id))
         (editing? (and epic #t))
         (action (if editing?
                     (format #f "/epics/~a/id/~a/edit"
                             project-human-id (assq-ref epic
                                                        'human-id))
                     (format #f "/epics/~a/new" project-human-id))))
    (html-template G_
                   #:title (if editing? (G_ "edit epic")
                                (G_ "new epic"))
                   #:canonical-path (format #f "/epics/~a" project-human-id)
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4 py-4
                                                              max-w-256
                                                              mx-auto))))
                                 (form (@ (class ,(mk-class '(flex flex-col
                                                                    gap-4)))
                                          (enctype
                                           "application/x-www-form-urlencoded")
                                          (method "POST")
                                          (action ,action))
                                       ,(input #:label (G_ "title")
                                               #:name 'title
                                               #:value title
                                               #:required #t)
                                       ,(input #:label (G_ "short name")
                                               #:name 'human-id
                                               #:value human-id
                                               #:required #t)
                                       (p (@ (class ,(mk-class '(text-sm
                                                                 text-onyx-500
                                                                 dark:text-stone-500))))
                                          ,(G_
                                            "The name this epic is named by in an address -- letters, digits and dashes -- the way a number names a ticket."))
                                       ,(textarea #:label (G_ "what it is for")
                                                  #:name 'description
                                                  #:value description
                                                  #:required #f)
                                       (div (@ (class ,(mk-class '(flex flex-row
                                                                         gap-3
                                                                         items-center
                                                                         mt-2))))
                                            ,(submit-button (G_ "save epic"))
                                            (a (@ (href ,(format #f
                                                          "/epics/~a"
                                                          project-human-id))
                                                  (class ,(mk-class '(text-sm
                                                                      text-onyx-700
                                                                      dark:text-stone-300
                                                                      underline))))
                                               ,(G_ "cancel")))))))))

;;; ===== The name of a short name =====

;; An epic is named by a slug rather than by its uuid, the way a ticket is
;; named by a number, so that an address can be read and remembered.  What a
;; form submits is text somebody typed, so it is lowered, its spaces and
;; punctuation become dashes, and what is left is what a URL can carry without
;; escaping.  A slug with nothing left in it answers #f, which is what refuses
;; the form rather than writing a row no address could ever reach.
(define-public (epic-slug text)
  "Answer the slug an epic submitted as TEXT is named by, or #f for none."
  (let* ((lowered (string-downcase (if (string? text) text "")))
         (dashed (list->string (map (lambda (c)
                                      (if (or (char-alphabetic? c)
                                              (char-numeric? c))
                                          c #\-))
                                    (string->list lowered))))
         (trimmed (string-trim-both dashed #\-)))
    (and (not (string-null? trimmed))
         (if (> (string-length trimmed) 60)
             (substring trimmed 0 60)
             trimmed))))
