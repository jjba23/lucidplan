;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views kanban)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
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
  #:use-module (lucidplan domain)
  #:use-module (app views epics)
  #:use-module (hygguile hygguile)
  #:use-module (artanis route)
  #:use-module (kracht prelude)
  #:export (render-k-ticket kanban-from-tickets))

(define* (render-k-ticket project t
                          #:key (draggable? #f))
  ;; The card is not the drag source, its grip is: a card marked `draggable'
  ;; cannot have its own text selected with the mouse.  See
  ;; `grab-indicator'.  The grip is offered only to a reader who may change a
  ;; ticket, so a board read by somebody the endpoint behind it would refuse
  ;; does not present a control that could only fail.
  `((div (@ (class ,(mk-class '(w-full)))
            (data-ticket-id ,(assq-ref t
                                       'human-id)))

         (div (@ (class ,(mk-class (append '(rounded-xl p-3
                                             border-stone-500/75
                                             dark:border-onyx-100/75
                                             border-2
                                             shadow-sm
                                             hover:shadow-md
                                             transition-shadow
                                             lp-lift)
                                           frosted-tint-classes))))
              (div (@ (class ,(mk-class '(flex flex-row justify-between
                                               items-center gap-2))))
                   (div (@ (class ,(mk-class '(flex flex-row
                                                    justify-start
                                                    items-center
                                                    gap-1
                                                    min-w-0
                                                    flex-1
                                                    whitespace-normal
                                                    leading-5))))
                        ,@(if draggable? grab-indicator
                              '())
                        ,(emoji-for-ticket-type (assq-ref t
                                                          'ticket-type))
                        (span (@ (class ,(mk-class '(text-sm font-medium
                                                             wrap-break-word
                                                             pl-1))))
                              ,(org2html (html-unescape (assq-ref t
                                                                  'title)))))

                   (a (@ (class ,(mk-class '(text-md no-underline)))
                         (href ,(format #f "/tickets/~a/id/~a" project
                                        (assq-ref t
                                                  'human-id))))
                      ((i (@ (class ,(mk-class '(ph ph-link text-onyx-500
                                                    dark:text-stone-500
                                                    hover:text-asparagus-500))))
                          ""))))))))

;; The bands a board is cut into: one for each epic that has tickets on it,
;; in the order the epics are named, and a last one for the tickets that belong
;; to none.  An epic with nothing on the board is left out -- a band of nothing
;; is a band a reader scrolls past -- but the band for no epic is always drawn
;; when it has anything in it, because that is where a board read by epic
;; starts showing what still has to be planned.
(define (lanes-of epics tickets)
  (let* ((by-epic (map (lambda (epic)
                         (cons epic
                               (filter (lambda (ticket)
                                         (equal? (format #f "~a"
                                                         (or (assq-ref ticket
                                                                       'epic-id)
                                                             ""))
                                                 (format #f "~a"
                                                         (assq-ref epic
                                                                   'id))))
                                       tickets)))
                       epics))
         (with-any (filter (lambda (lane)
                             (pair? (cdr lane)))
                           by-epic))
         (none (filter (lambda (ticket)
                         (let ((id (format #f "~a"
                                           (or (assq-ref ticket
                                                         'epic-id) ""))))
                           (not (any (lambda (epic)
                                       (equal? id
                                               (format #f "~a"
                                                       (assq-ref epic
                                                                 'id))))
                                     epics))))
                       tickets)))
    (append with-any
            (if (pair? none)
                (list (cons #f none))
                '()))))

(define* (kanban-from-tickets G_
                              #:key projects
                              project
                              tickets
                              ticket-id-to-human-id
                              is-logged-in
                              (can-drag? #f)
                              (statuses (default-ticket-statuses))
                              ;; The board can be cut into swimlanes: one
                              ;; band per epic, each a board of its own, with
                              ;; the tickets that belong to no epic in the
                              ;; last band.  `lanes?' is whether the reader
                              ;; asked for that, and it is a link rather than
                              ;; a stored preference -- a board is read one
                              ;; way or the other at a time.
                              (lanes? #f)
                              (epics '())
                              (current-sort #f))

  (let* ( ;Whether a card offers its drag grip is the same question the

         ;; endpoints that move and restatus a ticket answer, so the board
         ;; shows the grip to exactly the readers those endpoints accept.
         (draggable? can-drag?)
         (tickets-with-status (lambda (status)
                                (filter (lambda (x)
                                          (string=? status
                                                    (assq-ref x
                                                              'ticket-status)))
                                        tickets)))
         ;; Which statuses the board is drawn from is the page's ladder: the
         ;; open ones are the columns, in the ladder's order, and the closed
         ;; ones are the sections under the board.  A status an instance adds
         ;; therefore gets a column (or a section) of its own without the
         ;; board being told about it.
         (open-statuses (ticket-status-values-in-bucket statuses "open"))
         (done-statuses (ticket-status-values-in-bucket statuses "done"))
         (rejected-statuses (ticket-status-values-in-bucket statuses
                                                            "rejected"))

         ;; Consolidate CSS Faces
         (kanban-wrap-face (mk-class '(flex flex-col gap-3 grow)))
         (toolbar-face (mk-class '(flex flex-col
                                        sm:flex-row
                                        gap-3
                                        items-stretch
                                        sm:items-center
                                        justify-between)))
         (header-face (mk-class '(flex flex-row gap-2 items-center min-w-0)))
         ;; `shrink-0' keeps the chosen sort readable: alongside a long
         ;; project name the toolbar would otherwise squeeze the control
         ;; below the width of its own label.
         (sort-wrap-face (mk-class '(my-3 px-1
                                          py-3
                                          flex
                                          flex-row
                                          flex-wrap
                                          shrink-0
                                          justify-start
                                          items-center)))
         (btn-row-face (mk-class '(flex flex-row flex-wrap gap-2 items-center)))

         (board-face (mk-class '(grid grid-cols-1
                                      md:grid-cols-2
                                      lg:grid-cols-3
                                      gap-4
                                      items-start
                                      px-1
                                      py-3
                                      pb-4)))
         (std-col-face (mk-class (append '(flex flex-col
                                                gap-2
                                                w-full
                                                min-h-24
                                                border-stone-500/25
                                                dark:border-onyx-100/25
                                                border-2
                                                rounded-xl
                                                py-2
                                                sm:py-3
                                                my-2
                                                px-1
                                                sm:px-2) frosted-tint-classes)))

         (col-title-face (mk-class '(text-md font-bold
                                             flex
                                             flex-row
                                             flex-wrap
                                             gap-2
                                             items-center)))
         (count-face (mk-class '(text-md text-onyx-700 dark:text-stone-300
                                         font-light)))
         (summary-face (mk-class '(marker:block hover:cursor-pointer
                                                items-center)))
         (done-wrap-face (mk-class '(my-8 flex
                                          flex-col
                                          gap-2
                                          ticket-drop-zone
                                          min-h-24
                                          p-2
                                          rounded-lg
                                          transition-colors)))

         (kanban-icon-face '(text-onyx-700 dark:text-stone-300 text-2xl))
         (btn-icon-face '(text-md text-onyx-700 dark:text-stone-300))

         ;; Helper for the sort options
         (make-sort-opt (lambda (s)
                          (option #:value (format #f "/kanban/~a?sort=~a"
                                                  project
                                                  (car s))
                                  #:content (cdr s)
                                  #:selected-value (format #f
                                                    "/kanban/~a?sort=~a"
                                                    project current-sort))))

         ;; Column SXML Generators
         (build-col (lambda (status title list face)
                      `((div (@ (class ,face))
                             (div (@ (class ,col-title-face))
                                  ,(status-icon-node statuses status)
                                  ,(sub-h3 title)
                                  (span (@ (class ,count-face))
                                        ,(format #f " ( ~a ) "
                                                 (length list))))

                             ;; Drop zone container.  The zone answers the
                             ;; drag on the way in as well as on the way
                             ;; across it: a drop target is the element whose
                             ;; last `dragenter' or `dragover' was answered,
                             ;; and Chromium reads the release against that
                             ;; one element, so a card reached over a gap
                             ;; would otherwise never be a place to drop on.
                             (div (@ (class ,(mk-class '(ticket-drop-zone flex
                                                         flex-col
                                                         gap-2
                                                         min-h-24
                                                         p-2
                                                         rounded-lg
                                                         transition-colors)))
                                     (data-status ,status)
                                     (ondragenter "acceptDrag(event)")
                                     (ondragover "handleKanbanDragOver(event)")
                                     (ondragleave
                                      "handleKanbanDragLeave(event)")
                                     (ondrop ,(format #f
                                               "handleKanbanDrop(event, '~a')"
                                               project)))
                                  ,(map (lambda (t)
                                          (render-k-ticket project t
                                                           #:draggable?
                                                           draggable?)) list))))))

         (build-details-col (lambda (status title list)
                              `(div (@ (class ,std-col-face))
                                    (details (@ (class ,(mk-class '(cursor-pointer))))
                                             (summary (@ (class ,summary-face))
                                                      (div (@ (class ,col-title-face))
                                                           ,(status-icon-node
                                                             statuses status)
                                                           ,(sub-h3 title)
                                                           (span (@ (class ,count-face))
                                                                 ,(format #f
                                                                   " ( ~a ) "
                                                                   (length
                                                                    list)))))
                                             (div (@ (class ,done-wrap-face)
                                                     (data-status ,status)
                                                     (ondragenter
                                                      "acceptDrag(event)")
                                                     (ondragover
                                                      "handleKanbanDragOver(event)")
                                                     (ondragleave
                                                      "handleKanbanDragLeave(event)")
                                                     (ondrop ,(format #f
                                                               "handleKanbanDrop(event, '~a')"
                                                               project)))
                                                  ,@(map (lambda (t)
                                                           (render-k-ticket
                                                            project t
                                                            #:draggable?
                                                            draggable?)) list))))))

         ;; A board, drawn from whichever tickets it is handed.  The plain
         ;; board is this called with the whole backlog; a swimlane is this
         ;; called with the tickets of one epic.  Making it a function of the
         ;; tickets is what keeps the columns, the drag handlers and the
         ;; status ladder the same in both.
         (board-of (lambda (lane-tickets)
                     `(div (@ (class ,board-face))
                           ,@(map (lambda (status)
                                    (build-col status
                                               (status-label G_ statuses status)
                                               (filter (lambda (x)
                                                         (string=? status
                                                                   (assq-ref
                                                                    x
                                                                    'ticket-status)))
                                                       lane-tickets)
                                               std-col-face))
                                  open-statuses))))

         ;; Build Main HTML
         (kanban-html `((div (@ (class ,(mk-class '(px-2 py-4))))
                             (div (@ (class ,kanban-wrap-face))
                                  ;; Top Toolbar Area
                                  (div (@ (class ,toolbar-face))

                                       (div (@ (class ,header-face))
                                            ,(icon #:type 'kanban
                                                   #:icon-css kanban-icon-face)
                                            ;; The toolbar shares its row
                                            ;; with the sort control and two
                                            ;; buttons, so the board's name
                                            ;; is set smaller until there is
                                            ;; room for the display size.
                                            ,(h1 (format #f "kanban for ~a"
                                                         project)
                                                 #:size '(text-2xl lg:text-4xl)))

                                       (div (@ (class ,sort-wrap-face))
                                            ,(sort-select G_
                                                          #:project-id (assq-ref
                                                                        project
                                                                        'human-id)
                                                          #:current-sort
                                                          current-sort
                                                          #:option-maker
                                                          make-sort-opt))

                                       (div (@ (class ,btn-row-face))
                                            (a (@ (href ,(format #f
                                                          "/tickets/~a"
                                                          project)))
                                               ,(icon-button #:icon 'scroll
                                                             #:content (G_
                                                                        "backlog")
                                                             #:icon-css
                                                             btn-icon-face))
                                            ;; The plan beside the board: the
                                            ;; sprints the work is committed
                                            ;; to and the milestones it counts
                                            ;; toward, which are the two pages
                                            ;; a board is read against.
                                            (a (@ (href ,(format #f
                                                          "/projects/~a/sprints"
                                                          project)))
                                               ,(icon-button #:icon 'calendar-check
                                                             #:content (G_
                                                                        "sprints")
                                                             #:icon-css
                                                             btn-icon-face))
                                            (a (@ (href ,(format #f
                                                          "/projects/~a/milestones"
                                                          project)))
                                               ,(icon-button #:icon 'target
                                                             #:content (G_
                                                                        "milestones")
                                                             #:icon-css
                                                             btn-icon-face))
                                            ;; The board cut one way or the
                                            ;; other: the same page, and the
                                            ;; link says which way it is not
                                            ;; being read just now.
                                            ,(quiet-link #:icon 'stack
                                                         #:label (if lanes?
                                                                     (G_ "one board")
                                                                     (G_ "swimlanes by epic"))
                                                         #:href (if lanes?
                                                                    (format #f
                                                                            "/kanban/~a"
                                                                            project)
                                                                    (format #f
                                                                            "/kanban/~a?lanes=epic"
                                                                            project)))
                                            (a (@ (href ,(format #f
                                                          "/tickets/~a/new"
                                                          project)))
                                               ,(icon-button #:icon 'plus
                                                             #:content (G_
                                                                        "new ticket")
                                                             #:icon-css
                                                             btn-icon-face)))))

                             ;; Main Board, or one board per epic.
                             ;;
                             ;; A swimlane is a way of reading the same board
                             ;; through one epic at a time: the columns stay
                             ;; the project's ladder, and what changes is
                             ;; which tickets each column is drawn from.  The
                             ;; tickets in no epic come last, because a board
                             ;; is read for what has been planned before it is
                             ;; read for what has not.
                             ,@(if lanes?
                                   (map (lambda (lane)
                                          `(div (@ (class ,(mk-class '(flex
                                                                       flex-col
                                                                       gap-1
                                                                       py-2))))
                                                (div (@ (class ,(mk-class '(flex
                                                                            flex-row
                                                                            flex-wrap
                                                                            items-center
                                                                            gap-2
                                                                            px-1))))
                                                     ,@(if (car lane)
                                                           (list (epic-chip
                                                                  (car lane)))
                                                           (list `(span (@ (class ,
                                                                           (mk-class
                                                                            '(text-sm
                                                                              font-medium
                                                                              text-onyx-700
                                                                              dark:text-stone-300))))
                                                                        ,(G_ "no epic"))))
                                                     (span (@ (class ,(mk-class '(text-xs
                                                                                  text-onyx-500
                                                                                  dark:text-stone-500))))
                                                           ,(format #f "~a ~a"
                                                                    (length
                                                                     (cdr lane))
                                                                    (G_ "tickets"))))
                                                ,(board-of (cdr lane))))
                                        (lanes-of epics tickets))
                                   (list (board-of tickets)))

                             )
                        ;; The closed statuses sit under the board, one
                        ;; section each, in the ladder's order.
                        (div (@ (class ,(mk-class '(px-1))))
                             ,@(map (lambda (status)
                                      (build-details-col status
                                                         (status-label G_
                                                          statuses status)
                                                         (tickets-with-status
                                                          status)))
                                    (append done-statuses rejected-statuses))))))

    ;; Render final template
    (html-template G_
                   #:title (format #f "kanban for ~a" project)
                   #:canonical-path (format #f "/kanban/~a" project)
                   #:is-logged-in is-logged-in
                   #:body kanban-html
                   #:proj projects)))
