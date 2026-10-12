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
  #:use-module (hygguile hygguile)
  #:use-module (artanis route)
  #:use-module (kracht prelude)
  #:export (render-k-ticket kanban-from-tickets))

(define (render-k-ticket project t)
  ;; The card is not the drag source, its grip is: a card marked `draggable'
  ;; cannot have its own text selected with the mouse.  See
  ;; `grab-indicator'.
  `((div (@ (class ,(mk-class '(w-full)))
            (data-ticket-id ,(assq-ref t
                                       'human-id)))

         (div (@ (class ,(mk-class (append '(rounded-xl
                                             p-3
                                             border-stone-500/75
                                             dark:border-onyx-100/75
                                             border-2
                                             shadow-sm
                                             hover:shadow-md
                                             transition-shadow)
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
                        ,grab-indicator
                        ,(emoji-for-ticket-type (assq-ref t
                                                          'ticket-type))
                        (span (@ (class ,(mk-class '(text-sm font-medium
                                                             wrap-break-word pl-1))))
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

(define* (kanban-from-tickets G_
                              #:key projects
                              project
                              tickets
                              ticket-id-to-human-id
                              is-logged-in
                              (current-sort #f))

  (let* ((get-tickets (lambda (status-str)
                        (filter (lambda (x)
                                  (string=? status-str
                                            (assq-ref x
                                                      'ticket-status)))
                                tickets)))

         (tickets-todo (get-tickets "todo"))
         (tickets-wip (get-tickets "work-in-progress"))
         (tickets-rev (get-tickets "in-review"))
         (tickets-done (get-tickets "done"))
         (tickets-rej (get-tickets "rejected"))

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
                                           sm:px-2)
                                         frosted-tint-classes)))

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
                                  ,(emoji-for-ticket-status status)
                                  ,(sub-h3 (G_ title))
                                  (span (@ (class ,count-face))
                                        ,(format #f " ( ~a ) "
                                                 (length list))))

                             ;; Drop zone container
                             (div (@ (class ,(mk-class '(ticket-drop-zone flex
                                                         flex-col
                                                         gap-2
                                                         min-h-24
                                                         p-2
                                                         rounded-lg
                                                         transition-colors)))
                                     (data-status ,status)
                                     (ondragover "handleKanbanDragOver(event)")
                                     (ondragleave
                                      "handleKanbanDragLeave(event)")
                                     (ondrop ,(format #f
                                               "handleKanbanDrop(event, '~a')"
                                               project)))
                                  ,(map (lambda (t)
                                          (render-k-ticket project t)) list))))))

         (build-details-col (lambda (status title list)
                              `(div (@ (class ,std-col-face))
                                    (details (@ (class ,(mk-class '(cursor-pointer))))
                                             (summary (@ (class ,summary-face))
                                                      (div (@ (class ,col-title-face))
                                                           ,(emoji-for-ticket-status
                                                             status)
                                                           ,(sub-h3 (G_ title))
                                                           (span (@ (class ,count-face))
                                                                 ,(format #f
                                                                   " ( ~a ) "
                                                                   (length
                                                                    list)))))
                                             (div (@ (class ,done-wrap-face)
                                                     (data-status ,status)
                                                     (ondragover
                                                      "handleKanbanDragOver(event)")
                                                     (ondragleave
                                                      "handleKanbanDragLeave(event)")
                                                     (ondrop ,(format #f
                                                               "handleKanbanDrop(event, '~a')"
                                                               project)))
                                                  ,@(map (lambda (t)
                                                           (render-k-ticket
                                                            project t)) list))))))

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
                                                 #:size '(text-2xl
                                                          lg:text-4xl)))

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
                                            (a (@ (href ,(format #f
                                                          "/tickets/~a/new"
                                                          project)))
                                               ,(icon-button #:icon 'plus
                                                             #:content (G_
                                                                        "new ticket")
                                                             #:icon-css
                                                             btn-icon-face)))))

                             ;; Main Board
                             (div (@ (class ,board-face))
                                  ,(build-col "todo" "To-do" tickets-todo
                                              std-col-face)
                                  ,(build-col "work-in-progress"
                                              "Work in Progress" tickets-wip
                                              std-col-face)
                                  ,(build-col "in-review" "In Review"
                                              tickets-rev std-col-face))

                             )
                        (div (@ (class ,(mk-class '(px-1))))
                             ,(build-details-col "done" "Done" tickets-done)

                             ,(build-details-col "rejected" "Rejected"
                                                 tickets-rej)))))

    ;; Render final template
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body kanban-html
                   #:proj projects)))
