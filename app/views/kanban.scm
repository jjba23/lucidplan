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
  #:export (render-k-ticket kanban-from-tickets))

(define (render-k-ticket project t)
  `((div (@ (class ,(mk-class '(max-w-128))))
         (div (@ (class ,(mk-class '(rounded-xl bg-stone-300/25
                                                dark:bg-onyx-700/50
                                                p-3
                                                border-stone-500/75
                                                dark:border-onyx-100/75
                                                border-2
                                                shadow))))
              (div (@ (class ,(mk-class '(flex flex-row justify-between
                                               items-center gap-2))))
                   (div (@ (class ,(mk-class '(flex flex-row justify-left
                                                    items-center gap-2))))
                        ,(emoji-for-ticket-type (assoc-ref t "ticket_type"))
                        ,(p (org2html (assoc-ref t "title"))
                            #:size '(text-md leading-7)))

                   (a (@ (class ,(mk-class '(text-md no-underline)))
                         (href ,(format #f "/tickets/~a/id/~a" project
                                        (assoc-ref t "human_id"))))
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
  
  (let* ( ;Helper to DRY up ticket filtering
          (get-tickets (lambda (status-str)
                         (filter (lambda (x)
                                   (string=? status-str
                                             (assoc-ref x "ticket_status")))
                                 tickets)))

         (tickets-todo (get-tickets "todo"))
         (tickets-wip (get-tickets "work-in-progress"))
         (tickets-rev (get-tickets "in-review"))
         (tickets-done (get-tickets "done"))
         (tickets-rej (get-tickets "rejected"))

         ;; Consolidate CSS Faces
         (kanban-wrap-face (mk-class '(flex flex-col gap-3 flex-grow)))
         (toolbar-face (mk-class '(flex flex-wrap flex-row gap-4 items-center
                                        justify-between)))
         (header-face (mk-class '(flex flex-row gap-2 items-center)))
         (sort-wrap-face (mk-class '(my-3 px-1
                                          py-3
                                          flex
                                          flex-row
                                          flex-wrap
                                          justify-start
                                          items-center)))
         (btn-row-face (mk-class '(flex flex-row flex-wrap gap-1 items-center)))

         (board-face (mk-class '(flex flex-wrap gap-8 justify-center px-1 py-4)))
         (todo-col-face (mk-class '(w-full sm:w-1/2
                                           md:w-1/3
                                           lg:w-1/4
                                           flex
                                           flex-col
                                           gap-2)))
         (std-col-face (mk-class '(sm:w-1/3 flex flex-col gap-2)))
         (details-col-face (mk-class '(sm:w-1/3 my-6 p-4 flex flex-col gap-2)))

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
         (done-wrap-face (mk-class '(my-8 flex flex-col gap-2)))

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
                      `(div (@ (class ,face))
                            (div (@ (class ,col-title-face))
                                 ,(emoji-for-ticket-status status)
                                 ,(sub-h3 (G_ title))
                                 (span (@ (class ,count-face))
                                       ,(format #f " ( ~a ) "
                                                (length list))))
                            ,(map (lambda (t)
                                    (render-k-ticket project t)) list))))

         (build-details-col (lambda (status title list)
                              `(div (@ (class ,details-col-face))
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
                                             (div (@ (class ,done-wrap-face))
                                                  ,(map (lambda (t)
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
                                            ,(h1 (format #f "kanban for ~a"
                                                         project)))

                                       (div (@ (class ,sort-wrap-face))
                                            ,(sort-select G_
                                                          #:project-id (assoc-ref
                                                                        project
                                                                        "human_id")
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
                                              todo-col-face)
                                  ,(build-col "work-in-progress"
                                              "Work in Progress" tickets-wip
                                              std-col-face)
                                  ,(build-col "in-review" "In Review"
                                              tickets-rev std-col-face))

                             ,(hr)
                             ,(build-details-col "done" "Done" tickets-done)

                             ,(hr)
                             ,(build-details-col "rejected" "Rejected"
                                                 tickets-rej)))))
    
    ;; Render final template
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body kanban-html
                   #:proj projects)))
