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
                        ,(p (org2html (assoc-ref t "title"))))

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
  
  (let* ((tickets-todo (filter (lambda (x)
                                 (eq? 'todo
                                      (string->symbol (assoc-ref x
                                                       "ticket_status"))))
                               tickets))
         (tickets-work-in-progress (filter (lambda (x)
                                             (eq? 'work-in-progress
                                                  (string->symbol (assoc-ref x
                                                                   "ticket_status"))))
                                           tickets))
         (tickets-in-review (filter (lambda (x)
                                      (eq? 'in-review
                                           (string->symbol (assoc-ref x
                                                            "ticket_status"))))
                                    tickets))
         (tickets-rejected (filter (lambda (x)
                                     (eq? 'rejected
                                          (string->symbol (assoc-ref x
                                                           "ticket_status"))))
                                   tickets))
         (tickets-done (filter (lambda (x)
                                 (eq? 'done
                                      (string->symbol (assoc-ref x
                                                       "ticket_status"))))
                               tickets))

         (kanban-html `((div (@ (class ,(mk-class '(px-2 py-4))))
                             ((div (@ (class ,(mk-class '(flex flex-col gap-3
                                                               flex-grow))))
                                   (div (@ (class ,(mk-class '(flex flex-wrap
                                                               flex-row gap-4
                                                               items-center
                                                               justify-between))))
                                        (div (@ (class ,(mk-class '(flex
                                                                    flex-row
                                                                    gap-2
                                                                    items-center))))
                                             ,(icon #:type 'kanban
                                                    #:icon-css '(text-onyx-700
                                                                 dark:text-stone-300
                                                                 text-2xl))
                                             ,(h1 (format #f "kanban for ~a"
                                                          project)))

                                        (div (@ (class ,(mk-class '(my-3 px-1
                                                                    py-3
                                                                    flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    justify-start
                                                                    align-center
                                                                    items-center))))
                                             ,(sort-select G_
                                                           #:project-id (assoc-ref
                                                                         project
                                                                         "human_id")
                                                           #:current-sort
                                                           current-sort
                                                           #:option-maker (lambda 
                                                                                  (s)
                                                                            
                                                                            (option
                                                                             #:value
                                                                             (format
                                                                              #f
                                                                              "/kanban/~a?sort=~a"
                                                                              project

                                                                              
                                                                              (car
                                                                               s))
                                                                             #:content
                                                                             (cdr
                                                                              s)
                                                                             #:selected-value
                                                                             (format
                                                                              #f
                                                                              "/kanban/~a?sort=~a"
                                                                              project
                                                                              current-sort)))))

                                        (div (@ (class ,(mk-class '(flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    gap-1
                                                                    items-center))))
                                             (a (@ (href ,(format #f
                                                           "/tickets/~a"
                                                           project)))
                                                ,(icon-button #:icon 'scroll
                                                              #:content (G_
                                                                         "backlog")
                                                              #:icon-css '(text-md
                                                                           text-onyx-700
                                                                           dark:text-stone-300)))
                                             (a (@ (href ,(format #f
                                                           "/tickets/~a/new"
                                                           project)))
                                                ,(icon-button #:icon 'plus
                                                              #:content (G_
                                                                         "new ticket")
                                                              #:icon-css '(text-md
                                                                           text-onyx-700
                                                                           dark:text-stone-300)))))

                                   )))
                        (div (@ (class ,(mk-class '(flex flex-wrap gap-8
                                                         justify-center px-1
                                                         py-4))))
                             (div (@ (class ,(mk-class '(w-full sm:w-1/2
                                                                md:w-1/3
                                                                lg:w-1/4
                                                                flex
                                                                flex-col
                                                                gap-2))))
                                  (div (@ (class ,(mk-class '(text-md
                                                              font-bold
                                                              flex
                                                              flex-row
                                                              flex-wrap
                                                              gap-2
                                                              items-center))))
                                       ,(emoji-for-ticket-status "todo")
                                       ,(sub-h3 (G_ "To-do"))
                                       (span (@ (class ,(mk-class '(text-md
                                                                    text-onyx-700
                                                                    dark:text-stone-300
                                                                    font-light))))
                                             ,(format #f " ( ~a ) "
                                                      (length tickets-todo))))
                                  ,(map (lambda (t)
                                          (render-k-ticket project t))
                                        tickets-todo))
                             (div (@ (class ,(mk-class '(sm:w-1/3 flex
                                                                  flex-col
                                                                  gap-2))))
                                  (div (@ (class ,(mk-class '(text-md
                                                              font-bold
                                                              flex
                                                              flex-row
                                                              flex-wrap
                                                              gap-2
                                                              items-center))))
                                       ,(emoji-for-ticket-status
                                         "work-in-progress")
                                       ,(sub-h3 (G_ "Work in Progress"))
                                       (span (@ (class ,(mk-class '(text-md
                                                                    text-onyx-700
                                                                    dark:text-stone-300
                                                                    font-light))))
                                             ,(format #f " ( ~a ) "
                                                      (length
                                                       tickets-work-in-progress))))
                                  ,(map (lambda (t)
                                          (render-k-ticket project t))
                                        tickets-work-in-progress))
                             (div (@ (class ,(mk-class '(sm:w-1/3 flex
                                                                  flex-col
                                                                  gap-2))))
                                  (div (@ (class ,(mk-class '(text-md
                                                              font-bold
                                                              flex
                                                              flex-row
                                                              flex-wrap
                                                              gap-2
                                                              items-center))))
                                       ,(emoji-for-ticket-status "in-review")
                                       ,(sub-h3 (G_ "In Review"))
                                       (span (@ (class ,(mk-class '(text-md
                                                                    text-onyx-700
                                                                    dark:text-stone-300
                                                                    font-light))))
                                             ,(format #f " ( ~a ) "
                                                      (length
                                                       tickets-in-review))))
                                  ,(map (lambda (t)
                                          (render-k-ticket project t))
                                        tickets-in-review)))
                        (hr)
                        (div (@ (class ,(mk-class '(sm:w-1/3 my-6 p-4 flex
                                                             flex-col gap-2))))
                             (details (@ (class ,(mk-class '(cursor-pointer))))
                                      (summary (@ (class ,(mk-class '(marker:block
                                                                      hover:cursor-pointer
                                                                      items-center))))
                                               (div (@ (class ,(mk-class '(text-md
                                                                           font-bold
                                                                           flex
                                                                           flex-row
                                                                           flex-wrap
                                                                           gap-2
                                                                           items-center))))
                                                    ,(emoji-for-ticket-status
                                                      "done")
                                                    ,(sub-h3 (G_ "Done"))
                                                    (span (@ (class ,(mk-class '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300
                                                                       font-light))))
                                                          ,(format #f
                                                                   " ( ~a ) "
                                                                   (length
                                                                    tickets-done)))))
                                      (div (@ (class ,(mk-class '(my-8 flex
                                                                  flex-col
                                                                  gap-2))))
                                           ,(map (lambda (t)
                                                   (render-k-ticket project t))
                                                 tickets-done))))

                        (hr)
                        (div (@ (class ,(mk-class '(sm:w-1/3 my-6 p-4 flex
                                                             flex-col gap-2))))
                             (details (@ (class ,(mk-class '(cursor-pointer))))
                                      (summary (@ (class ,(mk-class '(marker:block
                                                                      hover:cursor-pointer
                                                                      items-center))))
                                               (div (@ (class ,(mk-class '(text-md
                                                                           font-bold
                                                                           flex
                                                                           flex-row
                                                                           flex-wrap
                                                                           gap-2
                                                                           items-center))))
                                                    ,(emoji-for-ticket-status
                                                      "rejected")
                                                    ,(sub-h3 (G_ "Rejected"))
                                                    (span (@ (class ,(mk-class '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300
                                                                       font-light))))
                                                          ,(format #f
                                                                   " ( ~a ) "
                                                                   (length
                                                                    tickets-rejected)))))
                                      (div (@ (class ,(mk-class '(my-8 flex
                                                                  flex-col
                                                                  gap-2))))
                                           ,(map (lambda (t)
                                                   (render-k-ticket project t))
                                                 tickets-rejected)))))))
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body kanban-html
                   #:proj projects)))

