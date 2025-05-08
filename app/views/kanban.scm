;;; lucidplan - free and open project management for everyone

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
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
  #:use-module (orgwebalchemy org-to-html)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan html)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components button)
  #:use-module (hygguile components typography)
  #:use-module (hygguile components icon)
  #:use-module (artanis route))

(define (render-k-ticket project t)
  `((div (@ (class "w-auto"))
         (div (@ (class ,(->css '(rounded-xl bg-stone-200/25 p-3
                                             border-stone-400/75 border-2
                                             shadow))))
              (div (@ (class ,(->css '(flex flex-row justify-between
                                            items-center gap-2))))
                   (div (@ (class ,(->css '(flex flex-row justify-left
                                                 items-center gap-2))))
                        ,(emoji-for-ticket-type (assoc-ref t "ticket_type"))
                        (p (@ (class ,(->css '(text-sm text-stone-700 flex
                                                       flex-row gap-2))))
                           (span ,(org-to-html (assoc-ref t "title")))))

                   (a (@ (class ,(->css '(text-xs)))
                         (href ,(format #f "/tickets/~a/~a" project
                                        (assoc-ref t "human_id"))))
                      ((i (@ (class ,(->css '(ph ph-link text-stone-600)))) ""))))))))

(begin
  (define* (kanban-from-tickets G_ projects project tickets
                                ticket-id-to-human-id)
    
    (let* ((tickets-todo (filter (lambda (x)
                                   (eq? 'todo
                                        (string->symbol (assoc-ref x
                                                         "ticket_status"))))
                                 tickets))
           (tickets-work-in-progress (filter (lambda (x)
                                               (eq? 'work-in-progress
                                                    (string->symbol (assoc-ref
                                                                     x
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

           (kanban-html `((div (@ (class ,(->css '(container mx-auto))))
                               ((div (@ (class ,(->css '(flex flex-col gap-3
                                                              flex-grow))))
                                     (div (@ (class ,(->css '(flex flex-wrap
                                                              flex-row gap-4
                                                              items-center
                                                              justify-between))))
                                          (div (@ (class ,(->css '(flex
                                                                   flex-row
                                                                   gap-2
                                                                   items-center))))
                                               ,(hyg-icon #:type 'kanban
                                                          #:icon-css '(text-stone-700
                                                                       text-2xl))
                                               ,(hyg-h1 (format #f
                                                         "kanban for ~a"
                                                         project)))

                                          (div (@ (class ,(->css '(flex
                                                                   flex-row
                                                                   flex-wrap
                                                                   gap-1
                                                                   items-center))))
                                               (a (@ (href ,(format #f
                                                             "/tickets/~a"
                                                             project)))
                                                  ,(hyg-icon-button #:icon 'scroll
                                                                    #:content (G_
                                                                               "backlog")))
                                               (a (@ (href ,(format #f
                                                             "/tickets/~a/new"
                                                             project)))
                                                  ,(hyg-icon-button #:icon 'plus
                                                                    #:content (G_
                                                                               "new ticket")))))

                                     )))
                          (div (@ (class ,(->css '(flex flex-wrap my-8
                                                        justify-between p-4))))
                               (div (@ (class ,(->css '(sm:w-1/3 flex flex-col
                                                                 gap-2 p-2))))
                                    (h4 (@ (class ,(->css '(text-sm font-bold
                                                            flex gap-2
                                                            items-center))))
                                        ,(emoji-for-ticket-status "todo")
                                        (span ,(G_ "To-do"))
                                        (span (@ (class ,(->css '(text-xs
                                                                  text-stone-700
                                                                  font-light))))
                                              ,(format #f " ( ~a ) "
                                                       (length tickets-todo))))
                                    ,(map (lambda (t)
                                            (render-k-ticket project t))
                                          tickets-todo))
                               (div (@ (class ,(->css '(sm:w-1/3 flex flex-col
                                                                 gap-2 p-2))))
                                    (h4 (@ (class ,(->css '(text-sm font-bold
                                                            flex gap-2
                                                            items-center))))
                                        ,(emoji-for-ticket-status
                                          "work-in-progress")
                                        (span ,(G_ "Work in Progress"))
                                        (span (@ (class ,(->css '(text-xs
                                                                  text-stone-700
                                                                  font-light))))
                                              ,(format #f " ( ~a ) "
                                                       (length
                                                        tickets-work-in-progress))))
                                    ,(map (lambda (t)
                                            (render-k-ticket project t))
                                          tickets-work-in-progress))
                               (div (@ (class ,(->css '(sm:w-1/3 flex flex-col
                                                                 gap-2 p-2))))
                                    (h4 (@ (class ,(->css '(text-sm font-bold
                                                            flex gap-2
                                                            items-center))))
                                        ,(emoji-for-ticket-status "in-review")
                                        (span ,(G_ "In Review"))
                                        (span (@ (class ,(->css '(text-xs
                                                                  text-stone-700
                                                                  font-light))))
                                              ,(format #f " ( ~a ) "
                                                       (length
                                                        tickets-in-review))))
                                    ,(map (lambda (t)
                                            (render-k-ticket project t))
                                          tickets-in-review)))
                          (hr)
                          (div (@ (class ,(->css '(sm:w-1/3 my-6
                                                            p-4
                                                            flex
                                                            flex-col
                                                            gap-2
                                                            p-2))))
                               (details (@ (class ,(->css '(cursor-pointer))))
                                        (summary (@ (class ,(->css '(marker:block
                                                                     hover:cursor-pointer
                                                                     items-center))))
                                                 (h4 (@ (class ,(->css '(text-sm
                                                                         font-bold
                                                                         flex
                                                                         gap-2
                                                                         items-center))))
                                                     ,(emoji-for-ticket-status
                                                       "done")
                                                     (span ,(G_ "Done"))
                                                     (span (@ (class ,(->css '
                                                                             (text-xs
                                                                              text-stone-700
                                                                              font-light))))
                                                           ,(format #f
                                                             " ( ~a ) "
                                                             (length
                                                              tickets-done)))))
                                        (div (@ (class
                                                 "my-8 flex flex-col gap-2"))
                                             ,(map (lambda (t)
                                                     (render-k-ticket project
                                                                      t))
                                                   tickets-done))))

                          (hr)
                          (div (@ (class ,(->css '(sm:w-1/3 my-6
                                                            p-4
                                                            flex
                                                            flex-col
                                                            gap-2
                                                            p-2))))
                               (details (@ (class "cursor-pointer"))
                                        (summary (@ (class ,(->css '(marker:block
                                                                     hover:cursor-pointer
                                                                     items-center))))
                                                 (h4 (@ (class ,(->css '(text-sm
                                                                         font-bold
                                                                         flex
                                                                         gap-2
                                                                         items-center))))
                                                     ,(emoji-for-ticket-status
                                                       "rejected")
                                                     (span ,(G_ "Rejected"))
                                                     (span (@ (class ,(->css '
                                                                             (text-xs
                                                                              text-stone-700
                                                                              font-light))))
                                                           ,(format #f
                                                             " ( ~a ) "
                                                             (length
                                                              tickets-rejected)))))
                                        (div (@ (class ,(->css '(my-8 flex
                                                                 flex-col
                                                                 gap-2))))
                                             ,(map (lambda (t)
                                                     (render-k-ticket project
                                                                      t))
                                                   tickets-rejected)))))))
      (html-template G_
                     #:body kanban-html
                     #:proj projects)))
  (export kanban-from-tickets))

