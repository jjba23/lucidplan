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
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan html)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components button)
  #:use-module (lucidplan prelude)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define render-k-ticket
  (lambda (t)
    `((div (@ (class "w-auto"))
           (div (@ (class
                    "rounded-xl bg-stone-200/50 p-3 border-stone-400/75 border-2"))
                (p (@ (class "text-xs text-stone-700 flex flex-row gap-2"))
                   (span ,(emoji-for-ticket-type (assoc-ref t "ticket_type")))
                   (span ,(assoc-ref t "title"))))))))

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
           (show-rejected #f)
           (show-done #f)
           (_ (log-exprs (list (list? tickets-todo) tickets-todo)))
           (kanban-html `((div (@ (class "container mx-auto"))
                               ((div (@ (class "flex flex-col gap-3 flex-grow"))
                                     (div (@ (class
                                              "flex flex-wrap flex-row gap-6 items-center"))
                                          (h2 (@ (class
                                                  "text-2xl font-sans font-bold text-stone-600"))
                                              ,(format #f "kanban for ~a"
                                                       project))
                                          (a (@ (class
                                                 "text-stone-600 text-md flex gap-1")
                                                (href ,(format #f
                                                               "/tickets/~a"
                                                               project)))
                                             (span "📚")
                                             (span ,(G_ "backlog"))))
                                     (div (a (@ (class
                                                 "text-sm text-stone-800 my-3")
                                                (href ,(format #f
                                                        "/tickets/~a/new"
                                                        project)))
                                             "➕ new ticket"))

                                     )))
                          (div (@ (class
                                   "flex flex-wrap my-8 justify-between p-4"))
                               (div (@ (class
                                        "sm:w-1/3 flex flex-col gap-2 p-2"))
                                    (h4 (@ (class
                                            "text-sm font-bold flex gap-2 items-center"))
                                        (span "☑️")
                                        (span ,(G_ "To-do"))
                                        (span (@ (class
                                                  "text-xs text-stone-700 font-light"))
                                              ,(format #f " ( ~a ) "
                                                       (length tickets-todo))))
                                    ,(map render-k-ticket tickets-todo))
                               (div (@ (class
                                        "sm:w-1/3 flex flex-col gap-2 p-2"))
                                    (h4 (@ (class
                                            "text-sm font-bold flex gap-2 items-center"))
                                        (span "⚙️")
                                        (span ,(G_ "Work in Progress"))
                                        (span (@ (class
                                                  "text-xs text-stone-700 font-light"))
                                              ,(format #f " ( ~a ) "
                                                       (length
                                                        tickets-work-in-progress))))
                                    ,(map render-k-ticket
                                          tickets-work-in-progress))
                               (div (@ (class
                                        "sm:w-1/3 flex flex-col gap-2 p-2"))
                                    (h4 (@ (class
                                            "text-sm font-bold flex gap-2 items-center"))
                                        (span "🔍")
                                        (span ,(G_ "In Review"))
                                        (span (@ (class
                                                  "text-xs text-stone-700 font-light"))
                                              ,(format #f " ( ~a ) "
                                                       (length
                                                        tickets-in-review))))
                                    ,(map render-k-ticket tickets-in-review)))
                          (hr)
                          (div (@ (class
                                   "sm:w-1/3 my-6 p-4 flex flex-col gap-2 p-2"))
                               (details (@ (class "cursor-pointer "))
                                        (summary (@ (class
                                                     "marker:block hover:cursor-pointer items-center"))
                                                 (h4 (@ (class
                                                         "text-sm font-bold flex gap-2 items-center"))
                                                     (span "✅")
                                                     (span ,(G_ "Done"))
                                                     (span (@ (class
                                                               "text-xs text-stone-700 font-light"))
                                                           ,(format #f
                                                             " ( ~a ) "
                                                             (length
                                                              tickets-done)))))
                                        (div (@ (class
                                                 "my-8 flex flex-col gap-2"))
                                             ,(map render-k-ticket
                                                   tickets-done))))

                          (hr)
                          (div (@ (class
                                   "sm:w-1/3 my-6 p-4 flex flex-col gap-2 p-2"))
                               (details (@ (class "cursor-pointer"))
                                        (summary (@ (class
                                                     "marker:block hover:cursor-pointer items-center"))
                                                 (h4 (@ (class
                                                         "text-sm font-bold flex gap-2 items-center"))
                                                     (span "❌")
                                                     (span ,(G_ "Rejected"))
                                                     (span (@ (class
                                                               "text-xs text-stone-700 font-light"))
                                                           ,(format #f
                                                             " ( ~a ) "
                                                             (length
                                                              tickets-rejected)))))
                                        (div (@ (class
                                                 "my-8 flex flex-col gap-2"))
                                             ,(map render-k-ticket
                                                   tickets-rejected)))))))
      (html-template G_
                     #:body kanban-html
                     #:proj projects)))
  (export kanban-from-tickets))

