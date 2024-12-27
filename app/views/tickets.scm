;;; lucidplan - free and open project management for everyone

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

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

(define-module (app views tickets)
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
  #:use-module (lucidplan html)
  #:use-module (artanis route))

(define-public (render-ticket project ticket)
  `((div (@ (class "flex flex-col gap-2 p-3 bg-stone-100 rounded-xl"))
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "title:")
         (h4 (@ (class "font-mono text-stone-700 text-lg font-bold"))
             ,(assoc-ref ticket
                         "title"))
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "status:")
         (p (@ (class "font-mono text-stone-700 text-md"))
            ,(assoc-ref ticket
                        "ticket_status"))
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "type:")
         (p (@ (class "font-mono text-stone-700 text-md"))
            ,(assoc-ref ticket
                        "ticket_type"))
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "description:")
         (p (@ (class
                 "font-mono text-stone-600 text-md max-w-prose whitespace-pre-wrap"))
            ,(assoc-ref ticket
                        "description"))
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "meta:")
         (form (@ (enctype "application/x-www-form-urlencoded")
                  (method "POST")
                  (action ,(format #f "/backlog/~a/tickets/delete" project)))
               (input (@ (hidden "")
                         (name "id")
                         (value ,(assoc-ref ticket
                                            "id"))))
               (button (@ (type "submit")
                          (class "text-red-600")) "delete"))
         (small (@ (class "font-mono text-stone-500 text-xs"))
                ,(format #f "~a-~s" project
                         (assoc-ref ticket
                                    "human_id")))
         (small (@ (class "font-mono text-stone-500 text-xs"))
                ,(assoc-ref ticket
                            "id")))))

(define-public ticket-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define-public (render-ticket-listitem project ticket)
  `((div (@ (draggable "true")
            (ondragstart "dragBacklogItem(event)")
            (class "flex flex-col gap-2")
            (id ,(format #f "listitem-~a"
                         (assoc-ref ticket
                                    "id")))
            (onclick ,(format #f ticket-listitem-reveal-details-js-template
                              (assoc-ref ticket
                                         "id"))))
         (div (@ (class
                   "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300 border-stone-200 py-1 px-3 cursor-pointer rounded-xl"))
              (small (@ (class "font-mono text-stone-500 text-sm"))
                     ,(format #f "~a-~a" project
                              (assoc-ref ticket "human_id")))
              (h5 (@ (class "font-mono text-stone-700 text-lg"))
                  ,(assoc-ref ticket
                              "title")))
         (div (@ (id ,(format #f "details-~a"
                              (assoc-ref ticket
                                         "id")))
                 (class "ml-4")
                 (hidden ""))
              ,(render-ticket project ticket)))
    
    ))

(define-public (html-from-backlog project tickets)
  (html-template `((div (@ (class "flex flex-col gap-4 flex-grow"))
                        (div (@ (id "lucidplan-backlog-list")
                                (ondrop "dropBacklogItem(event)")
                                (ondragover "allowDrop(event)")
                                (class "flex flex-col gap-2 container"))
                             ,(map (lambda (t)
                                     (render-ticket-listitem project
                                                             t))
                                   tickets))
                        (div (a (@ (href ,(format #f "/backlog/~a/tickets/new"
                                                  project))) "+ new ticket"))))))

(define-public (html-new-ticket-form project)
  (html-template `((form (@ (class "flex flex-col gap-4 flex-grow")
                            (enctype "application/x-www-form-urlencoded")
                            (method "POST")
                            (action ,(format #f
                                             "/backlog/~a/tickets/new-submit"
                                             project)))
                         (label "title:")
                         (input (@ (name "title")
                                   (required "")) "")
                         (label "status:")
                         (select (@ (name "ticket-status")
                                    (required ""))
                                 (option (@ (value "todo")
                                            (selected "")) "todo")
                                 (option (@ (value "work-in-progress"))
                                         "work-in-progress")
                                 (option (@ (value "in-review")) "in-review")
                                 (option (@ (value "done")) "done")
                                 (option (@ (value "rejected")) "rejected"))
                         (label "type:")
                         (select (@ (name "ticket-type")
                                    (required ""))
                                 (option (@ (value "task")
                                            (selected "")) "task")
                                 (option (@ (value "bug")) "bug")
                                 (option (@ (value "story")) "story")
                                 (option (@ (value "spike")) "spike")
                                 (option (@ (value "time-box")) "time-box"))
                         (label "description:")
                         (textarea (@ (name "description")
                                      (required "")) "")
                         (button (@ (type "submit")) "create")))))
