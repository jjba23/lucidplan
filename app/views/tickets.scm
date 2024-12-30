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

(define-public (render-ticket project ticket ticket-id-to-human-id)
  
  (define (mk-rel pp)
    (let ((x (assoc-ref ticket-id-to-human-id pp)))
      (cond
       ((not (equal? #f x))
        `(span (@ (class "font-mono text-stone-500 text-xs"))
               ,(format #f "~a-~a" project x))
        )
       (else `())
       )
      ))

  (define (mk-cond-rel r label)
    (let ((xs (string-split (assoc-ref ticket r) #\:)))
      (cond
       ((and (not (null? xs))
             (not (equal? '("") xs)))
        `((div (@ (class "flex flex-col gap-2"))
               (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                      ,label)
               (div (@ (class
                         "flex flex-row flex-wrap gap-4 items-center justify-left ml-4"))
                    ,(map mk-rel xs)))))
       (else `()))))

  `((div (@ (class "flex flex-col gap-2 p-3 bg-stone-100 rounded-xl"))
         (h4 (@ (class "font-mono text-stone-700 text-2xl font-bold"))
             ,(assoc-ref ticket "title"))
         (a (@ (class "font-mono text-stone-400 text-xs font-light")
               (href ,(format #f "/tickets/~a/~a/edit" project
                              (assoc-ref ticket "id")))) "edit ticket")
         (hr (@(class "my-2")))

         
         (div (@(class "flex flex-row flex-wrap gap-8 justify-left my-4"))
              
              (div (@(class "flex flex-col gap-3"))
                   (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                          "status:")
                   (p (@ (class "font-mono text-stone-700 text-sm ml-4"))
                      ,(assoc-ref ticket "ticket_status"))
                   )
              
              (div (@(class "flex flex-col gap-3"))
                   (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                          "type:")
                   (p (@ (class "font-mono text-stone-700 text-sm ml-4"))
                      ,(assoc-ref ticket "ticket_type"))
                   )
              )

         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "description:")
         (p (@ (class
                 "font-mono text-stone-600 text-md max-w-prose whitespace-pre-wrap ml-4"))
            ,(assoc-ref ticket "description"))

         (hr (@(class "my-2")))
         
         (small (@(class "font-mono text-stone-400 text-xs font-light")) "human id:")
         (small (@ (class "font-mono text-stone-500 text-xs ml-4"))
                ,(format #f "~a-~s" project
                         (assoc-ref ticket "human_id")))
         
         ,(mk-cond-rel "child_of" "child of:")
         ,(mk-cond-rel "blocked_by" "blocked by:")
         ,(mk-cond-rel "splits_from" "splits from:")
         ,(mk-cond-rel "clone_of" "clone of:")
         ,(mk-cond-rel "blocks" "blocks:")
         ,(mk-cond-rel "related_to" "related to:")

         
         
         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "id:")
         (small (@ (class "font-mono text-stone-500 text-xs ml-4"))
                ,(assoc-ref ticket "id"))

         (small (@ (class "font-mono text-stone-400 text-xs font-light"))
                "user actions:")
         (form (@ (enctype "application/x-www-form-urlencoded")
                  (method "POST")
                  (action ,(format #f "/tickets/~a/delete" project)))
               (input (@ (hidden "")
                         (name "id")
                         (value ,(assoc-ref ticket "id"))))
               (button (@ (type "submit")
                          (class "text-red-600 text-sm ml-4")) "delete"))
         
         )))

(define-public ticket-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define-public (render-ticket-listitem project ticket ticket-id-to-human-id)
  `((div (@ (draggable "true")
            (ondragstart "dragTicketsItem(event)")
            (class "flex flex-col gap-2")
            (id ,(format #f "listitem-~a"
                         (assoc-ref ticket "id")))
            (onclick ,(format #f ticket-listitem-reveal-details-js-template
                              (assoc-ref ticket "id"))))
         (div (@ (class
                  "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300 border-stone-200 py-1 px-3 cursor-pointer rounded-xl"))
              (small (@ (class "font-mono text-stone-500 text-sm"))
                     ,(format #f "~a-~a" project
                              (assoc-ref ticket "human_id")))
              (h5 (@ (class "font-mono text-stone-700 text-lg"))
                  ,(assoc-ref ticket "title")))
         (div (@ (id ,(format #f "details-~a"
                              (assoc-ref ticket "id")))
                 (class "ml-4")
                 (hidden ""))
              ,(render-ticket project ticket ticket-id-to-human-id)))
    
    ))

(define-public (html-from-tickets project tickets ticket-id-to-human-id)
  (html-template `((div (@ (class "flex flex-col gap-4 flex-grow"))
                        (div (@ (id "lucidplan-tickets-list")
                                (ondrop "dropTicketsItem(event)")
                                (ondragover "allowDrop(event)")
                                (class "flex flex-col gap-2 container"))
                             ,(map (lambda (t)
                                     (render-ticket-listitem project t
                                      ticket-id-to-human-id)) tickets))
                        (div (a (@ (href ,(format #f "/tickets/~a/new" project)))
                                "+ new ticket"))))))

(define-public (html-new-ticket-form project)
  (html-ticket-form #:project project
                    #:action (format #f "/tickets/~a/new" project)))

(define-public (html-edit-ticket-form project t)
  (html-ticket-form #:project project
                    #:title (assoc-ref t "title")
                    #:description (assoc-ref t "description")
                    #:ticket-type (assoc-ref t "ticket_type")
                    #:ticket-status (assoc-ref t "ticket_status")
                    #:child-of (assoc-ref t "child_of")
                    #:blocked-by (assoc-ref t "blocked_by")
                    #:splits-from (assoc-ref t "splits_from")
                    #:clone-of (assoc-ref t "clone_of")
                    #:blocks (assoc-ref t "blocks")
                    #:related-to (assoc-ref t "related_to")
                    #:action (format #f "/tickets/~a/~a/edit" project
                                     (assoc-ref t "id"))))

(define* (html-ticket-form #:key (project #f)
                           (title "")
                           (description "")
                           (ticket-status "")
                           (ticket-type "")
                           (child-of "")
                           (blocked-by "")
                           (splits-from "")
                           (clone-of "")
                           (blocks "")
                           (related-to "")
                           (action ""))
  (html-template `((form (@ (class "flex flex-col gap-4 flex-grow")
                            (enctype "application/x-www-form-urlencoded")
                            (method "POST")
                            (action ,action))
                         (label "title:")
                         (input (@ (class "p-3 w-min rounded-xl")
                                   (name "title")
                                   (value ,title)
                                   (required "")) "")
                         (label "status:")
                         (select (@ (class "p-3 w-min rounded-xl")
                                    (name "ticket-status")
                                    (required ""))
                                 ,(mk-option "todo" ticket-status)
                                 ,(mk-option "work-in-progress" ticket-status)
                                 ,(mk-option "in-review" ticket-status)
                                 ,(mk-option "done" ticket-status)
                                 ,(mk-option "rejected" ticket-status))
                         (label "type:")
                         (select (@ (class "p-3 w-min rounded-xl")
                                    (name "ticket-type")
                                    (required ""))
                                 ,(mk-option "task" ticket-type)
                                 ,(mk-option "bug" ticket-type)
                                 ,(mk-option "story" ticket-type)
                                 ,(mk-option "spike" ticket-type)
                                 ,(mk-option "time-box" ticket-type))
                         (label "description:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "description")
                                      (required ""))
                                   ,description)

                         (label "child of:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "child-of"))
                                   ,child-of)
                         (label "blocked by:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "blocked-by"))
                                   ,blocked-by)
                         (label "splits from:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "splits-from"))
                                   ,splits-from)
                         (label "clone of:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "clone-of"))
                                   ,clone-of)
                         (label "blocks:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "blocks"))
                                   ,blocks)
                         (label "related to:")
                         (textarea (@ (class "p-3 w-min rounded-xl")
                                      (name "related-to"))
                                   ,related-to)

                         (button (@ (class
                                     "bg-green-800 text-white p-3 w-min rounded-xl")
                                    (type "submit")) "done")))))
