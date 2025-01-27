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
  #:use-module (ice-9 regex)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan html)
  #:use-module (lucidplan base64)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define-public (parse-org org)
  (write (string-match "/{1}[^\n]*/{1}" org)) org)

(define-public (render-ticket G_ project ticket ticket-id-to-human-id)
  `((div (@ (class "flex flex-col gap-2 p-3 bg-stone-100 rounded-xl"))
         (h4 (@ (class "font-sans text-stone-700 text-2xl font-bold"))
             ,(safe-base64-decode (assoc-ref ticket "title")))
         (a (@ (class "font-sans text-stone-400 text-xs font-light")
               (href ,(format #f "/tickets/~a/~a/edit" project
                              (assoc-ref ticket "id"))))
            ,(G_ "edit ticket"))
         (hr (@ (class "")))

         (div (@ (class "flex flex-row flex-wrap gap-8 justify-left"))

              (div (@ (class "flex flex-col gap-3"))
                   (small (@ (class
                              "font-sans text-stone-400 text-xs font-light"))
                          ,(G_ "status"))
                   (p (@ (class "font-sans text-stone-700 text-sm"))
                      ,(safe-base64-decode (assoc-ref ticket "ticket_status"))))

              (div (@ (class "flex flex-col gap-3"))
                   (small (@ (class
                              "font-sans text-stone-400 text-xs font-light"))
                          ,(G_ "type"))
                   (p (@ (class "font-sans text-stone-700 text-sm ml-4"))
                      ,(safe-base64-decode (assoc-ref ticket "ticket_type")))))

         (small (@ (class "font-sans text-stone-400 text-xs font-light"))
                ,(G_ "description"))
         (p (@ (class
                "font-sans text-stone-600 text-md max-w-prose whitespace-pre-wrap ml-4"))
            ,(parse-org (safe-base64-decode (assoc-ref ticket "description"))))

         (hr (@ (class "")))

         (small (@ (class "font-sans text-stone-400 text-xs font-light"))
                ,(G_ "human id"))
         (small (@ (class "font-sans text-stone-500 text-xs ml-4"))
                ,(format #f "~a-~s" project
                         (assoc-ref ticket "human_id")))

         (small (@ (class "font-sans text-stone-400 text-xs font-light"))
                ,(G_ "id"))
         (small (@ (class "font-sans text-stone-500 text-xs ml-4"))
                ,(assoc-ref ticket "id"))

         (small (@ (class "font-sans text-stone-400 text-xs font-light"))
                ,(G_ "user actions"))
         (form (@ (enctype "application/x-www-form-urlencoded")
                  (method "POST")
                  (action ,(format #f "/tickets/~a/delete" project)))
               (input (@ (hidden "")
                         (name "id")
                         (value ,(assoc-ref ticket "id"))))
               (button (@ (type "submit")
                          (class "text-red-600 text-sm ml-4"))
                       ,(G_ "delete")))

         )))

(define-public ticket-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define-public (render-ticket-listitem G_ project ticket ticket-id-to-human-id)
  `((div (@ (class "flex flex-col gap-2")
            (id ,(format #f "listitem-~a"
                         (assoc-ref ticket "id"))))
         (div (@ (draggable "true")
                 (ondragstart "dragTicketsItem(event)")
                 (onclick ,(format #f
                                   ticket-listitem-reveal-details-js-template
                                   (assoc-ref ticket "id")))
                 (class
                  "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300/50 border-stone-200 py-1 px-3 cursor-pointer rounded-xl"))
              (small (@ (class "font-sans text-stone-500 text-sm"))
                     ,(format #f "~a-~a" project
                              (assoc-ref ticket "human_id")))
              (h5 (@ (class "font-sans text-stone-700 text-lg"))
                  ,(safe-base64-decode (assoc-ref ticket "title"))))
         (div (@ (id ,(format #f "details-~a"
                              (assoc-ref ticket "id")))
                 (class "ml-4")
                 (hidden ""))
              ,(render-ticket G_ project ticket ticket-id-to-human-id)))
    
    ))

(define-public (html-from-tickets G_ projects project tickets
                                  ticket-id-to-human-id)
  (html-template G_
                 #:body `(div (@ (class "container mx-auto"))
                              ((div (@ (class "flex flex-col gap-4 flex-grow"))
                                    (div (@ (class
                                             "flex flex-wrap flex-row gap-6 items-center"))
                                         (h2 (@ (class
                                                 "text-2xl font-sans font-bold text-stone-500"))
                                             ,(format #f "tickets for ~a"
                                                      project))
                                         (a (@ (class "text-stone-500 text-md")
                                               (href ,(format #f "/kanban/~a"
                                                              project)))
                                            (span "kanban board")))
                                    (div (@ (id "lucidplan-tickets-list")
                                            (ondrop "dropTicketsItem(event)")
                                            (ondragover "allowDrop(event)")
                                            (class
                                             "flex flex-col gap-2 container"))
                                         ,(map (lambda (t)
                                                 (render-ticket-listitem G_
                                                  project t
                                                  ticket-id-to-human-id))
                                               tickets))
                                    (div (a (@ (href ,(format #f
                                                       "/tickets/~a/new"
                                                       project)))
                                            "+ new ticket")))))
                 #:proj projects))

(define-public (html-new-ticket-form G_ proj project)
  (html-ticket-form G_
                    #:project project
                    #:proj proj
                    #:action (format #f "/tickets/~a/new" project)))

(define-public (html-edit-ticket-form G_ proj project t)
  (html-ticket-form G_
                    #:project project
                    #:is-edit #t
                    #:title (safe-base64-decode (assoc-ref t "title"))
                    #:description (safe-base64-decode (assoc-ref t
                                                       "description"))
                    #:ticket-type (safe-base64-decode (assoc-ref t
                                                       "ticket_type"))
                    #:ticket-status (safe-base64-decode (assoc-ref t
                                                         "ticket_status"))
                    #:action (format #f "/tickets/~a/~a/edit" project
                                     (assoc-ref t "id"))
                    #:proj proj))

(define* (html-ticket-form G_
                           #:key (project #f)
                           (title "")
                           (description "")
                           (ticket-status "")
                           (ticket-type "")
                           (action "")
                           (is-edit #f)
                           (proj '()))
  (define form_
    `((form (@ (class "flex flex-col gap-4 flex-grow")
               (enctype "application/x-www-form-urlencoded")
               (method "POST")
               (action ,action))
            (label ,(G_ "title"))
            (input (@ (class "p-3 w-min rounded-xl")
                      (name "title")
                      (value ,title)
                      (required "")) "")
            (label ,(G_ "status"))
            (select (@ (class "p-3 rounded-xl")
                       (name "ticket-status")
                       (required ""))
                    ,(mk-option "todo" ticket-status)
                    ,(mk-option "work-in-progress" ticket-status)
                    ,(mk-option "in-review" ticket-status)
                    ,(mk-option "done" ticket-status)
                    ,(mk-option "rejected" ticket-status))
            (label ,(G_ "type"))
            (select (@ (class "p-3 rounded-xl")
                       (name "ticket-type")
                       (required ""))
                    ,(mk-option "task" ticket-type)
                    ,(mk-option "bug" ticket-type)
                    ,(mk-option "story" ticket-type)
                    ,(mk-option "spike" ticket-type)
                    ,(mk-option "time-box" ticket-type))
            (label ,(G_ "description"))
            (textarea (@ (class "p-3 w-full min-h-48 rounded-xl")
                         (name "description")
                         (required ""))
                      ,description)

            (button (@ (class
                        "bg-green-700/75 text-white p-2 w-min rounded-xl")
                       (type "submit"))
                    ,(G_ "done")))))

  (html-template G_
                 #:body `((div (@ (class "container mx-auto"))
                               ,form_))
                 #:proj proj))

(define-public (kanban-from-tickets G_ projects project tickets
                                    ticket-id-to-human-id)
  (html-template G_
                 #:body `((div (@ (class "container mx-auto"))
                               ((div (@ (class "flex flex-col gap-4 flex-grow"))
                                     (div (@ (class
                                              "flex flex-wrap flex-row gap-6 items-center"))
                                          (h2 (@ (class
                                                  "text-2xl font-sans font-bold text-stone-500"))
                                              ,(format #f "kanban for ~a"
                                                       project))
                                          (a (@ (class
                                                 "text-stone-500 text-md")
                                                (href ,(format #f
                                                               "/tickets/~a"
                                                               project)))
                                             (span "backlog")))
                                     (div (a (@ (href ,(format #f
                                                        "/tickets/~a/new"
                                                        project)))
                                             "+ new ticket"))

                                     )))
                          (div (@ (class "flex flex-wrap gap-1 justify-center"))
                               (div (@ (class
                                        "w-1/5 flex flex-col gap-2 bg-stone-200"))
                                    (h4 (@ (class "text-sm")) "todo"))
                               (div (@ (class
                                        "w-1/5 flex flex-col gap-2 bg-stone-200"))
                                    (h4 (@ (class "text-sm"))
                                        "work-in-progress"))
                               (div (@ (class
                                        "w-1/5 flex flex-col gap-2 bg-stone-200"))
                                    (h4 (@ (class "text-sm")) "in-review"))
                               (div (@ (class
                                        "w-1/5 flex flex-col gap-2 bg-stone-200"))
                                    (h4 (@ (class "text-sm")) "done"))
                               ;; ,(map (lambda (t)
                               ;; (render-ticket-kanban-item G_ project
                               ;; t ticket-id-to-human-id)) tickets)
                               ))
                 #:proj projects))
