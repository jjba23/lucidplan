;;; lucidplan - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (lucidplan query-escape)
  #:use-module (lucidplan domain)
  #:use-module (orgwebalchemy org-to-html)
  #:use-module (hygguile components button)
  #:use-module (hygguile components select)
  #:use-module (hygguile components input)
  #:use-module (hygguile components icon)
  #:use-module (hygguile abstraction)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define-public (render-ticket G_ project ticket)
  `((div (@ (class ,(->css '(flex flex-col
                                  gap-2
                                  p-3
                                  bg-stone-100
                                  rounded-2xl
                                  border-stone-400/75
                                  border-2))))
         (div (@ (class ,(->css '(flex flex-row justify-between))))
              (h4 (@ (class ,(->css '(font-sans text-stone-700 text-2xl
                                                font-bold break-words my-3))))
                  ,(org-to-html (assoc-ref ticket "title")))
              (div (@ (class ,(->css '(flex justify-right))))
                   (span (@ (class ,(->css '(cursor-pointer)))
                            (onclick ,(format #f
                                       "document.getElementById('details-~a').toggleAttribute('hidden')"
                                       (assoc-ref ticket "id"))))
                         ,(hyg-icon #:type 'x
                                    #:icon-css '(text-stone-800 text-xl)))))

         (div (@ (class ,(->css '(flex flex-row flex-wrap gap-8 justify-left))))

              (div (@ (class ,(->css '(flex flex-col gap-3))))
                   (small (@ (class ,(->css '(font-sans text-stone-500 text-md
                                                        font-light))))
                          ,(G_ "status"))
                   (p (@ (class ,(->css '(flex gap-2 items-center))))
                      (span (@ (class "text-2xl"))
                            ,(emoji-for-ticket-status (assoc-ref ticket
                                                       "ticket_status")))
                      (span (@ (class ,(->css '(font-sans text-stone-700
                                                          text-sm))))
                            ,(assoc-ref ticket "ticket_status"))))

              (div (@ (class ,(->css '(flex flex-col gap-3))))
                   (small (@ (class ,(->css '(font-sans text-stone-500 text-md
                                                        font-light))))
                          ,(G_ "type"))
                   (p (@ (class ,(->css '(ml-4 flex gap-2 items-center))))
                      (span (@ (class "text-2xl"))
                            ,(emoji-for-ticket-type (assoc-ref ticket
                                                               "ticket_type")))
                      (span (@ (class ,(->css '(font-sans text-stone-700
                                                          text-sm))))
                            ,(assoc-ref ticket "ticket_type")))))

         (small (@ (class ,(->css '(font-sans text-stone-500 text-md
                                              font-light))))
                ,(G_ "description"))
         (span (@ (class ,(->css '(text-md py-3
                                           text-stone-800
                                           font-medium
                                           font-sans
                                           max-w-prose
                                           whitespace-pre-wrap
                                           ml-4))))
               ,(org-to-html (assoc-ref ticket "description")))

         (hr (@ (class "")))

         (small (@ (class ,(->css '(font-sans text-stone-500 text-md
                                              font-light))))
                ,(G_ "human id"))
         (small (@ (class ,(->css '(font-sans text-stone-600 text-md ml-4))))
                ,(format #f "~a-~s" project
                         (assoc-ref ticket "human_id")))

         (small (@ (class ,(->css '(font-sans text-stone-500 text-md
                                              font-light))))
                ,(G_ "id"))
         (small (@ (class ,(->css '(font-sans text-stone-600 text-md ml-4))))
                ,(assoc-ref ticket "id"))

         (hr)
         (div (@ (class ,(->css '(flex flex-row flex-wrap gap-4))))
              (a (@ (href ,(format #f "/tickets/~a/~a/edit" project
                                   (assoc-ref ticket "human_id"))))
                 ,(hyg-button `(span (@ (class ,(->css '(items-center flex
                                                         flex-row gap-1))))
                                     ((i (@ (class ,(->css '(ph ph-pencil
                                                                text-white
                                                                text-xl)))) ""))
                                     (G_ "edit ticket"))))
              (form (@ (enctype application/x-www-form-urlencoded)
                       (method POST)
                       (action ,(format #f "/tickets/~a/delete" project)))
                    (input (@ (hidden "")
                              (name id)
                              (value ,(assoc-ref ticket "id"))))
                    ,(hyg-danger-submit-button `(span (@ (class ,(->css '(items-center
                                                                          flex
                                                                          flex-row
                                                                          gap-1))))
                                                      ((i (@ (class ,(->css '(ph
                                                                              ph-trash
                                                                              text-white
                                                                              text-xl))))
                                                          ""))
                                                      ,(G_ "delete")))))

         )))

(define-public (render-ticket-listitem G_ project ticket ticket-id-to-human-id)
  `((div (@ (class ,(->css '(flex flex-col gap-2)))
            (id ,(format #f "listitem-~a"
                         (assoc-ref ticket "id"))))
         (div (@ (onclick ,(format #f
                            "document.getElementById('details-~a').toggleAttribute('hidden')"
                            (assoc-ref ticket "id")))
                 (class ,(->css '(flex flex-row
                                       gap-4
                                       items-center
                                       justify-left
                                       bg-stone-300/75
                                       border-stone-400/75
                                       border-2
                                       py-2
                                       px-4
                                       shadow
                                       cursor-pointer
                                       rounded-xl))))
              ,(emoji-for-ticket-type (assoc-ref ticket "ticket_type"))
              (small (@ (class ,(->css '(font-sans text-stone-600 text-xs
                                                   hidden md:flex))))
                     ,(format #f "~a-~a" project
                              (assoc-ref ticket "human_id")))
              (h5 (@ (class ,(->css '(font-sans text-stone-700 text-md
                                                font-black break-words))))
                  ,(org-to-html (assoc-ref ticket "title")))
              (div (a (@ (href ,(format #f "/tickets/~a/~a" project
                                        (assoc-ref ticket "human_id"))))
                      (span ((i (@ (class ,(->css '(ph ph-link text-stone-600))))
                                ""))))))
         (div (@ (id ,(format #f "details-~a"
                              (assoc-ref ticket "id")))
                 (class "ml-4")
                 (hidden ""))
              ,(render-ticket G_ project ticket)))
    
    ))

(begin
  (define* (html-from-tickets G_
                              #:key projects
                              project-details
                              tickets
                              ticket-id-to-human-id
                              with-done
                              with-rejected
                              search-query)
    (html-template G_
                   #:body `(div (@ (class "container mx-auto"))
                                ((div (@ (class ,(->css '(flex flex-col gap-4
                                                               flex-grow))))
                                      (div (@ (class ,(->css '(flex flex-wrap
                                                               flex-row gap-4
                                                               items-center
                                                               justify-between))))
                                           (div (@ (class ,(->css '(flex
                                                                    flex-row
                                                                    gap-2
                                                                    items-center))))
                                                ,(hyg-icon #:type 'scroll
                                                           #:icon-css '(text-stone-700
                                                                        text-2xl))
                                                (h2 (@ (class ,(->css '(text-3xl
                                                                        font-sans
                                                                        font-black
                                                                        text-stone-700))))
                                                    ,(format #f
                                                             "backlog of ~a"
                                                             (assoc-ref
                                                              project-details
                                                              "title"))))
                                           (div (@ (class ,(->css '(flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    gap-1
                                                                    items-center))))
                                                (div (a (@ (href ,(format #f
                                                                   "/kanban/~a"
                                                                   (assoc-ref
                                                                    project-details
                                                                    "human_id"))))
                                                        ,(hyg-icon-button
                                                                          #:icon 'kanban
                                                                          #:content
                                                                          (G_
                                                                           "kanban"))))
                                                (a (@ (href ,(format #f
                                                              "/tickets/~a/new"
                                                              (assoc-ref
                                                               project-details
                                                               "human_id"))))
                                                   ,(hyg-icon-button #:icon 'plus
                                                                     #:content
                                                                     (G_
                                                                      "new ticket")))))
                                      ,(render-if-nestring (lambda (x)
                                                             `(p (@ (class ,(->css '
                                                                             (text-sm
                                                                              text-stone-600))))
                                                                 ,x))
                                                           (assoc-ref
                                                            project-details
                                                            "description"))

                                      (div (details (summary (@ (class ,(->css '
                                                                         (text-stone-600
                                                                          text-sm
                                                                          cursor-pointer))))
                                                             ,(G_
                                                               "backlog options"))
                                                    (div (@ (class ,(->css '(flex
                                                                             flex-wrap
                                                                             flex-row
                                                                             gap-4
                                                                             my-3))))
                                                         (input (@ (id
                                                                    "should-show-done")
                                                                   (type
                                                                    checkbox)
                                                                   ,(if
                                                                     with-done
                                                                     '(checked
                                                                       true)
                                                                     '(not-checked
                                                                       true))
                                                                   (onchange ,
                                                                             (format
                                                                              #f

                                                                              
                                                                              (string-append
                                                                               "window.location.href ="
                                                                               " `/tickets/~a?with-done="
                                                                               "${document.getElementById('should-show-done').checked}`;")

                                                                              
                                                                              (assoc-ref
                                                                               project-details
                                                                               "human_id"))))
                                                                (span (@ (class
                                                                          "text-stone-600 text-sm"))
                                                                      ,(G_
                                                                        "show done?")))
                                                         (input (@ (id
                                                                    "should-show-rejected")
                                                                   (type
                                                                    checkbox)
                                                                   ,(if
                                                                     with-rejected
                                                                     '(checked
                                                                       true)
                                                                     '(not-checked
                                                                       true))
                                                                   (onchange ,
                                                                             (format
                                                                              #f

                                                                              
                                                                              (string-append
                                                                               "window.location.href ="
                                                                               " `/tickets/~a?with-rejected="
                                                                               "${document.getElementById('should-show-rejected').checked}`;")

                                                                              
                                                                              (assoc-ref
                                                                               project-details
                                                                               "human_id"))))
                                                                (span (@ (class
                                                                          "text-stone-600 text-sm"))
                                                                      ,(G_
                                                                        "show rejected?"))))))

                                      (div (@ (class ,(->css '(flex flex-row
                                                               flex-wrap
                                                               justify-between))))
                                           (div (@ (class ,(->css '(my-3 p-2
                                                                    flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    justify-start
                                                                    align-center
                                                                    items-center))))
                                                ,(sort-select G_
                                                              #:project-id (assoc-ref
                                                                            project-details
                                                                            "human_id")))
                                           (div (@ (class ,(->css '(my-3 p-2
                                                                    flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    justify-end
                                                                    align-center
                                                                    items-center))))
                                                ,(search-box G_
                                                             #:search-query
                                                             search-query)))

                                      (div (@ (id lucidplan-tickets-list)
                                              (ondrop "dropTicketsItem(event)")
                                              (ondragover "allowDrop(event)")
                                              (class ,(->css '(flex flex-col
                                                                    gap-2
                                                                    container))))
                                           ,(map (lambda (t)
                                                   (render-ticket-listitem G_
                                                    (assoc-ref project-details
                                                     "human_id") t
                                                    ticket-id-to-human-id))
                                                 (filter
                                                  only-active-tickets-filter
                                                  tickets)))

                                      ,(if with-done
                                           `(div (@ (id
                                                     "lucidplan-done-tickets-list")
                                                    (class "my-4"))
                                                 (hr)
                                                 (h2 (@ (class ,(->css '(text-lg
                                                                         font-sans
                                                                         font-black
                                                                         text-stone-600
                                                                         flex
                                                                         flex-row
                                                                         gap-2
                                                                         my-4))))
                                                     ,(emoji-for-ticket-status
                                                       "done")
                                                     (span ,(G_ "done")))
                                                 (div (@ (class
                                                          "flex flex-col gap-2 container"))
                                                      ,(map (lambda (t)
                                                              (render-ticket-listitem
                                                               G_
                                                               (assoc-ref
                                                                project-details
                                                                "human_id") t
                                                               ticket-id-to-human-id))
                                                            (filter
                                                             only-done-tickets-filter
                                                             tickets))))
                                           '())
                                      ,(if with-rejected
                                           `(div (@ (id
                                                     "lucidplan-rejected-tickets-list")
                                                    (class "my-4"))
                                                 (hr)
                                                 (h2 (@ (class ,(->css '(text-lg
                                                                         font-sans
                                                                         font-black
                                                                         text-stone-600
                                                                         flex
                                                                         flex-row
                                                                         gap-2
                                                                         my-4))))
                                                     ,(emoji-for-ticket-status
                                                       "rejected")
                                                     (span ,(G_ "rejected")))
                                                 (div (@ (class
                                                          "flex flex-col gap-2 container"))
                                                      ,(map (lambda (t)
                                                              (render-ticket-listitem
                                                               G_
                                                               (assoc-ref
                                                                project-details
                                                                "human_id") t
                                                               ticket-id-to-human-id))
                                                            (filter
                                                             only-rejected-tickets-filter
                                                             tickets))))
                                           '()))))
                   #:proj projects))
  (export html-from-tickets))

(define-public (html-new-ticket-form G_ proj project)
  (html-ticket-form G_
                    #:project project
                    #:proj proj
                    #:action (format #f "/tickets/~a/new" project)))

(define-public (html-edit-ticket-form G_ proj project t)
  (html-ticket-form G_
                    #:project project
                    #:is-edit #t
                    #:title (assoc-ref t "title")
                    #:description (assoc-ref t "description")
                    #:ticket-type (assoc-ref t "ticket_type")
                    #:ticket-status (assoc-ref t "ticket_status")
                    #:action (format #f "/tickets/~a/~a/edit" project
                                     (assoc-ref t "human_id"))
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
    `((form (@ (class "flex flex-col gap-4")
               (enctype "application/x-www-form-urlencoded")
               (method "POST")
               (action ,action))
            ,(hyg-input #:label (G_ "title")
                        #:name "title"
                        #:value (unescape-str title)
                        #:required #t)

            (label ,(G_ "status"))
            (select (@ (class ,(->css '(p-3 rounded-xl border-stone-400/75
                                            border-2 shadow-md)))
                       (name "ticket-status")
                       (required ""))
                    ,(hyg-glyph-option #:value "todo"
                                       #:glyph (emoji-for-ticket-status "todo")
                                       #:content (G_ "To-do")
                                       #:selected-value ticket-status)
                    ,(hyg-glyph-option #:value "work-in-progress"
                                       #:glyph (emoji-for-ticket-status
                                                "work-in-progress")
                                       #:content (G_ "Work in Progress")
                                       #:selected-value ticket-status)
                    ,(hyg-glyph-option #:value "in-review"
                                       #:glyph (emoji-for-ticket-status
                                                "in-review")
                                       #:content (G_ "In Review")
                                       #:selected-value ticket-status)
                    ,(hyg-glyph-option #:value "done"
                                       #:glyph (emoji-for-ticket-status "done")
                                       #:content (G_ "Done")
                                       #:selected-value ticket-status)
                    ,(hyg-glyph-option #:value "rejected"
                                       #:glyph (emoji-for-ticket-status
                                                "rejected")
                                       #:content (G_ "Rejected")
                                       #:selected-value ticket-status))
            (label ,(G_ "type"))
            (select (@ (class ,(->css '(p-3 rounded-xl border-stone-400/75
                                            border-2 shadow-md)))
                       (name "ticket-type")
                       (required ""))
                    ,(hyg-glyph-option #:value "task"
                                       #:glyph (emoji-for-ticket-type "task")
                                       #:content (G_ "Task")
                                       #:selected-value ticket-type)
                    ,(hyg-glyph-option #:value "bug"
                                       #:glyph (emoji-for-ticket-type "bug")
                                       #:content (G_ "Bug")
                                       #:selected-value ticket-type)
                    ,(hyg-glyph-option #:value "story"
                                       #:glyph (emoji-for-ticket-type "story")
                                       #:content (G_ "Story")
                                       #:selected-value ticket-type)
                    ,(hyg-glyph-option #:value "spike"
                                       #:glyph (emoji-for-ticket-type "spike")
                                       #:content (G_ "Spike")
                                       #:selected-value ticket-type)
                    ,(hyg-glyph-option #:value "time-box"
                                       #:glyph (emoji-for-ticket-type
                                                "time-box")
                                       #:content (G_ "Time Box")
                                       #:selected-value ticket-type))
            (label ,(G_ "description"))
            (textarea (@ (class ,(->css '(p-3 w-full min-h-96 rounded-xl
                                              border-stone-400/75 border-2 shadow-md)))
                         (name "description")
                         (required ""))
                      ,(unescape-str description))

            (div ,(hyg-submit-button (G_ "submit"))))))

  (html-template G_
                 #:body `((div (@ (class "container mx-auto"))
                               ,form_))
                 #:proj proj))

