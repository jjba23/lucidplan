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
  #:use-module (kracht prelude)
  #:use-module (lucidplan domain)
  #:use-module (hygguile hygguile)
  #:use-module (artanis security nss)
  #:use-module (artanis route)
  #:export (render-ticket render-ticket-listitem html-from-tickets
                          html-new-ticket-form html-edit-ticket-form
                          html-ticket-form))

(define (render-ticket G_ project ticket)
  (let* ( ;Extract Ticket Data (No more repetitive assoc-ref lookups)
          (id (assoc-ref ticket "id"))
         (human-id (assoc-ref ticket "human_id"))
         (title (assoc-ref ticket "title"))
         (status (assoc-ref ticket "ticket_status"))
         (type (assoc-ref ticket "ticket_type"))
         (desc (assoc-ref ticket "description"))
         (priority (assoc-ref ticket "priority"))

         ;; Consolidate Reusable CSS Classes
         (card-face (mk-class '(flex flex-col
                                     gap-2
                                     px-6
                                     py-10
                                     bg-stone-100
                                     dark:bg-onyx-900
                                     rounded-2xl
                                     dark:border-onyx-100/75
                                     border-stone-300/75
                                     shadow-md
                                     border-2)))
         (label-face (mk-class '(font-sans text-onyx-700 dark:text-stone-300
                                           text-md font-light)))
         (val-face (mk-class '(font-sans text-onyx-700 dark:text-stone-300
                                         text-md ml-4)))

         ;; Pre-calculate URLs and JS callbacks
         (toggle-js (format #f
                     "document.getElementById('details-~a').toggleAttribute('hidden')"
                     id))
         (edit-url (format #f "/tickets/~a/id/~a/edit" project human-id))
         (delete-url (format #f "/tickets/~a/delete" project)))
    
    ;; Render the clean SXML
    `((div (@ (class ,card-face))

           ;; Header
           (div (@ (class ,(mk-class '(flex flex-row justify-between))))
                ,(h4 (org2html title))
                (div (@ (class ,(mk-class '(flex justify-right))))
                     (span (@ (class ,(mk-class '(cursor-pointer)))
                              (onclick ,toggle-js))
                           ,(icon #:type 'x
                                  #:icon-css '(text-onyx-700
                                               dark:text-stone-300 text-xl)))))

           ;; Meta Info (Status & Type)
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                            justify-left))))

                ;; Status
                (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                     (small (@ (class ,label-face))
                            ,(G_ "status"))
                     (div (@ (class ,(mk-class '(flex gap-2 items-center))))
                          (span (@ (class ,(mk-class '(text-2xl))))
                                ,(emoji-for-ticket-status status))
                          ,(p status)))

                ;; Type
                (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                     (small (@ (class ,label-face))
                            ,(G_ "type"))
                     (div (@ (class ,(mk-class '(ml-4 flex gap-2 items-center))))
                          (span (@ (class ,(mk-class '(text-2xl))))
                                ,(emoji-for-ticket-type type))
                          ,(p type))))

           ;; Description
           (small (@ (class ,label-face))
                  ,(G_ "description"))
           (span (@ (class ,(mk-class '(text-md py-3
                                                text-onyx-700
                                                dark:text-stone-300
                                                font-medium
                                                font-sans
                                                max-w-prose
                                                whitespace-pre-wrap
                                                ml-4))))
                 ,(org2html desc))

           ,(hr)

           ;; Ticket Details
           (small (@ (class ,label-face))
                  ,(G_ "human id"))
           (small (@ (class ,val-face))
                  ,(format #f "~a-~s" project human-id))

           (small (@ (class ,label-face))
                  ,(G_ "priority"))
           (small (@ (class ,val-face))
                  ,priority)

           (small (@ (class ,label-face))
                  ,(G_ "id"))
           (small (@ (class ,val-face))
                  ,id)

           ,(hr)

           ;; Action Buttons
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4))))
                (a (@ (href ,edit-url))
                   ,(icon-button #:icon 'pencil
                                 #:content (G_ "edit ticket")
                                 #:icon-css '(text-md text-onyx-700
                                                      dark:text-stone-300)))
                (form (@ (enctype application/x-www-form-urlencoded)
                         (method POST)
                         (action ,delete-url))
                      (input (@ (hidden "")
                                (name id)
                                (value ,id)))
                      ,(danger-submit-button `(span (@ (class ,(mk-class '(items-center
                                                                           flex
                                                                           flex-row
                                                                           gap-1))))
                                                    (,(icon #:type 'trash
                                                            #:icon-css '(text-stone-300
                                                                         dark:text-stone-300
                                                                         text-lg)))
                                                    ,(G_ "delete")))))))))

(define (render-ticket-listitem G_ project ticket ticket-id-to-human-id)
  (let* ( ;Extract Ticket Data
          (id (assoc-ref ticket "id"))
         (human-id (assoc-ref ticket "human_id"))
         (title (assoc-ref ticket "title"))
         (type (assoc-ref ticket "ticket_type"))

         ;; Pre-calculate IDs, URLs, and JS
         (item-id (format #f "listitem-~a" id))
         (details-id (format #f "details-~a" id))
         ;; We can reuse the details-id variable directly in the toggle JS!
         (toggle-js (format #f
                     "document.getElementById('~a').toggleAttribute('hidden')"
                     details-id))
         (ticket-url (format #f "/tickets/~a/id/~a" project human-id))

         ;; Consolidate CSS Classes
         (wrapper-face (mk-class '(flex flex-col gap-2)))
         (header-face (mk-class '(flex flex-row
                                       gap-4
                                       items-center
                                       justify-left
                                       bg-stone-300/75
                                       dark:bg-onyx-700
                                       dark:border-onyx-100/75
                                       border-stone-300/75
                                       shadow-md
                                       border-2
                                       py-2
                                       px-4
                                       shadow
                                       cursor-pointer
                                       rounded-xl)))
         (title-face (mk-class '(font-sans text-onyx-700
                                           dark:text-stone-300
                                           text-md
                                           font-bold
                                           break-words
                                           max-w-196
                                           leading-6
                                           truncate)))
         (link-face (mk-class '(flex flex-row
                                     flex-wrap
                                     gap-2
                                     items-center
                                     justify-start
                                     no-underline))))
    
    ;; Render the clean SXML
    `((div (@ (class ,wrapper-face)
              (id ,item-id))

           ;; Clickable Header Row
           (div (@ (onclick ,toggle-js)
                   (class ,header-face))
                ,(emoji-for-ticket-type type)
                (h5 (@ (class ,title-face))
                    ,(org2html title))

                ;; Direct Link Icon
                (div (a (@ (class ,link-face)
                           (href ,ticket-url))
                        ,(icon #:type 'link
                               #:icon-css '(text-md text-onyx-700
                                            dark:text-stone-300
                                            hover:text-asparagus-500
                                            hover:dark:text-asparagus-300)))))

           ;; Expandable Details Container
           (div (@ (id ,details-id)
                   (class "ml-4")
                   (hidden ""))
                ,(render-ticket G_ project ticket))))))

(define* (html-from-tickets G_
                            #:key is-logged-in
                            (current-sort #f)
                            projects
                            project-details
                            tickets
                            ticket-id-to-human-id
                            with-done
                            with-rejected
                            search-query)
  (let* ((search-param-components `((div (@ (class ,(mk-class '(flex flex-row
                                                                flex-wrap
                                                                justify-between))))
                                         (div (@ (class ,(mk-class '(my-3 p-2
                                                                     flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-start
                                                                     align-center
                                                                     items-center))))
                                              ,(sort-select G_
                                                            #:project-id (assoc-ref
                                                                          project-details
                                                                          "human_id")
                                                            #:current-sort
                                                            current-sort))
                                         (div (@ (class ,(mk-class '(my-3 p-2
                                                                     flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-end
                                                                     align-center
                                                                     items-center))))
                                              ,(search-box G_
                                                           #:search-query
                                                           search-query)))))
         (ticket-list `((div (@ (id lucidplan-tickets-list)
                                (ondrop "dropTicketsItem(event)")
                                (ondragover "allowDrop(event)")
                                (class ,(mk-class '(flex flex-col gap-2))))
                             ,(map (lambda (t)
                                     (render-ticket-listitem G_
                                      (assoc-ref project-details "human_id") t
                                      ticket-id-to-human-id))
                                   (filter only-active-tickets-filter tickets)))))
         (done-ticket-list `(,(if with-done
                                  `(div (@ (id lucidplan-done-tickets-list)
                                           (class ,(mk-class '(my-4))))
                                        (hr)
                                        (h2 (@ (class ,(mk-class '(text-lg
                                                                   font-sans
                                                                   font-black
                                                                   text-onyx-700
                                                                   dark:text-stone-300
                                                                   flex
                                                                   flex-row
                                                                   gap-2
                                                                   my-4))))
                                            ,(emoji-for-ticket-status "done")
                                            (span ,(G_ "done")))
                                        (div (@ (class ,(mk-class '(flex
                                                                    flex-col
                                                                    gap-2))))
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
                                  '())))
         (rejected-ticket-list `(,(if with-rejected
                                      `(div (@ (id
                                                lucidplan-rejected-tickets-list)
                                               (class ,(mk-class '(my-4))))
                                            (hr)
                                            (h2 (@ (class ,(mk-class '(text-lg
                                                                       font-sans
                                                                       font-black
                                                                       text-onyx-700
                                                                       dark:text-stone-300
                                                                       flex
                                                                       flex-row
                                                                       gap-2
                                                                       my-4))))
                                                ,(emoji-for-ticket-status
                                                  "rejected")
                                                (span ,(G_ "rejected")))
                                            (div (@ (class ,(mk-class '(flex
                                                                        flex-col
                                                                        gap-2))))
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
                                      '())))
         (fallback-no-ticket-list `(,(p (G_ "Wow.. it's really empty here")))))
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(px-4 py-4))))
                                ((div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-4
                                                                  flex-grow))))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-wrap
                                                                  flex-row
                                                                  gap-4
                                                                  items-center
                                                                  justify-between))))
                                           (div (@ (class ,(mk-class '(flex
                                                                       flex-row
                                                                       gap-2
                                                                       items-center))))
                                                ,(icon #:type 'scroll
                                                       #:icon-css '(text-onyx-700
                                                                    dark:text-stone-300
                                                                    text-2xl))
                                                ,(h2 (format #f
                                                             "backlog of ~a"
                                                             (assoc-ref
                                                              project-details
                                                              "title"))))
                                           (div (@ (class ,(mk-class '(flex
                                                                       flex-row
                                                                       flex-wrap
                                                                       gap-1
                                                                       items-center))))
                                                (div (a (@ (href ,(format #f
                                                                   "/kanban/~a"
                                                                   (assoc-ref
                                                                    project-details
                                                                    "human_id"))))
                                                        ,(icon-button #:icon 'kanban
                                                                      #:content
                                                                      (G_
                                                                       "kanban")
                                                                      #:icon-css '
                                                                      (text-md
                                                                       text-onyx-700
                                                                       dark:text-stone-300))))
                                                (a (@ (href ,(format #f
                                                              "/tickets/~a/new"
                                                              (assoc-ref
                                                               project-details
                                                               "human_id"))))
                                                   ,(icon-button #:icon 'plus
                                                                 #:content (G_
                                                                            "new ticket")
                                                                 #:icon-css '(text-md
                                                                              text-onyx-700
                                                                              dark:text-stone-300)))))
                                      ,(render-if-nestring (lambda (x)
                                                             `(em (@ (class ,(mk-class '
                                                                              (text-sm
                                                                               text-onyx-700
                                                                               font-sans
                                                                               dark:text-stone-300))))
                                                                  ,x))
                                                           (assoc-ref
                                                            project-details
                                                            "description"))

                                      (div (details (summary (@ (class ,(mk-class '
                                                                         (text-stone-600
                                                                          dark:text-stone-300
                                                                          text-sm
                                                                          font-sans
                                                                          cursor-pointer))))
                                                             ,(G_
                                                               "backlog options"))
                                                    (div (@ (class ,(mk-class '
                                                                              (flex
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
                                                                (span (@ (class ,
                                                                          (mk-class '
                                                                           (text-stone-600
                                                                            dark:text-stone-300
                                                                            text-sm))))
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
                                                                (span (@ (class ,
                                                                          (mk-class '
                                                                           (text-stone-600
                                                                            dark:text-stone-300
                                                                            text-sm))))
                                                                      ,(G_
                                                                        "show rejected?"))))))

                                      ,search-param-components
                                      ,(cond
                                         ((nil? ticket-list)
                                          fallback-no-ticket-list)
                                         (else ticket-list))
                                      ,done-ticket-list
                                      ,rejected-ticket-list)))
                   #:proj projects)))

(define* (html-new-ticket-form G_
                               #:key projects project is-logged-in)
  (html-ticket-form G_
                    #:is-logged-in is-logged-in
                    #:project project
                    #:projects projects
                    #:action (format #f "/tickets/~a/new" project)))

(define* (html-edit-ticket-form G_
                                #:key projects
                                project
                                t
                                is-logged-in)
  (html-ticket-form G_
                    #:is-logged-in is-logged-in
                    #:project project
                    #:is-edit #t
                    #:title (assoc-ref t "title")
                    #:description (assoc-ref t "description")
                    #:ticket-type (assoc-ref t "ticket_type")
                    #:ticket-status (assoc-ref t "ticket_status")
                    #:priority (assoc-ref t "priority")
                    #:action (format #f "/tickets/~a/id/~a/edit" project
                                     (assoc-ref t "human_id"))
                    #:projects projects))

(define* (html-ticket-form G_
                           #:key (project #f)
                           (title "")
                           (description "")
                           (ticket-status "")
                           (ticket-type "")
                           (priority 0)
                           (action "")
                           (is-edit #f)
                           (projects '())
                           (is-logged-in #f))
  (define form_
    `((div (form (@ (enctype "application/x-www-form-urlencoded")
                    (method POST)
                    (action ,action))

                 (div (@ (class ,(mk-class '(flex flex-col gap-8))))

                      ,(input #:label (G_ "title")
                              #:name "title"
                              #:value (html-unescape title)
                              #:required #t)
                      (div (@ (class ,(mk-class '(max-w-128))))
                           (label (@ (class ,(mk-class '(text-onyx-700
                                                         dark:text-stone-300))))
                                  ,(G_ "status"))
                           (div ,(select #:name 'ticket-status
                                         #:on-change ""
                                         #:options (list (glyph-option #:value
                                                          "todo"
                                                          #:glyph (emoji-for-ticket-status
                                                                   "todo")
                                                          #:content (G_
                                                                     "To-do")
                                                          #:selected-value
                                                          ticket-status)
                                                         (glyph-option #:value
                                                          "work-in-progress"
                                                          #:glyph (emoji-for-ticket-status
                                                                   "work-in-progress")
                                                          #:content (G_
                                                                     "Work in Progress")
                                                          #:selected-value
                                                          ticket-status)
                                                         (glyph-option #:value
                                                          "in-review"
                                                          #:glyph (emoji-for-ticket-status
                                                                   "in-review")
                                                          #:content (G_
                                                                     "In Review")
                                                          #:selected-value
                                                          ticket-status)
                                                         (glyph-option #:value
                                                          "done"
                                                          #:glyph (emoji-for-ticket-status
                                                                   "done")
                                                          #:content (G_ "Done")
                                                          #:selected-value
                                                          ticket-status)
                                                         (glyph-option #:value
                                                          "rejected"
                                                          #:glyph (emoji-for-ticket-status
                                                                   "rejected")
                                                          #:content (G_
                                                                     "Rejected")
                                                          #:selected-value
                                                          ticket-status))))

                           )
                      (div (@ (class ,(mk-class '(max-w-128))))
                           (label (@ (class ,(mk-class '(text-onyx-700
                                                         dark:text-stone-300))))
                                  ,(G_ "type"))
                           (div ,(select #:name 'ticket-type
                                         #:on-change ""
                                         #:options (list (glyph-option #:value
                                                          "task"
                                                          #:glyph (emoji-for-ticket-type
                                                                   "task")
                                                          #:content (G_ "Task")
                                                          #:selected-value
                                                          ticket-type)
                                                         (glyph-option #:value
                                                          "bug"
                                                          #:glyph (emoji-for-ticket-type
                                                                   "bug")
                                                          #:content (G_ "Bug")
                                                          #:selected-value
                                                          ticket-type)
                                                         (glyph-option #:value
                                                          "story"
                                                          #:glyph (emoji-for-ticket-type
                                                                   "story")
                                                          #:content (G_
                                                                     "Story")
                                                          #:selected-value
                                                          ticket-type)
                                                         (glyph-option #:value
                                                          "spike"
                                                          #:glyph (emoji-for-ticket-type
                                                                   "spike")
                                                          #:content (G_
                                                                     "Spike")
                                                          #:selected-value
                                                          ticket-type)
                                                         (glyph-option #:value
                                                          "time-box"
                                                          #:glyph (emoji-for-ticket-type
                                                                   "time-box")
                                                          #:content (G_
                                                                     "Time Box")
                                                          #:selected-value
                                                          ticket-type)))))
                      (div (@ (class ,(mk-class '(max-w-128))))
                           ,(input #:label (G_ "priority")
                                   #:name "priority"
                                   #:required #f
                                   #:type 'number
                                   #:value (if (not priority) 0 priority)))

                      ,(textarea #:label (G_ "description")
                                 #:name "description"
                                 #:required #f
                                 #:value (html-unescape description))

                      (div ,(submit-button (G_ "submit"))))))))

  (html-template G_
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(mk-class '(px-4 py-4))))
                               (div ,form_)))
                 #:proj projects))

