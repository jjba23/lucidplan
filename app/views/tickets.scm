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
  #:use-module (srfi srfi-1)
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
  #:export (render-ticket render-ticket-listitem html-from-tickets
                          html-new-ticket-form html-edit-ticket-form
                          html-ticket-form))

;; =============================================================================
;; Reusable Styles
;; =============================================================================

(define %card-face
  (mk-class '(flex flex-col
                   gap-2
                   px-3
                   sm:px-4
                   md:px-6
                   py-4
                   sm:py-6
                   md:py-8
                   bg-stone-100
                   dark:bg-onyx-900
                   rounded-xl
                   md:rounded-2xl
                   dark:border-onyx-100/75
                   border-stone-300/75
                   shadow-md
                   border-2)))

(define %label-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-md font-light)))

(define %val-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-md ml-4)))

(define %mono-face
  (mk-class '(font-sans text-onyx-700 dark:text-stone-300 text-sm ml-4
                        font-mono)))

(define %list-wrapper-face
  (mk-class '(flex flex-col gap-2)))

(define %list-header-face
  (mk-class '(flex flex-row
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

(define %list-title-face
  (mk-class '(font-sans text-sm
                        sm:text-md
                        font-bold
                        break-words
                        flex-grow
                        leading-5
                        line-clamp-2
                        sm:line-clamp-none)))

(define %list-link-face
  (mk-class '(flex flex-row
                   flex-wrap
                   gap-2
                   items-center
                   justify-start
                   no-underline)))

;; =============================================================================
;;  UI Helper Components
;; =============================================================================

(define (render-meta-badge label icon value)
  `(div (@ (class ,(mk-class '(flex flex-col gap-3))))
        (small (@ (class ,%label-face))
               ,label)
        (div (@ (class ,(mk-class '(ml-4 flex gap-2 items-center))))
             (span (@ (class ,(mk-class '(text-2xl))))
                   ,icon)
             (p (@ (class ,%label-face))
                ,value))))

(define (render-field label value val-class)
  `((small (@ (class ,%label-face))
           ,label)
    (small (@ (class ,val-class))
           ,(format #f "~a" value))))

(define (render-toggle-button label js-action)
  `(button (@ (class ,(mk-class '(text-sm font-semibold
                                          underline
                                          cursor-pointer
                                          text-onyx-700
                                          dark:text-stone-300
                                          hover:text-onyx-900
                                          hover:dark:text-stone-100)))
              (onclick ,js-action))
           ,label))

(define* (render-ticket G_
                        #:key project ticket
                        (assignee #f))
  (let* ((t-ref (lambda (k)
                  (assq-ref ticket k)))
         (id (t-ref 'id))
         (human-id (t-ref 'human-id))
         (title (t-ref 'title))
         (status (t-ref 'ticket-status))
         (type (t-ref 'ticket-type))
         (desc (t-ref 'description))
         (priority (t-ref 'priority))

         (toggle-js (format #f
                     "document.getElementById('details-~a').toggleAttribute('hidden')"
                     id))
         (edit-url (format #f "/tickets/~a/id/~a/edit" project human-id))
         (delete-url (format #f "/tickets/~a/delete" project)))

    `(div (@ (class ,%card-face))

          ;; Header
          (div (@ (class ,(mk-class '(flex flex-row justify-between))))
               ,(h4 (org2html (html-unescape title)))
               (div (@ (class ,(mk-class '(flex justify-right))))
                    (span (@ (class ,(mk-class '(cursor-pointer)))
                             (onclick ,toggle-js))
                          ,(icon #:type 'x
                                 #:icon-css '(text-onyx-700
                                              dark:text-stone-300 text-xl)))))

          ;; Meta Info (Status & Type)
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-6
                                           justify-left))))
               ,(render-meta-badge (G_ "status")
                                   (emoji-for-ticket-status status) status)
               ,(render-meta-badge (G_ "type")
                                   (emoji-for-ticket-type type) type))

          ;; Description
          ,(render-if-nestring (lambda (d)
                                 `((small (@ (class ,%label-face))
                                          ,(G_ "description"))
                                   (span (@ (class ,(mk-class '(text-md py-3
                                                                text-onyx-700
                                                                dark:text-stone-300
                                                                font-medium
                                                                font-sans
                                                                max-w-prose
                                                                whitespace-pre-wrap
                                                                leading-7
                                                                ml-4))))
                                         ,(org2html (html-unescape d))))) desc)

          ;; Ticket Details
          (small (@ (class ,%mono-face))
                 ,(format #f "~a-~s" project human-id))

          ,@(if (and (not (nil? priority))
                     (> priority 0))
                (render-field (G_ "priority") priority %val-face)
                '())

          ,@(if (not (nil? assignee))
                (render-field (G_ "assignee") assignee %val-face)
                '())

          ;; Action Buttons
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4 py-3))))
               (a (@ (href ,edit-url))
                  ,(icon-button #:icon 'pencil
                                #:content (G_ "edit ticket")
                                #:icon-css '(text-md text-onyx-700
                                                     dark:text-stone-300)))

               (form (@ (enctype application/x-www-form-urlencoded)
                        (method POST)
                        (action ,delete-url))
                     (input (@ (hidden "")
                               (name "id")
                               (value ,id)))
                     ,(danger-submit-button `(span (@ (class ,(mk-class '(items-center
                                                                          flex
                                                                          flex-row
                                                                          gap-1))))
                                                   ,(icon #:type 'trash
                                                          #:icon-css '(text-stone-300
                                                                       dark:text-stone-300
                                                                       text-lg))
                                                   ,(G_ "delete"))))))))

(define* (render-ticket-listitem G_
                                 #:key project
                                 ticket
                                 ticket-id-to-human-id
                                 ticket-assignee
                                 user-names)
  (let* ((t-ref (lambda (k)
                  (assq-ref ticket k)))
         (id (t-ref 'id))
         (human-id (t-ref 'human-id))
         (title (t-ref 'title))
         (type (t-ref 'ticket-type))
         (priority (or (t-ref 'priority) 0))

         (item-id (format #f "listitem-~a" id))
         (details-id (format #f "details-~a" id))
         (toggle-js (format #f
                     "document.getElementById('~a').toggleAttribute('hidden')"
                     details-id))
         (ticket-url (format #f "/tickets/~a/id/~a" project human-id))
         (assignee-user (find (lambda (u)
                                (equal? (assq-ref u
                                                  'id) ticket-assignee))
                              user-names)))

    `(div (@ (class ,(string-append %list-wrapper-face
                                    " cursor-grab active:cursor-grabbing"))
             (id ,item-id)
             (draggable "true")
             (data-ticket-id ,human-id)
             (data-priority ,(number->string priority))
             (ondragstart "dragTicketStart(event)")
             (ondragend "dragTicketEnd(event)"))

          ;; Clickable Header Row
          (div (@ (onclick ,toggle-js)
                  (tabindex "0")
                  (onkeydown
                   "if(event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }")
                  (class ,%list-header-face))

               ,(emoji-for-ticket-type type)
               (h5 (@ (class ,%list-title-face))
                   ,(org2html (html-unescape title)))

               ;; Direct Link Icon
               (div (a (@ (class ,%list-link-face)
                          (href ,ticket-url))
                       ,(icon #:type 'link
                              #:icon-css '(text-md text-onyx-700
                                           dark:text-stone-300
                                           hover:text-asparagus-500
                                           hover:dark:text-asparagus-300)))))

          ;; Expandable Details Container
          (div (@ (id ,details-id)
                  (class ,(mk-class '(ml-1 sm:ml-3)))
                  (hidden ""))
               ,(render-ticket G_
                               #:project project
                               #:ticket ticket
                               #:assignee (if (nil? assignee-user) #f
                                              (assq-ref assignee-user
                                                        'username)))))))

(define (expand-and-tips G_)
  `(div (@ (class ,(mk-class '(flex flex-row
                                    flex-wrap
                                    justify-between
                                    items-center
                                    my-1
                                    gap-4))))
        (p (@ (class ,(mk-class '(text-sm text-onyx-500 dark:text-stone-500))))
           ,(G_
             "Tip: Use Tab to navigate tickets and Enter/Space to expand or collapse cards."))

        (div (@ (class ,(mk-class '(flex flex-row gap-4))))
             ,(render-toggle-button (G_ "Expand All")
               "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = false)")

             ,(render-toggle-button (G_ "Collapse All")
               "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = true)"))))

(define* (html-from-tickets G_
                            #:key is-logged-in
                            (current-sort #f)
                            projects
                            project-details
                            tickets
                            ticket-id-to-human-id
                            user-names
                            ticket-assignee
                            with-done
                            with-rejected
                            search-query)
  (let* ( ;Core Data
          (proj-human-id (assq-ref project-details
                                   'human-id))
         (proj-title (assq-ref project-details
                               'title))
         (proj-desc (assq-ref project-details
                              'description))

         ;; CSS Faces
         (header-face (mk-class '(flex flex-row gap-2 items-center)))
         (btn-row-face (mk-class '(flex flex-row flex-wrap gap-1 items-center)))
         (label-face (mk-class '(text-stone-600 dark:text-stone-300 text-sm)))
         (icon-face '(text-onyx-700 dark:text-stone-300 text-2xl))

         ;; JS Handlers
         (done-js (string-append "window.location.href = `/tickets/"
                   proj-human-id
                   "?with-done=${document.getElementById('should-show-done').checked}`;"))
         (rejected-js (string-append "window.location.href = `/tickets/"
                       proj-human-id
                       "?with-rejected=${document.getElementById('should-show-rejected').checked}`;"))

         ;; Search Component
         (search-param-components `((div (@ (class ,(mk-class '(flex flex-row
                                                                flex-wrap
                                                                gap-1 sm:gap-2
                                                                justify-between))))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-start
                                                                     items-center))))
                                              ,(sort-select G_
                                                            #:project-id
                                                            proj-human-id
                                                            #:current-sort
                                                            current-sort))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     flex-wrap
                                                                     justify-end
                                                                     items-center))))
                                              ,(search-box G_
                                                           #:search-query
                                                           search-query)))))

         ;; List Generators
         (tickets (filter only-tickets-filter tickets))
         (done-tickets (filter only-done-tickets-filter tickets))
         (rejected-tickets (filter only-rejected-tickets-filter tickets))
         (ticket-list `((div (@ (id lucidplan-tickets-list)
                                (data-project-id ,proj-human-id)
                                (ondragover "allowDrop(event)")
                                (ondrop "dropTicketsItem(event)")
                                (class ,(mk-class '(flex flex-col gap-2))))
                             ,(map (lambda (t)
                                     (let* ((tt-assignee (find (lambda (a)
                                                                 (equal? (assq-ref
                                                                          t
                                                                          'id)
                                                                         (assq-ref
                                                                          a
                                                                          'ticket-id)))
                                                          ticket-assignee))
                                            (ttt-assignee (if (nil?
                                                               tt-assignee) #f
                                                              (assq-ref
                                                               tt-assignee
                                                               'user-id))))
                                       (render-ticket-listitem G_
                                        #:project proj-human-id
                                        #:ticket t
                                        #:ticket-id-to-human-id
                                        ticket-id-to-human-id
                                        #:ticket-assignee ttt-assignee
                                        #:user-names user-names))) tickets))))

         (done-ticket-list (if with-done
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
                                     (div (@ (class ,(mk-class '(flex flex-col
                                                                 gap-2))))
                                          ,(map (lambda (t)
                                                  (render-ticket-listitem G_
                                                   #:project proj-human-id
                                                   #:ticket t
                                                   #:ticket-id-to-human-id
                                                   ticket-id-to-human-id
                                                   #:ticket-assignee
                                                   ticket-assignee
                                                   #:user-names user-names))
                                                done-tickets)))
                               '()))

         (rejected-ticket-list (if with-rejected
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
                                                       #:project proj-human-id
                                                       #:ticket t
                                                       #:ticket-id-to-human-id
                                                       ticket-id-to-human-id
                                                       #:ticket-assignee
                                                       ticket-assignee
                                                       #:user-names user-names))
                                                    rejected-tickets)))
                                   '()))

         (fallback-no-ticket-list `(,(p (G_ "Wow.. it's really empty here")))))

    ;; Build the final HTML View
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:proj projects
                   #:body `(div (@ (class ,(mk-class '(px-2 sm:px-4
                                                            md:px-6
                                                            pt-1
                                                            sm:pt-2
                                                            pb-3
                                                            sm:pb-5))))
                                ((div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-2
                                                                  sm:gap-4
                                                                  flex-grow))))

                                      ;; Header Section
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-wrap
                                                                  flex-row
                                                                  gap-2
                                                                  sm:gap-4
                                                                  items-center
                                                                  justify-between))))
                                           (div (@ (class ,header-face))
                                                ,(icon #:type 'scroll
                                                       #:icon-css icon-face)
                                                ,(h2 (format #f
                                                             "backlog of ~a"
                                                             proj-title)))

                                           (div (@ (class ,btn-row-face))
                                                (div (a (@ (href ,(format #f
                                                                   "/kanban/~a"
                                                                   proj-human-id)))
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
                                                              proj-human-id)))
                                                   ,(icon-button #:icon 'plus
                                                                 #:content (G_
                                                                            "new ticket")
                                                                 #:icon-css '(text-md
                                                                              text-onyx-700
                                                                              dark:text-stone-300)))))

                                      ;; Description
                                      ,(render-if-nestring (lambda (x)
                                                             `(em (@ (class ,(mk-class '
                                                                              (text-sm
                                                                               text-onyx-700
                                                                               font-sans
                                                                               dark:text-stone-300
                                                                               break-words
                                                                               line-clamp-3
                                                                               sm:line-clamp-none
                                                                               max-w-full))))
                                                                  ,x))
                                                           proj-desc)

                                      ;; Options (Checkboxes)
                                      (div (details (summary (@ (class ,(mk-class '
                                                                         (text-stone-600
                                                                          dark:text-stone-300
                                                                          text-sm
                                                                          font-sans
                                                                          cursor-pointer
                                                                          py-2))))
                                                             ,(G_
                                                               "backlog options"))
                                                    (div (@ (class ,(mk-class '
                                                                              (flex
                                                                               flex-wrap
                                                                               flex-row
                                                                               gap-2
                                                                               sm:gap-4
                                                                               my-3))))

                                                         ;; Done Checkbox
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
                                                                   (onchange ,done-js)))
                                                         (span (@ (class ,label-face))
                                                               ,(G_
                                                                 "show done?"))

                                                         ;; Rejected Checkbox
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
                                                                   (onchange ,rejected-js)))
                                                         (span (@ (class ,label-face))
                                                               ,(G_
                                                                 "show rejected?")))))

                                      ;; Sub-Components & Lists
                                      ,search-param-components
                                      ,(expand-and-tips G_)
                                      ,(if (nil? tickets)
                                           fallback-no-ticket-list ticket-list)
                                      ,done-ticket-list
                                      ,rejected-ticket-list))))))

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
                                assignee
                                is-logged-in)
  (html-ticket-form G_
                    #:is-logged-in is-logged-in
                    #:project project
                    #:is-edit #t
                    #:title (assq-ref t
                                      'title)
                    #:description (assq-ref t
                                            'description)
                    #:ticket-type (assq-ref t
                                            'ticket-type)
                    #:ticket-status (assq-ref t
                                              'ticket-status)
                    #:assignee assignee
                    #:priority (assq-ref t
                                         'priority)
                    #:action (format #f "/tickets/~a/id/~a/edit" project
                                     (assq-ref t
                                               'human-id))
                    #:projects projects))

(define* (html-ticket-form G_
                           #:key (project #f)
                           (title "")
                           (description "")
                           (ticket-status "")
                           (ticket-type "")
                           (priority 0)
                           (assignee "")
                           (action "")
                           (is-edit #f)
                           (projects '())
                           (is-logged-in #f))

  (let* ( ;Consolidate Reusable CSS Classes
          (form-face (mk-class '(flex flex-col gap-6)))
         (field-face (mk-class '(max-w-128)))
         (label-face (mk-class '(text-onyx-700 dark:text-stone-300)))

         ;; Format Data Safely
         (safe-title (html-unescape title))
         (safe-desc (html-unescape description))
         (safe-prio (or priority 0))

         ;; Dynamically generate dropdown options using `map`
         (status-opts (map (lambda (opt)
                             (glyph-option #:value (car opt)
                                           #:glyph (emoji-for-ticket-status (car
                                                                             opt))
                                           #:content (G_ (cadr opt))
                                           #:selected-value ticket-status))
                           '(("todo" "To-do")
                             ("work-in-progress" "Work in Progress")
                             ("in-review" "In Review")
                             ("done" "Done")
                             ("rejected" "Rejected"))))

         (type-opts (map (lambda (opt)
                           (glyph-option #:value (car opt)
                                         #:glyph (emoji-for-ticket-type (car
                                                                         opt))
                                         #:content (G_ (cadr opt))
                                         #:selected-value ticket-type))
                         '(("task" "Task")
                           ("bug" "Bug")
                           ("story" "Story")
                           ("spike" "Spike")
                           ("time-box" "Time Box"))))

         ;; Build the Form SXML
         (form-sxml `(form (@ (enctype "application/x-www-form-urlencoded")
                              (method POST)
                              (action ,action))

                           (div (@ (class ,form-face))

                                ;; Title
                                ,(input #:label (G_ "title")
                                        #:name "title"
                                        #:value safe-title
                                        #:required #t)

                                ;; Status
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "status"))
                                     (div ,(select #:name 'ticket-status
                                                   #:on-change ""
                                                   #:options status-opts)))

                                ;; Type
                                (div (@ (class ,field-face))
                                     (label (@ (class ,label-face))
                                            ,(G_ "type"))
                                     (div ,(select #:name 'ticket-type
                                                   #:on-change ""
                                                   #:options type-opts)))

                                ;; Priority
                                (div (@ (class ,field-face))
                                     ,(input #:label (G_ "priority")
                                             #:name "priority"
                                             #:required #f
                                             #:type 'number
                                             #:value safe-prio))

                                ;; Assignee
                                ,(if is-edit
                                     `((div ,(input #:label (G_ "assignee")
                                                    #:name "assignee"
                                                    #:value assignee
                                                    #:required #f)))
                                     '())

                                ;; Description
                                ,(textarea #:label (G_ "description")
                                           #:name "description"
                                           #:required #f
                                           #:value safe-desc
                                           #:textarea-css '(leading-7 p-4
                                                            rounded-xl
                                                            border-stone-700
                                                            dark:border-onyx-100
                                                            dark:text-stone-300
                                                            dark:bg-onyx-700/75
                                                            bg-stone-300/75
                                                            border-2
                                                            min-h-72
                                                            min-w-90
                                                            shadow-md
                                                            min-w-prose))

                                ;; Submit Button
                                (div ,(submit-button (G_ "submit")))))))

    ;; Render final template
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4 py-4))))
                                 ,form-sxml))
                   #:proj projects)))
