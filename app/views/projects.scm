;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views projects)
  #:use-module (web uri)
  #:use-module (ice-9 format)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (kracht prelude)
  #:use-module (hygguile hygguile)
  #:export (render-project render-project-list-item html-from-projects
                           html-new-project-form html-edit-project-form
                           html-project-form))

;;; ----------------------------------------------------------------------------
;;; Data Access Helpers
;;; ----------------------------------------------------------------------------

(define (proj-ref p key . default)
  "Safely retrieve KEY from project association list P, falling back to DEFAULT or empty string."
  (let ((val (assq-ref p key)))
    (if val val
        (if (null? default) ""
            (car default)))))

(define (proj-decoded-url p key)
  "Retrieve and URI-decode a URL field from project P."
  (uri-decode (proj-ref p key "")))

;;; ----------------------------------------------------------------------------
;;; UI Helpers & Sub-components
;;; ----------------------------------------------------------------------------

(define (toggle-details-js id)
  (format #f "document.getElementById('details-~a').toggleAttribute('hidden')"
          id))

(define (project-external-link G_ href label)
  `(a (@ (class ,(mk-class '(text-onyx-700 dark:text-stone-300 no-underline)))
         (href ,href))
      ,(icon-button #:icon 'link
                    #:content (G_ label)
                    #:icon-css '(text-md text-onyx-700 dark:text-stone-300))))

(define (project-links-bar G_ p)
  (let ((title (proj-ref p
                         'title))
        (human-id (proj-ref p
                            'human-id))
        (url (proj-decoded-url p
                               'url))
        (vcs-url (proj-decoded-url p
                                   'vcs-url)))
    `(div (@ (class ,(mk-class '(flex flex-wrap flex-row gap-3))))
          (a (@ (href ,(format #f "/tickets/~a" human-id)))
             ,(icon-button #:icon 'scroll
                           #:content (format #f "backlog: ~a" title)
                           #:icon-css '(text-md text-onyx-700
                                                dark:text-stone-300)))
          (a (@ (class ,(mk-class '(text-onyx-700 dark:text-stone-300)))
                (href ,(format #f "/kanban/~a" human-id)))
             ,(icon-button #:icon 'kanban
                           #:content (format #f "kanban: ~a" title)
                           #:icon-css '(text-md text-onyx-700
                                                dark:text-stone-300)))
          ,(render-if-nestring (lambda (x)
                                 (project-external-link G_ x "view live")) url)
          ,(render-if-nestring (lambda (x)
                                 (project-external-link G_ x "project code"))
                               vcs-url))))

(define (project-actions-bar G_ p)
  (let ((id (proj-ref p
                      'id))
        (human-id (proj-ref p
                            'human-id)))
    `(div (@ (class ,(mk-class '(flex flex-wrap
                                      flex-row
                                      items-center
                                      items-center
                                      gap-4
                                      py-1))))
          (a (@ (href ,(format #f "/projects/id/~a/edit" human-id)))
             ,(icon-button #:icon 'pencil
                           #:content (G_ "edit project")
                           #:icon-css '(text-md text-onyx-700
                                                dark:text-stone-300)))
          (form (@ (enctype "application/x-www-form-urlencoded")
                   (method "POST")
                   (action "/projects/delete"))
                (input (@ (name "id")
                          (hidden "")
                          (value ,id)) "")
                ,(danger-submit-button `(span (@ (class ,(mk-class '(items-center
                                                                     flex
                                                                     flex-row
                                                                     gap-1))))
                                              ,(icon #:type 'trash
                                                     #:icon-css '(text-md
                                                                  text-stone-300))
                                              ,(G_ "delete"))))
          (small (@ (class ,(mk-class '(font-mono text-onyx-700
                                                  dark:text-stone-500 text-sm))))
                 ,human-id))))

;;; ----------------------------------------------------------------------------
;;; Core View Components
;;; ----------------------------------------------------------------------------

(define (render-project G_ p)
  (let* ((id (proj-ref p
                       'id))
         (title (proj-ref p
                          'title))
         (description (proj-ref p
                                'description))
         (toggle-js (toggle-details-js id)))
    `((div (@ (class ,(mk-class (append '(flex flex-col
                                               gap-2
                                               px-3
                                               sm:px-4
                                               md:px-6
                                               py-3
                                               sm:py-6
                                               md:py-8
                                               rounded-2xl
                                               border-2
                                               border-stone-300/75
                                               dark:border-onyx-100/75
                                               shadow-md) frosted-tint-classes))))
           ;; Header
           (div (@ (class ,(mk-class '(flex flex-row justify-between
                                            items-start gap-2))))
                ,(h4 title)
                (div (@ (class ,(mk-class '(flex justify-end pt-1))))
                     (span (@ (class ,(mk-class '(cursor-pointer)))
                              (onclick ,toggle-js))
                           ,(icon #:type 'x
                                  #:icon-css '(text-onyx-700
                                               dark:text-stone-300 text-xl)))))

           ;; Description
           ,(render-if-nestring (lambda (x)
                                  `(p (@ (class ,(mk-class '(text-sm
                                                             sm:text-md
                                                             py-1
                                                             sm:py-3
                                                             leading-5
                                                             sm:leading-6
                                                             text-onyx-700
                                                             dark:text-stone-300
                                                             whitespace-pre-wrap
                                                             font-medium
                                                             wrap-break-word))))
                                      ,x))
                                (org2html (html-unescape description)))

           ,(hr #:margin '(my-2 sm:my-3 mx-1 sm:mx-3))

           ;; Links & Actions
           ,(project-links-bar G_ p)
           ,(project-actions-bar G_ p)))))

(define (render-project-list-item G_ p)
  (let* ((id (proj-ref p
                       'id))
         (title (proj-ref p
                          'title))
         (description (proj-ref p
                                'description))
         (toggle-js (toggle-details-js id)))
    `((div (@ (class ,(mk-class (append '(flex flex-row
                                               flex-wrap
                                               gap-1
                                               sm:gap-4
                                               items-center
                                               justify-between
                                               border-2
                                               border-stone-300/75
                                               dark:border-onyx-100/75
                                               shadow-sm
                                               py-2
                                               px-3
                                               sm:px-4
                                               cursor-pointer
                                               rounded-xl
                                               w-full
                                               focus:outline-none
                                               focus:ring-2)
                                        frosted-tint-classes)))
              (tabindex "0")
              (onclick ,toggle-js)
              (onkeydown
               "if(event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }"))
           (h3 (@ (class ,(mk-class '(flex flex-row items-center gap-2
                                           max-w-full truncate))))
               ,(icon #:type 'list
                      #:icon-css '(text-onyx-700 dark:text-stone-300 text-lg))
               (span (@ (class ,(mk-class '(font-sans text-onyx-700
                                                      dark:text-stone-300
                                                      text-md
                                                      sm:text-lg
                                                      font-bold
                                                      truncate))))
                     ,title))
           ,(org2html (html-unescape (format #f "~a" description))
                      #:extra-paragraph-classes '(truncate italic
                                                  text-onyx-300
                                                  dark:text-stone-700 text-md)))
      (div (@ (id ,(format #f "details-~a" id))
              (hidden ""))
           ,(render-project G_ p)))))

(define (projects-header-title G_ search-query)
  `(div (@ (class ,(mk-class '(flex flex-col
                                    sm:flex-row
                                    justify-between
                                    w-full
                                    gap-3
                                    sm:gap-1
                                    items-start
                                    sm:items-center))))
        (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-1
                                         sm:gap-2))))
             ,(icon #:type 'align-bottom
                    #:icon-css '(text-onyx-700 dark:text-stone-300 text-2xl))
             ,(h2 (G_ "Projects"))
             (div (@ (class ,(mk-class '(ml-2))))
                  (a (@ (href "/projects/new"))
                     ,(icon-button #:icon 'plus
                                   #:content (G_ "new project")
                                   #:size '(text-xs sm:text-sm)
                                   #:icon-css '(text-sm text-onyx-700
                                                        dark:text-stone-300)))))
        (div (@ (class ,(mk-class '(w-full sm:w-auto flex justify-start
                                           sm:justify-end))))
             ,(search-box G_
                          #:search-query search-query))))

(define (projects-toolbar G_)
  `(div (@ (class ,(mk-class '(flex flex-col
                                    sm:flex-row
                                    justify-between
                                    items-start
                                    sm:items-center
                                    my-2
                                    gap-2
                                    sm:gap-4))))
        (p (@ (class ,(mk-class '(text-xs sm:text-sm text-onyx-500
                                          dark:text-stone-500 leading-6))))
           ,(G_
             "Tip: Use Tab to navigate projects and Enter/Space to expand or collapse cards."))
        (div (@ (class ,(mk-class '(flex flex-row gap-3 sm:gap-4 pt-1 sm:pt-0))))
             (button (@ (class ,(mk-class '(text-sm font-semibold
                                                    underline
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300
                                                    hover:text-onyx-900
                                                    dark:hover:text-stone-100)))
                        (onclick
                         "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = false)"))
                     ,(G_ "Expand All"))
             (button (@ (class ,(mk-class '(text-sm font-semibold
                                                    underline
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300
                                                    hover:text-onyx-900
                                                    dark:hover:text-stone-100)))
                        (onclick
                         "document.querySelectorAll('[id^=\"details-\"]').forEach(el => el.hidden = true)"))
                     ,(G_ "Collapse All")))))

(define* (html-from-projects G_
                             #:key projects is-logged-in search-query)
  (html-template G_
                 #:title (G_ "projects")
                 #:canonical-path "/projects"
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(mk-class '(px-3 pb-6))))
                               ((div (@ (class ,(mk-class '(flex flex-col
                                                                 gap-4 grow))))
                                     ,(projects-header-title G_ search-query)
                                     ,(projects-toolbar G_)
                                     (div (@ (id lucidplan-projects-list)
                                             (class ,(mk-class '(flex flex-col
                                                                 gap-2
                                                                 container
                                                                 my-3))))
                                          ,(map (lambda (x)
                                                  (render-project-list-item G_
                                                   x)) projects))))))
                 #:proj projects))

(define* (html-new-project-form G_
                                #:key projects is-logged-in)
  (html-project-form G_
                     #:projects projects
                     #:page-title (G_ "new project")
                     #:canonical-path "/projects/new"
                     #:is-logged-in is-logged-in))

(define* (html-edit-project-form G_
                                 #:key projects project is-logged-in)
  (html-project-form G_
                     #:is-logged-in is-logged-in
                     #:title (proj-ref project
                                       'title)
                     #:human-id (proj-ref project
                                          'human-id)
                     #:url (proj-ref project
                                     'url)
                     #:vcs-url (proj-ref project
                                         'vcs-url)
                     #:description (html-unescape (proj-ref project
                                                            'description))
                     #:ticket-statuses (proj-ref project
                                                 'ticket-statuses)
                     #:action (format #f "/projects/id/~a/edit"
                                      (proj-ref project
                                                'human-id))
                     #:page-title (G_ "edit project")
                     #:canonical-path "/projects"
                     #:projects projects))

(define* (html-project-form G_
                            #:key (title "")
                            (human-id "")
                            (description "")
                            (ticket-statuses "")
                            (url "")
                            (vcs-url "")
                            (action "/projects/new")
                            (projects '())
                            (page-title "")
                            (canonical-path "/projects")
                            (is-logged-in #f))
  (html-template G_
                 #:title (if (string-null? page-title) #f page-title)
                 #:canonical-path canonical-path
                 #:is-logged-in is-logged-in
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4))))
                              (div (form (@ (class ,(mk-class '(flex flex-col
                                                                     gap-4)))
                                            (enctype
                                             "application/x-www-form-urlencoded")
                                            (method "POST")
                                            (action ,action))
                                         ,(input #:label (G_ "title")
                                                 #:name 'title
                                                 #:value (html-unescape title)
                                                 #:required #t)
                                         ,(input #:label (G_ "human id")
                                                 #:name 'human-id
                                                 #:value human-id
                                                 #:required #t)
                                         ,(input #:label (G_ "homepage url")
                                                 #:name 'url
                                                 #:value (uri-decode url)
                                                 #:required #f)
                                         ,(input #:label (G_
                                                          "version control url")
                                                 #:name 'vcs-url
                                                 #:value (uri-decode vcs-url)
                                                 #:required #f)
                                         ,(textarea #:label (G_ "description")
                                                    #:name 'description
                                                    #:value description
                                                    #:required #f)
                                         ;; The statuses this project's own
                                         ;; tickets move through.  Left
                                         ;; empty, the project uses the
                                         ;; instance's statuses, which is
                                         ;; what every project does until it
                                         ;; is given a list here.  The
                                         ;; format is the one the domain
                                         ;; reads back: one status to a
                                         ;; line, its five fields separated
                                         ;; by `|'.
                                         ,(textarea #:label (G_
                                                             "ticket statuses")
                                                    #:name 'ticket-statuses
                                                    #:value ticket-statuses
                                                    #:required #f)
                                         (p (@ (class ,(mk-class '(text-sm
                                                                   text-onyx-500
                                                                   dark:text-stone-500))))
                                            ,(G_
                                              "One status to a line: value | label | icon | glyph | bucket, where bucket is open, done or rejected. Leave empty to use the instance's statuses."))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     gap-3
                                                                     items-center
                                                                     mt-2))))
                                              ,(submit-button (G_
                                                               "save project"))
                                              (a (@ (href "/projects")
                                                    (class ,(mk-class '(text-sm
                                                                        text-onyx-700
                                                                        dark:text-stone-300
                                                                        underline))))
                                                 ,(G_ "cancel"))))))
                 #:proj projects))
