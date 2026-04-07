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

(define-module (app views projects)
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
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (kracht prelude)
  #:use-module (hygguile hygguile)
  #:use-module (artanis security nss)
  #:use-module (artanis route)
  #:export (render-project render-project-list-item html-from-projects
                           html-new-project-form html-edit-project-form
                           html-project-form))

(define (render-project G_ p)
  `((div (@ (class ,(mk-class '(flex flex-col
                                     gap-2
                                     px-6
                                     py-10
                                     bg-stone-100
                                     dark:bg-onyx-900
                                     rounded-2xl
                                     dark:border-onyx-100/75
                                     border-stone-300/75
                                     shadow-md
                                     border-2))))
         ,(h4 (assq-ref p
                        'title))
         ,(render-if-nestring (lambda (x)
                                `(p (@ (class ,(mk-class '(text-md py-3
                                                           text-onyx-700
                                                           dark:text-stone-300
                                                           whitespace-pre-wrap
                                                           font-medium))))
                                    ,x))
                              (org2html (html-unescape (assq-ref p
                                                                 'description))))
         ,(hr)
         (div (@ (class ,(mk-class '(flex flex-wrap flex-row gap-3))))
              (a (@ (href ,(format #f "/tickets/~a"
                                   (assq-ref p
                                             'human-id))))
                 ,(icon-button #:icon 'scroll
                               #:content (format #f "backlog: ~a"
                                                 (assq-ref p
                                                           'title))
                               #:icon-css '(text-md text-onyx-700
                                                    dark:text-stone-300)))
              (a (@ (class ,(mk-class '(text-onyx-700 dark:text-stone-300)))
                    (href ,(format #f "/kanban/~a"
                                   (assq-ref p
                                             'human-id))))
                 ,(icon-button #:icon 'kanban
                               #:content (format #f "kanban: ~a"
                                                 (assq-ref p
                                                           'title))
                               #:icon-css '(text-md text-onyx-700
                                                    dark:text-stone-300)))
              ,(render-if-nestring (lambda (x)
                                     `(a (@ (class ,(mk-class '(text-onyx-700
                                                                dark:text-stone-300
                                                                no-underline)))
                                            (href ,x))
                                         ,(icon-button #:icon 'link
                                                       #:content (G_
                                                                  "view live")
                                                       #:icon-css '(text-md
                                                                    text-onyx-700
                                                                    dark:text-stone-300))))
                                   (uri-decode (if (assq-ref p
                                                             'url)
                                                   (assq-ref p
                                                             'url) "")))
              ,(render-if-nestring (lambda (x)
                                     `(a (@ (class ,(mk-class '(text-onyx-700
                                                                dark:text-stone-300
                                                                no-underline)))
                                            (href ,x))
                                         ,(icon-button #:icon 'link
                                                       #:content (G_
                                                                  "project code")
                                                       #:icon-css '(text-md
                                                                    text-onyx-700
                                                                    dark:text-stone-300))))
                                   (uri-decode (if (assq-ref p
                                                             'vcs-url)
                                                   (assq-ref p
                                                             'vcs-url) ""))))
         (hr (@ (class ,(mk-class '(my-3)))))
         (div (@ (class ,(mk-class '(flex flex-wrap flex-row gap-4 py-4))))
              (a (@ (href ,(format #f "/projects/~a/edit"
                                   (assq-ref p
                                             'human-id))))
                 ,(icon-button #:icon 'pencil
                               #:content (G_ "edit project")
                               #:icon-css '(text-md text-onyx-700
                                                    dark:text-stone-300)))
              (form (@ (enctype "application/x-www-form-urlencoded")
                       (method "POST")
                       (action "/projects/delete"))
                    (input (@ (name "id")
                              (hidden "")
                              (value ,(assq-ref p
                                                'id))) "")
                    ,(danger-submit-button `(span (@ (class ,(mk-class '(items-center
                                                                         flex
                                                                         flex-row
                                                                         gap-1))))
                                                  ,(icon #:type 'trash
                                                         #:icon-css '(text-md
                                                                      text-stone-300))
                                                  ,(G_ "delete")))))
         (small (@ (class ,(mk-class '(font-mono text-onyx-700
                                                 dark:text-stone-500 text-sm))))
                ,(assq-ref p
                           'human-id)))))

(define (render-project-list-item G_ p)
  `((div (@ (class ,(mk-class '(flex flex-row
                                     flex-wrap
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
                                     cursor-pointer
                                     rounded-xl
                                     shadow)))
            (onclick ,(format #f
                       "document.getElementById('details-~a').toggleAttribute('hidden')"
                       (assq-ref p
                                 'id))))

         (h3 (@ (class ,(mk-class '(flex flex-row items-center gap-2 truncate
                                         leading-6))))
             ,(icon #:type 'list
                    #:icon-css '(text-onyx-700 dark:text-stone-300 text-lg))
             (span (@ (class ,(mk-class '(font-sans text-onyx-700
                                                    dark:text-stone-300
                                                    text-lg font-bold
                                                    break-words))))
                   ,(assq-ref p
                              'title)))
         (p (@ (class ,(mk-class '(font-sans text-stone-600
                                             dark:text-stone-500
                                             text-md
                                             leading-7
                                             max-w-196
                                             break-words
                                             truncate))))
            ,(org2html (html-unescape (format #f "~a"
                                              (assq-ref p
                                                        'description))))))
    (div (@ (id ,(format #f "details-~a"
                         (assq-ref p
                                   'id)))
            (hidden ""))
         ,(render-project G_ p))))

(define* (html-from-projects G_
                             #:key projects is-logged-in search-query)
  (let* ((tit `((div (@ (class ,(mk-class '(flex flex-row
                                                 justify-between
                                                 flex-wrap
                                                 items-center
                                                 gap-1
                                                 my-3))))
                     (div (@ (class ,(mk-class '(flex flex-row items-center
                                                      gap-1))))
                          ,(icon #:type 'align-bottom
                                 #:icon-css '(text-onyx-700
                                              dark:text-stone-300 text-2xl))
                          ,(h2 (G_ "Projects")))
                     (div (a (@ (href "/projects/new"))
                             ,(icon-button #:icon 'plus
                                           #:content (G_ "new project")
                                           #:icon-css '(text-md text-onyx-700
                                                        dark:text-stone-300))))))))
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4 py-4))))
                                 ((div (@ (class ,(mk-class '(flex flex-col
                                                                   gap-4
                                                                   flex-grow))))
                                       ,tit

                                       (div (@ (class ,(mk-class '(flex
                                                                   flex-row
                                                                   flex-wrap
                                                                   justify-end))))
                                            ,(search-box G_
                                                         #:search-query
                                                         search-query))

                                       (div (@ (id lucidplan-projects-list)
                                               (class ,(mk-class '(flex
                                                                   flex-col
                                                                   gap-2
                                                                   container
                                                                   my-3))))
                                            ,(map (lambda (x)
                                                    (render-project-list-item
                                                     G_ x)) projects))))))
                   #:proj projects)))

(define* (html-new-project-form G_
                                #:key projects is-logged-in)
  (html-project-form G_
                     #:projects projects
                     #:is-logged-in is-logged-in))

(define* (html-edit-project-form G_
                                 #:key projects project is-logged-in)
  (html-project-form G_
                     #:is-logged-in is-logged-in
                     #:title (assq-ref project
                                       'title)
                     #:human-id (assq-ref project
                                          'human-id)
                     #:url (if (assq-ref project
                                         'url)
                               (assq-ref project
                                         'url) "")
                     #:vcs-url (if (assq-ref project
                                             'vcs-url)
                                   (assq-ref project
                                             'vcs-url) "")
                     #:description (if (assq-ref project
                                                 'description)
                                       (html-unescape (assq-ref project
                                                                'description))
                                       "")
                     #:action (format #f "/projects/~a/edit"
                                      (assq-ref project
                                                'human-id))
                     #:projects projects))

(define* (html-project-form G_
                            #:key (title "")
                            (human-id "")
                            (description "")
                            (url "")
                            (vcs-url "")
                            (action "/projects/new")
                            (projects '())
                            (is-logged-in #f))
  (html-template G_
                 #:is-logged-in is-logged-in
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4))))
                              (div (form (@ (class ,(mk-class '(flex flex-col
                                                                     gap-4)))
                                            (enctype
                                             "application/x-www-form-urlencoded")
                                            (method POST)
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
                                                    #:value (html-unescape
                                                             description)
                                                    #:required #f
                                                    #:textarea-css '(leading-7
                                                                     p-4
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

                                         (div ,(submit-button (G_ "submit"))))))
                 #:proj projects))
