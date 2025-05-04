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
  #:use-module (orgwebalchemy org-to-html)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan query-escape)
  #:use-module (hygguile components button)
  #:use-module (hygguile components input)
  #:use-module (hygguile components icon)
  #:use-module (hygguile abstraction)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define (render-project G_ p)
  `((div (@ (class ,(->css '(flex flex-col
                                  gap-2
                                  px-3
                                  py-6
                                  bg-stone-100
                                  rounded-2xl
                                  border-stone-400/75
                                  border-2))))
         ,(render-if-nestring (lambda (x)
                                `(p (@ (class ,(->css '(text-md py-3
                                                                text-stone-800
                                                                whitespace-pre-wrap
                                                                font-medium))))
                                    ,x))
                              (org-to-html (unescape-str (assoc-ref p
                                                                    "description"))))
         (hr)
         (div (@ (class ,(->css '(flex flex-wrap flex-row gap-3))))
              (a (@ (href ,(format #f "/tickets/~a"
                                   (assoc-ref p "human_id"))))
                 ,(hyg-icon-button #:icon 'scroll
                                   #:content (format #f "backlog: ~a"
                                                     (assoc-ref p "title"))))
              (a (@ (class "text-stone-800")
                    (href ,(format #f "/kanban/~a"
                                   (assoc-ref p "human_id"))))
                 ,(hyg-icon-button #:icon 'kanban
                                   #:content (format #f "kanban: ~a"
                                                     (assoc-ref p "title"))))
              ,(render-if-nestring (lambda (x)
                                     `(a (@ (class ,(->css '(text-stone-800)))
                                            (href ,x))
                                         ,(hyg-icon-button #:icon 'link
                                                           #:content (G_
                                                                      "view live"))))
                                   (assoc-ref p "url"))
              ,(render-if-nestring (lambda (x)
                                     `(a (@ (class ,(->css '(text-stone-800)))
                                            (href ,x))
                                         ,(hyg-icon-button #:icon 'link
                                                           #:content (G_
                                                                      "project code"))))
                                   (assoc-ref p "vcs_url")))
         (hr (@ (class ,(->css '(my-3)))))
         (div (@ (class ,(->css '(flex flex-wrap flex-row gap-4))))
              (a (@ (href ,(format #f "/projects/~a/edit"
                                   (assoc-ref p "human_id"))))
                 ,(hyg-button `(span (@ (class ,(->css '(items-center
                                                         flex
                                                         flex-row
                                                         gap-1))))
                                     ((i (@ (class ,(->css '(ph
                                                             ph-pencil
                                                             text-white
                                                             text-xl))))
                                         ""))
                                     (span ,(G_ "edit project")))))
              (form (@ (enctype "application/x-www-form-urlencoded")
                       (method "POST")
                       (action "/projects/delete"))
                    (input (@ (name "id")
                              (hidden "")
                              (value ,(assoc-ref p "id"))) "")
                    ,(hyg-danger-submit-button `(span (@ (class ,(->css '
                                                                  (items-center
                                                                   flex
                                                                   flex-row
                                                                   gap-1))))
                                                      ,(hyg-icon
                                                        #:type 'trash
                                                        #:icon-css '
                                                        (text-white
                                                         text-xl))
                                                      ,(G_ "delete")))))
         (small (@ (class ,(->css '(font-mono text-stone-600))))
                ,(format #f "~a"
                         (assoc-ref p "human_id")))
         (small (@ (class ,(->css '(font-mono text-stone-600 text-xs))))
                ,(assoc-ref p "id")))))

(define (render-project-list-item G_ p)
  `((div (@ (class ,(->css '(flex flex-row
                                  flex-wrap
                                  gap-4
                                  items-center
                                  justify-left
                                  bg-stone-300/75
                                  border-stone-400/75
                                  border-2
                                  py-2
                                  px-4
                                  cursor-pointer
                                  rounded-xl)))
            (onclick ,(format #f
                              "document.getElementById('details-~a').toggleAttribute('hidden')"
                              (assoc-ref p "id"))))

         (h3 (@ (class ,(->css '(flex flex-row items-center gap-2 truncate))))
             ,(hyg-icon #:type 'list
                        #:icon-css '(text-stone-500 text-lg))
             (span (@ (class ,(->css '(font-sans text-stone-700 text-lg
                                                 font-black break-words))))
                   ,(format #f "~a"
                            (assoc-ref p "title"))))
         (p (@ (class ,(->css '(font-sans text-stone-600 text-sm break-words
                                          truncate))))
            ,(org-to-html (unescape-str (assoc-ref p "description")))))
    (div (@ (id ,(format #f "details-~a"
                         (assoc-ref p "id")))
            (class "")
            (hidden ""))
         ,(render-project G_ p))))

(define-public (html-from-projects G_ projects)
  (html-template G_
                 #:body `(div (@ (class "container mx-auto"))
                              ((div (@ (class ,(->css '(flex flex-col gap-4
                                                             flex-grow))))
                                    (div (@ (class ,(->css '(flex flex-row
                                                             items-center
                                                             gap-1 my-3))))
                                         ,(hyg-icon #:type 'align-bottom
                                                    #:icon-css '(text-stone-800
                                                                 text-2xl))
                                         (h2 (@ (class ,(->css '(text-3xl
                                                                 font-sans
                                                                 font-bold
                                                                 text-stone-700))))
                                             ,(G_ "projects")))
                                    ,(map (lambda (x)
                                            (render-project-list-item G_ x))
                                          projects)
                                    (div (a (@ (href "/projects/new"))
                                            ,(G_ "create a project"))))))
                 #:proj projects))

(define-public (html-new-project-form G_ proj)
  (html-project-form G_
                     #:proj proj))

(define-public (html-edit-project-form G_ proj p)
  (html-project-form G_
                     #:title (assoc-ref p "title")
                     #:human-id (assoc-ref p "human_id")
                     #:url (if (assoc-ref p "url")
                               (assoc-ref p "url") "")
                     #:vcs-url (if (assoc-ref p "vcs_url")
                                   (assoc-ref p "vcs_url") "")
                     #:description (if (assoc-ref p "description")
                                       (unescape-str (assoc-ref p
                                                                "description"))
                                       "")
                     #:action (format #f "/projects/~a/edit"
                                      (assoc-ref p "human_id"))
                     #:proj proj))

(define* (html-project-form G_
                            #:key (title "")
                            (human-id "")
                            (description "")
                            (url "")
                            (vcs-url "")
                            (action "/projects/new")
                            (proj '()))
  (html-template G_
                 #:body `(div (@ (class "container mx-auto"))
                              ((form (@ (class "flex flex-col gap-4 p-2")
                                        (enctype
                                         "application/x-www-form-urlencoded")
                                        (method "POST")
                                        (action ,action))
                                     ,(hyg-input #:label (G_ "title")
                                                 #:name "title"
                                                 #:value (unescape-str title)
                                                 #:required #t)
                                     ,(hyg-input #:label (G_ "human id")
                                                 #:name "human-id"
                                                 #:value human-id
                                                 #:required #t)
                                     ,(hyg-input #:label (G_ "homepage url")
                                                 #:name "url"
                                                 #:value url
                                                 #:required #f)
                                     ,(hyg-input #:label (G_
                                                          "version control url")
                                                 #:name "vcs-url"
                                                 #:value vcs-url
                                                 #:required #f)
                                     ,(hyg-textarea #:label (G_ "description")
                                                    #:name "description"
                                                    #:value (unescape-str
                                                             description)
                                                    #:required #f)

                                     (div ,(hyg-submit-button (G_
                                                               "submit"))))))
                 #:proj proj))
