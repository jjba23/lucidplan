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
  #:use-module (lucidplan prelude)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 i18n)
  #:use-module (lucidplan query-escape)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define-public project-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define (render-project G_ p)
  `((div (@ (class
             "flex flex-col gap-2 p-3 bg-stone-100 rounded-2xl  border-stone-400/75 border-2"))
         ,(render-if-nestring (lambda (x)
                                `(p ,x))
                              (assoc-ref p "description"))
         (hr)
         (a (@ (class "text-stone-800")
               (href ,(format #f "/tickets/~a"
                              (assoc-ref p "human_id"))))
            ,(format #f "📚 go to backlog: ~a"
                     (assoc-ref p "title")))
         (a (@ (class "text-stone-800")
               (href ,(format #f "/kanban/~a"
                              (assoc-ref p "human_id"))))
            ,(format #f "💡 go to kanban: ~a"
                     (assoc-ref p "title")))
         (hr)
         ,(render-if-nestring (lambda (x)
                                `(a (@ (class "text-stone-800")
                                       (href ,x)) "🔗 view project"))
                              (assoc-ref p "url"))
         ,(render-if-nestring (lambda (x)
                                `(a (@ (class "text-stone-800")
                                       (href ,x)) "🔗 project code"))
                              (assoc-ref p "vcs_url"))
         (hr)
         (div (@ (class "flex flex-wrap flex-row gap-4"))
              (a (@ (href ,(format #f "/projects/~a/edit"
                                   (assoc-ref p "id"))))
                 ,(G_ "edit project"))
              (form (@ (enctype "application/x-www-form-urlencoded")
                       (method "POST")
                       (action "/projects/delete"))
                    (input (@ (name "id")
                              (hidden "")
                              (value ,(assoc-ref p "id"))) "")
                    (button (@ (type "submit"))
                            (p (@ (class "text-red-600"))
                               ,(format #f "~a  ❌"
                                        (G_ "delete"))))))
         (small (@ (class "font-mono text-stone-600"))
                ,(format #f "~a"
                         (assoc-ref p "human_id")))
         (small (@ (class "font-mono text-stone-600 text-xs"))
                ,(assoc-ref p "id")))))

(define (render-project-list-item G_ p)
  `((div (@ (class
              "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300/75 border-stone-400/75 border-2 py-2 px-4 cursor-pointer rounded-xl")
            (onclick ,(format #f project-listitem-reveal-details-js-template
                              (assoc-ref p "id"))))

         (p (@ (class "font-sans text-stone-800 text-lg"))
            ,(format #f "~a"
                     (assoc-ref p "title"))))
    (div (@ (id ,(format #f "details-~a"
                         (assoc-ref p "id")))
            (class "")
            (hidden ""))
         ,(render-project G_ p))))

(define-public (html-from-projects G_ projects)
  (html-template G_
                 #:body `(div (@ (class "container mx-auto"))
                              ((div (@ (class "flex flex-col gap-4 flex-grow"))
                                    (h2 (@ (class
                                            "text-xl font-sans font-bold text-stone-600"))
                                        ,(G_ "projects"))
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
                                       (assoc-ref p "description") "")
                     #:action (format #f "/projects/~a/edit"
                                      (assoc-ref p "id"))
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
                              ((form (@ (class "flex flex-col gap-4")
                                        (enctype
                                         "application/x-www-form-urlencoded")
                                        (method "POST")
                                        (action ,action))
                                     (label ,(G_ "title"))
                                     (input (@ (class
                                                "p-3 w-min rounded-xl border-stone-400/75 border-2")
                                               (name "title")
                                               (value ,(unescape-str title))
                                               (required "")) "")

                                     (label ,(G_ "human id"))
                                     (input (@ (class
                                                "p-3 w-min rounded-xl border-stone-400/75 border-2")
                                               (name "human-id")
                                               (value ,human-id)
                                               (required "")) "")
                                     (label ,(G_ "url"))
                                     (input (@ (class
                                                "p-3 w-min rounded-xl border-stone-400/75 border-2")
                                               (name "url")
                                               (value ,url)) "")
                                     (label ,(G_ "code url"))
                                     (input (@ (class
                                                "p-3 w-min rounded-xl border-stone-400/75 border-2")
                                               (name "vcs-url")
                                               (value ,vcs-url)) "")

                                     (label ,(G_ "description"))
                                     (input (@ (class
                                                "p-3 w-min rounded-xl border-stone-400/75 border-2")
                                               (name "description")
                                               (value ,(unescape-str
                                                        description))) "")

                                     (button (@ (class
                                                 "bg-green-800/75 text-white p-3 w-min rounded-xl")
                                                (type "submit"))
                                             ,(G_ "done")))))
                 #:proj proj))
