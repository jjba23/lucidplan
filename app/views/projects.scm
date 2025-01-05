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
  #:use-module (lucidplan base64)
  #:use-module (artanis security nss)
  #:use-module (artanis route))

(define-public project-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define (render-project p)
  `((div (@ (class "flex flex-col gap-2 p-3 bg-stone-100 rounded-xl ml-4"))
         (a (@ (href ,(format #f "/tickets/~a"
                              (safe-base64-decode (assoc-ref p "human_id")))))
            ,(format #f "go to tickets: ~a"
                     (safe-base64-decode (assoc-ref p "title"))))
         (a (@ (href ,(format #f "/projects/~a/edit"
                              (assoc-ref p "id")))) "edit project")
         (form (@ (enctype "application/x-www-form-urlencoded")
                  (method "POST")
                  (action "/projects/delete"))
               (input (@ (name "id")
                         (hidden "")
                         (value ,(assoc-ref p "id"))) "")
               (button (@ (type "submit"))
                       (small (@ (class "text-red-600")) "delete")))
         (small (@ (class "font-mono text-stone-500 text-xs"))
                ,(format #f "~a"
                         (safe-base64-decode (assoc-ref p "human_id"))))
         (small (@ (class "font-mono text-stone-500 text-xs"))
                ,(assoc-ref p "id")))))

(define (render-project-list-item p)
  `((div (@ (class
             "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300 border-stone-200 py-1 px-3 cursor-pointer rounded-xl")
            (onclick ,(format #f project-listitem-reveal-details-js-template
                              (assoc-ref p "id"))))

         (p (@ (class "font-mono text-stone-700 text-lg"))
            ,(format #f "~a (~a)"
                     (safe-base64-decode (assoc-ref p "title"))
                     (safe-base64-decode (assoc-ref p "human_id")))))
    (div (@ (id ,(format #f "details-~a"
                         (assoc-ref p "id")))
            (class "ml-4")
            (hidden ""))
         ,(render-project p))))

(define-public (html-from-projects projects)
  (html-template `((div (@ (class "flex flex-col gap-4 flex-grow"))
                        (h2 (@ (class
                                "text-xl font-mono font-bold text-stone-500"))
                            "projects")
                        ,(map render-project-list-item projects)
                        (div (a (@ (href "/projects/new")) "+ new project"))))))

(define-public (html-new-project-form)
  (html-project-form))

(define-public (html-edit-project-form p)
  (html-project-form #:title (safe-base64-decode (assoc-ref p "title"))
                     #:human-id (safe-base64-decode (assoc-ref p "human_id"))
                     #:action (format #f "/projects/~a/edit"
                                      (assoc-ref p "id"))))

(define* (html-project-form #:key (title "")
                            (human-id "")
                            (action "/projects/new"))
  (html-template `((form (@ (class "flex flex-col gap-4")
                            (enctype "application/x-www-form-urlencoded")
                            (method "POST")
                            (action ,action))
                         (label "title:")
                         (input (@ (class "p-3 w-min rounded-xl")
                                   (name "title")
                                   (value ,title)
                                   (required "")) "")

                         (label "human id:")
                         (input (@ (class "p-3 w-min rounded-xl")
                                   (name "human-id")
                                   (value ,human-id)
                                   (required "")) "")

                         (button (@ (class
                                     "bg-green-800 text-white p-3 w-min rounded-xl")
                                    (type "submit")) "done")))))
