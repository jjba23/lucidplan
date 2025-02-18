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

(define-module (lucidplan html)
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
  #:use-module (lucidplan base64)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis route))

(define-public (page-footer G_)
  `((div (@ (class "block text-center font-sans"))
         (div (@ (class
                  "mt-4 flex flex-row flex-wrap gap-4 justify-center align-center p-2"))
              (p (@ (class "text-sm text-stone-600"))
                 ,(G_ "find the source code here"))
              (a (@ (class "text-amber-800 font-bold cursor-pointer text-sm")
                    (href "https://codeberg.org/jjba23/lucidplan"))
                 "https://codeberg.org/jjba23/lucidplan"))
         (p (@ (class "mt-4 p-2"))
            (em (@ (class "text-md text-stone-600"))
                ,(G_ "Copyright 2025 © Josep Bigorra")))
         (p (@ (class "mt-4 p-2"))
            (span (@ (class "text-sm text-stone-600"))
                  ,(G_ "free project management for everyone"))))))

(begin
  (define* (html-template G_
                          #:key body
                          (proj '()))
    (define nav
      `(div (@ (class
                 "flex flex-row flex-wrap gap-3 p-2 bg-stone-200/50 items-center justify-left w-full"))
            (a (@ (class "text-md text-teal-800 font-bold mr-4")
                  (href "/")) "LucidPlan")
            (a (@ (class "text-sm lowercase text-teal-800 font-bold")
                  (href "/login"))
               ,(G_ "Login"))
            (a (@ (class "text-sm lowercase text-teal-800 font-bold")
                  (href "/projects"))
               ,(G_ "Projects"))

            (hr)
            
            (select
             (@
              (onchange "window.location.href=this.value;"))
             ,(map (lambda (p)
                     `(option (@ (value ,(format #f "/tickets/~a"
                                                 (safe-base64-decode (assoc-ref p
                                                                                "human_id"))) )
                                 (class
                                   "text-sm font-light lowercase text-teal-800"))
                              ,(safe-base64-decode (assoc-ref p "title")))) proj))
            ))
    `(html (head (title "LucidPlan")
                 (link (@ (rel "preconnect")
                          (href "https://fonts.googleapis.com")))
                 (link (@ (rel "preconnect")
                          (href "https://fonts.gstatic.com")
                          (crossorigin "")))
                 (link (@ (rel "stylesheet")
                          (href "https://fonts.cdnfonts.com/css/work-sans")))
                 (link (@ (rel "stylesheet")
                          (href "/css/fonts.css")))
                 (meta (@ (name "viewport")
                          (content "width=device-width, initial-scale=1.0")))
                 (script (@ (src "https://cdn.tailwindcss.com")) "")
                 (script (@ (src "/js/tailwind.config.js")) ())
                 (script (@ (src "/js/tickets.js")) ()))
           (body (@ (class "bg-teal-100/35"))
                 (div (@ (class "flex flex-col h-screen justify-between"))
                      (div ,nav
                           (div (@ (class "my-6"))
                                ,body))
                      (footer ,(page-footer G_))))))
  (export html-template))

(define-public (mk-option val comp)
  (cond
    ((equal? val comp)
     `(option (@ (value ,val)
                 (selected ""))
              ,val))
    (else `(option (@ (value ,val))
                   ,val))))
