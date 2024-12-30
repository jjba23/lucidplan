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
  #:use-module (artanis route))

(define-public (page-footer)
  `((div (@ (class "block text-center font-mono"))
         (div (@ (class
                  "mt-4 flex flex-row flex-wrap gap-4 justify-center align-center"))
              (p (@ (class "text-sm text-stone-600"))
                 "find the source code here:")
              (a (@ (class "text-orange-700 font-bold cursor-pointer text-sm")
                    (href "https://codeberg.org/jjba23/lucidplan"))
                 "https://codeberg.org/jjba23/lucidplan"))
         (p (@ (class "mt-4"))
            (em (@ (class "text-md text-stone-600"))
                "Copyright 2025 - Josep Bigorra"))
         (p (@ (class "mt-4"))
            (span (@ (class "text-sm text-stone-600"))
                  "free and open project management for everyone")))))

(define-public (html-template body)
  `(html (head (title "LucidPlan")
               (link (@ (rel "preconnect")
                        (href "https://fonts.googleapis.com")))
               (link (@ (rel "preconnect")
                        (href "https://fonts.gstatic.com")
                        (crossorigin "")))
               (link (@ (rel "stylesheet")
                        (href "https://fonts.cdnfonts.com/css/liberation-sans")))
               (link (@ (rel "stylesheet")
                        (href "/css/fonts.css")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1.0")))
               (script (@ (src "https://cdn.tailwindcss.com")) "")
               (script (@ (src "/js/tailwind.config.js")) ())
               (script (@ (src "/js/tickets.js")) ()))
         (body (@ (class "bg-stone-200 p-2"))
               (div (@ (class
                         "my-6 flex flex-row flex-wrap justify-left gap-4"))
                    (div (@ (class "flex flex-col gap-3 p-2"))
                         (a (@ (class
                                 "text-sm uppercase text-stone-600 font-bold")
                               (href "/projects")) "Projects")
                         ;; (a (@ (class
                         ;;         "text-sm uppercase text-stone-600 font-bold")
                         ;;       (href "/tickets/JOE")) "Tickets")
                         ;; (a (@ (class
                         ;;         "text-sm uppercase text-stone-600 font-bold")
                         ;;       (href "/board/JOE")
                         ;;       (hidden "")) "Board")
                         )
                    ,body)
               (footer ,(page-footer)))))

(define-public (mk-option val comp)
  (cond
   ((equal? val comp) `(option (@ (value ,val)
                                  (selected "")) ,val))
   (else
    `(option (@ (value ,val)) ,val))
   ))
