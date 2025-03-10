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
  #:use-module (lucidplan prelude)
  #:use-module (artanis i18n)
  #:use-module (hygguile abstraction)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis route))

(define-public (page-footer G_)
  `((div (@ (class "block text-center font-sans"))
         (div (@ (class
                  "flex flex-row flex-wrap gap-2 justify-center align-center py-3"))
              (p (@ (class "text-sm text-stone-600"))
                 ,(G_ "find the source code here"))
              (a (@ (class "text-stone-800 font-bold cursor-pointer text-sm")
                    (href "https://codeberg.org/jjba23/lucidplan"))
                 "https://codeberg.org/jjba23/lucidplan"))
         (p (@ (class "py-3"))
            (em (@ (class "text-md text-stone-600"))
                ,(G_ "Copyright 2025 © Josep Bigorra")))
         (p (@ (class "py-3"))
            (span (@ (class "text-sm text-stone-600"))
                  ,(G_ "free project management for everyone"))))))

(begin
  (define* (html-template G_
                          #:key body
                          (proj '()))
    (define nav
      `(div (@ (class
                "flex flex-row flex-wrap gap-3 p-3 bg-stone-900/90 items-center justify-left w-full shadow-lg"))
            (a (@ (class "text-md text-white font-sans font-black px-4")
                  (href "/")) "LucidPlan")
            (a (@ (class
                   "text-sm lowercase text-white font-sans font-medium px-2 flex-items-center gap-2")
                  (href "/login"))
               (span (i (@ (class ,(->css '(ph ph-sign-in text-md)))) ""))
               "&nbsp;"
               (span ,(G_ "Login")))
            (a (@ (class
                   "text-sm lowercase text-white font-sans font-medium px-2 flex items-center gap-2")
                  (href "/projects"))
               (span (i (@ (class ,(->css '(ph ph-align-bottom text-md)))) ""))
               (span ,(G_ "Projects")))

            (hr)

            (select (@ (onchange "window.location.href=this.value;")
                       (class
                        "text-sm text-white bg-transparent font-medium font-sans"))
                    (option "🔗 go to")
                    ,(map (lambda (p)
                            `(option (@ (value ,(format #f "/tickets/~a"
                                                        (assoc-ref p
                                                                   "human_id")))
                                        (class
                                         "text-sm font-light lowercase text-stone-800"))
                                     ,(assoc-ref p "title"))) proj))))
    `(html (@ (lang en))
           (head (title "LucidPlan")
                 (style ,(assoc-ref asset-library "pub/css/fonts.css"))
                 (style ,(assoc-ref asset-library "pub/css/phosphor.css"))
                 (meta (@ (name viewport)
                          (content "width=device-width, initial-scale=1.0")))
                 (meta (@ (charset UTF-8)))
                 (script ,(assoc-ref asset-library "pub/js/tailwind.js"))
                 (script ,(assoc-ref asset-library "pub/js/tailwind.config.js")))
           (body (@ (class "bg-stone-200/50"))
                 (div (@ (class "flex flex-col h-screen justify-between"))
                      (div ,nav
                           (div (@ (class "py-8"))
                                ,body))
                      (footer ,(page-footer G_)))
                 (script ,(assoc-ref asset-library "pub/js/tickets.js")))))
  (export html-template))

(define-public (emoji-for-ticket-status ticket-status)
  (cond
    ((equal? "todo" ticket-status)
     `((i (@ (class ,(->css '(ph ph-anchor text-stone-600 text-xl)))) "")))
    ((equal? "work-in-progress" ticket-status)
     `((i (@ (class ,(->css '(ph ph-gear-fine text-stone-600 text-xl)))) "")))
    ((equal? "in-review" ticket-status)
     `((i (@ (class ,(->css '(ph ph-list-magnifying-glass text-stone-600
                                 text-xl)))) "")))
    ((equal? "done" ticket-status)
     `((i (@ (class ,(->css '(ph ph-check text-stone-600 text-xl)))) "")))
    ((equal? "rejected" ticket-status)
     `((i (@ (class ,(->css '(ph ph-x text-stone-600 text-xl)))) "")))
    (else "")))

(define-public (emoji-for-ticket-type ticket-type)
  (cond
    ((equal? "task" ticket-type)
     `((i (@ (class ,(->css '(ph ph-gear-fine text-stone-600 text-xl)))) "")))
    ((equal? "bug" ticket-type)
     `((i (@ (class ,(->css '(ph ph-bug text-stone-600 text-xl)))) "")))
    ((equal? "story" ticket-type)
     `((i (@ (class ,(->css '(ph ph-book-open-text text-stone-600 text-xl))))
          "")))
    ((equal? "spike" ticket-type)
     `((i (@ (class ,(->css '(ph ph-flask text-stone-600 text-xl)))) "")))
    ((equal? "time-box" ticket-type)
     `((i (@ (class ,(->css '(ph ph-clock text-stone-600 text-xl)))) "")))
    (else "")))
