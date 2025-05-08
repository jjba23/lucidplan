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
  #:use-module (artanis i18n)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components input)
  #:use-module (hygguile components button)
  #:use-module (hygguile components typography)
  #:use-module (hygguile components select)
  #:use-module (artanis i18n sxml)
  #:use-module (kracht prelude)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis route))

(define-public asset-library
  (build-asset-library '("pub/css/fonts.css" "pub/css/phosphor.css"
                         "pub/js/tickets.js" "pub/js/tailwind.js"
                         "pub/js/tailwind.config.js")))

(define-public (page-footer G_)
  `((div (@ (class "block text-center font-sans"))
         (div (@ (class ,(->css '(flex flex-row
                                       flex-wrap
                                       gap-2
                                       justify-center
                                       align-center
                                       py-3))))
              (p (@ (class "text-sm text-stone-600"))
                 ,(G_ "find the source code here"))
              (a (@ (class ,(->css '(text-stone-800 font-bold cursor-pointer
                                                    text-sm)))
                    (href "https://codeberg.org/jjba23/lucidplan"))
                 "https://codeberg.org/jjba23/lucidplan"))
         (p (@ (class ,(->css '(py-3))))
            (em (@ (class "text-md text-stone-600"))
                ,(G_ "Copyright 2025 © Josep Bigorra")))
         (p (@ (class ,(->css '(py-3))))
            (span (@ (class ,(->css '(text-sm text-stone-600))))
                  ,(G_ "free project management for everyone"))))))

(begin
  (define* (html-template G_
                          #:key body
                          (proj '()))
    (define nav
      `(div (@ (class ,(->css '(flex flex-row
                                     flex-wrap
                                     gap-3
                                     p-3
                                     bg-stone-900/90
                                     items-center
                                     justify-left
                                     w-full
                                     shadow-lg))))
            (a (@ (class ,(->css '(text-md text-white font-sans font-black
                                           px-4)))
                  (href "/")) "LucidPlan")
            (a (@ (class ,(->css '(text-sm lowercase
                                           text-white
                                           font-sans
                                           font-medium
                                           px-2
                                           flex-items-center
                                           gap-2)))
                  (href "/login"))
               (span (i (@ (class ,(->css '(ph ph-sign-in text-md)))) ""))
               "&nbsp;"
               (span ,(G_ "Login")))
            (a (@ (class ,(->css '(text-sm lowercase
                                           text-white
                                           font-sans
                                           font-medium
                                           px-2
                                           flex
                                           items-center
                                           gap-2)))
                  (href "/projects"))
               (span (i (@ (class ,(->css '(ph ph-align-bottom text-md)))) ""))
               (span ,(G_ "Projects")))

            (hr)

            (select (@ (onchange "window.location.href=this.value;")
                       (class ,(->css '(text-sm text-white bg-transparent
                                                font-medium font-sans
                                                shadow-md))))
                    (option "🔗 go to")
                    ,(map (lambda (p)
                            `(option (@ (value ,(format #f "/tickets/~a"
                                                        (assoc-ref p
                                                                   "human_id")))
                                        (class
                                         "text-sm font-light lowercase text-stone-800"))
                                     ,(assoc-ref p "title"))) proj))))
    `("<!DOCTYPE html>" (html (@ (lang en))
                              (head (title "LucidPlan")
                                    (style ,(assoc-ref asset-library
                                                       "pub/css/fonts.css"))
                                    (style ,(assoc-ref asset-library
                                                       "pub/css/phosphor.css"))
                                    (meta (@ (name viewport)
                                             (content
                                              "width=device-width, initial-scale=1.0")))
                                    (meta (@ (charset UTF-8)))
                                    (script ,(assoc-ref asset-library
                                                        "pub/js/tailwind.js"))
                                    (script ,(assoc-ref asset-library
                                              "pub/js/tailwind.config.js")))
                              (body (@ (class "bg-stone-200/50"))
                                    (div (@ (class
                                             "flex flex-col h-screen justify-between"))
                                         (div ,nav
                                              (div (@ (class "py-8"))
                                                   ,body))
                                         (footer ,(page-footer G_)))
                                    (script ,(assoc-ref asset-library
                                                        "pub/js/tickets.js"))))))
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

(begin
  (define* (search-box G_
                       #:key search-query)
    `((script
       "
function addSearchQueryParam(form) {
  const searchInput = document.getElementsByName('search')[0];

  if (searchInput.value) {
    const urlSearchParams = new URLSearchParams(window.location.search);
    var params = Object.fromEntries(urlSearchParams.entries());

    params.search = encodeURIComponent(searchInput.value);

    const q = Object.entries(params).map((k,_)=>`${k[0]}=${k[1]}`).reduce((a,b) => `${a}&${b}`);

    console.log(q);
    console.log(`${window.location.pathname}?${q}`);

    window.location.href = `${window.location.pathname}?${q}`;
    return false; // Prevent the default form submission
  }
  return true; // Allow default submission if the input is empty
}
")
      (div (@ (class ,(->css '(my-3))))
           (form (@ (onsubmit "addSearchQueryParam(this)"))
                 (div (@ (class ,(->css '(flex flex-row align-center
                                               items-center justify-left gap-2))))
                      ,(hyg-input #:label ""
                                  #:name "search"
                                  #:value (cond
                                            ((equal? #f search-query)
                                             "")
                                            (else search-query))
                                  #:placeholder (G_ "Search...")
                                  #:input-css '(p-2 max-w-prose rounded-xl
                                                    border-stone-200 border-2
                                                    shadow-md)
                                  #:input-flex-css #f)
                      ,(hyg-submit-button (G_ "search")))))))
  (export search-box))

(begin
  (define* (sort-select G_
                        #:key project-id)
    `((div (@ (class ,(->css '(flex flex-col gap-2 w-min))))
           (select (@ (onchange "window.location.href=this.value;")
                      (name "sort-select")
                      (id "sort-select")
                      (class ,(->css '(p-2 rounded-xl
                                           bg-white
                                           border-stone-200
                                           border-2
                                           shadow-md
                                           max-w-prose))))
                   (option ,(G_ "sort order"))
                   ,(map (lambda (s)
                           `((option (@ (value ,(format #f
                                                        "/tickets/~a?sort=~a"
                                                        project-id
                                                        (car s))))
                                     ,(cdr s))))
                         `(("title-asc" unquote
                            (G_ "Title - Ascending"))
                           ("title-desc" unquote
                            (G_ "Title - Descending"))

                           ("created-at-asc" unquote
                            (G_ "Created - Ascending"))
                           ("created-at-desc" unquote
                            (G_ "Created - Descending"))

                           ("last-edited-at-asc" unquote
                            (G_ "Last edited - Ascending"))
                           ("last-edited-at-desc" unquote
                            (G_ "Last edited - Descending"))

                           ))))))
  (export sort-select))
