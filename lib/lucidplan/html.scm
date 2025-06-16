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
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n sxml)
  #:use-module (kracht prelude)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (orgwebalchemy org-to-html)
  #:use-module (artanis route))

(define-public asset-library
  (build-asset-library '("pub/js/tickets.js" "pub/js/tailwind.js")))

(define-public (page-footer G_)
  (footer `(,(em (G_ "lucidplan - free project management for everyone")) ,(em
                                                                            (G_
                                                                             "lucidplan is free software, available under the GNU AGPL v3 or newer"))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center))))
                 ,(em (G_ "find the source code here"))
                 ,(a "https://codeberg.org/jjba23/lucidplan"))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center))))
                 ,(em (G_ "find the manual here"))
                 ,(a
                   "https://jointhefreeworld.org/manuals/lucidplan/index.html"))
            ,(em (G_ "Copyright © Josep Bigorra  &lt;jjbigorra@gmail.com&gt;"))

            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center items-center))))
                 ,(em "version:")
                 ,(mono (format #f "~a"
                                (get-short-commit-sha))
                        #:size '(text-sm)
                        #:weight '(font-light))))))

(begin
  (define* (html-template G_
                          #:key body
                          (proj '())
                          (is-logged-in #f))
    (define nav
      `(div (@ (class ,(mk-class '(flex flex-row
                                        flex-wrap
                                        gap-3
                                        p-3
                                        bg-stone-200
                                        dark:bg-stone-900
                                        items-center
                                        justify-left
                                        w-full
                                        shadow-lg))))
            (a (@ (href "/")
                  (class ,(mk-class '(mx-3))))
               ,(h4 "LucidPlan"))
            ,(if (not is-logged-in)
                 `((a (@ (class ,(mk-class '(text-sm lowercase
                                                     text-white
                                                     dark:text-black
                                                     font-sans
                                                     font-medium
                                                     px-2
                                                     flex-items-center
                                                     gap-2)))
                         (href "/login"))
                      ,(icon-button #:icon 'sign-in
                                    #:content (G_ "login"))))
                 `((a (@ (class ,(mk-class '(text-sm lowercase
                                                     text-white
                                                     dark:text-black
                                                     font-sans
                                                     font-medium
                                                     px-2
                                                     flex-items-center
                                                     gap-2)))
                         (href "/me"))
                      ,(icon-button #:icon 'user-circle
                                    #:content (G_ "me")))))
            (a (@ (class ,(mk-class '(text-sm lowercase
                                              text-stone-900
                                              dark:text-stone-100
                                              font-sans
                                              font-medium
                                              px-2
                                              flex
                                              items-center
                                              gap-2)))
                  (href "/projects"))
               ,(icon-button #:icon 'align-bottom
                             #:content (G_ "projects")))

            ,(ui-mode-button)

            ,(if (nil? proj)
                 '()
                 `((select (@ (onchange "window.location.href=this.value;")
                              (class ,(mk-class '(text-sm text-stone-900
                                                          dark:text-stone-100
                                                          bg-transparent
                                                          font-medium
                                                          font-sans))))
                           (option "🔗 go to")
                           ,(map (lambda (p)
                                   `(option (@ (value ,(format #f
                                                               "/tickets/~a"
                                                               (assoc-ref p
                                                                "human_id")))
                                               (class ,(mk-class '(text-sm
                                                                   font-light
                                                                   lowercase
                                                                   text-stone-800))))
                                            ,(assoc-ref p "title"))) proj))))))
    `("<!DOCTYPE html>" (html (@ (lang en))
                              (head (title "LucidPlan")
                                    (link (@ (rel preconnect)
                                             (href
                                              "https://fonts.googleapis.com")))
                                    (link (@ (rel preconnect)
                                             (href "https://fonts.gstatic.com")
                                             (crossorigin "")))
                                    (link (@ (rel stylesheet)
                                             (href
                                              "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&family=IBM+Plex+Sans:ital,wght@0,100..700;1,100..700&family=IBM+Plex+Serif:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap")))
                                    (style (@ (type "text/tailwindcss"))
                                     "
@custom-variant dark (&:where([data-theme=dark], [data-theme=dark] *));

@theme {
  --font-sans: \"IBM Plex Sans\", sans-serif;
  --font-mono: \"IBM Plex Mono\", monospace;
}
")
                                    (link (@ (rel stylesheet)
                                             (type text/css)
                                             (href
                                              "https://cdn.jsdelivr.net/npm/@phosphor-icons/web@2.1.1/src/regular/style.css")))
                                    (meta (@ (name viewport)
                                             (content
                                              "width=device-width, initial-scale=1.0")))
                                    (meta (@ (charset UTF-8)))
                                    (link (@ (rel "shortcut icon")
                                             (href "/favicon.ico")
                                             (type "image/x-icon")))
                                    (link (@ (rel "icon")
                                             (href "/favicon.ico")
                                             (type "image/x-icon")))
                                    (script ,(assoc-ref asset-library
                                                        "pub/js/tailwind.js")))
                              (body (@ (class ,(mk-class '(bg-stone-100
                                                           dark:bg-stone-800))))

                                    ,(with-footer #:main `((div (@ (class ,(mk-class '
                                                                            (flex
                                                                             flex-col
                                                                             justify-between
                                                                             min-h-96))))
                                                                (div (@ (class ,
                                                                         (mk-class '
                                                                          (flex
                                                                           flex-col
                                                                           gap-8
                                                                           justify-start))))
                                                                     ,nav
                                                                     ,body)))
                                                  #:footer (page-footer G_))
                                    (script ,(assoc-ref asset-library
                                                        "pub/js/tickets.js"))
                                    (script ,toggle-dark-theme-js)
                                    (script ,remember-dark-theme-js)))))
  (export html-template))

(define-public (emoji-for-ticket-status ticket-status)
  (cond
    ((equal? "todo" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-anchor text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "work-in-progress" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-gear-fine text-stone-600 text-xl)))) "")))
    ((equal? "in-review" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-list-magnifying-glass text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "done" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-check text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "rejected" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-x text-stone-600 dark:text-stone-300
                                    text-xl)))) "")))
    (else "")))

(define-public (emoji-for-ticket-type ticket-type)
  (cond
    ((equal? "task" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-gear-fine text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "bug" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-bug text-stone-600 dark:text-stone-300
                                    text-xl)))) "")))
    ((equal? "story" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-book-open-text text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "spike" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-flask text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "time-box" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-clock text-stone-600
                                    dark:text-stone-300 text-xl)))) "")))
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
      (div (@ (class ,(mk-class '(my-3))))
           (form (@ (onsubmit "addSearchQueryParam(this)"))
                 (div (@ (class ,(mk-class '(flex flex-row align-center
                                                  items-center justify-left
                                                  gap-2))))
                      ,(input #:label ""
                              #:name "search"
                              #:value (cond
                                        ((equal? #f search-query)
                                         "")
                                        (else search-query))
                              #:placeholder (G_ "Search...")
                              #:input-css '(p-3 max-w-prose
                                                rounded-xl
                                                border-stone-200
                                                dark:border-stone-700
                                                dark:text-stone-300
                                                dark:bg-stone-700/75
                                                border-2
                                                shadow-md)
                              #:input-flex-css #f)
                      ,(submit-button (G_ "search")))))))
  (export search-box))

(begin
  (define* (sort-select G_
                        #:key project-id
                        (current-sort #f)
                        (option-maker (lambda (s)
                                        (option #:value (format #f
                                                         "/tickets/~a?sort=~a"
                                                         project-id
                                                         (car s))
                                                #:content (cdr s)
                                                #:selected-value (format #f
                                                                  "/tickets/~a?sort=~a"
                                                                  project-id
                                                                  current-sort)))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2 w-min))))
           ,(select #:name 'sort-select
                    #:id 'sort-select
                    #:on-change "window.location.href=this.value;"
                    #:options (map option-maker
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

                                     ("priority-asc" unquote
                                      (G_ "Priority - Ascending"))
                                     ("priority-desc" unquote
                                      (G_ "Priority - Descending"))

                                     ))))))
  (export sort-select))

(begin
  (define (org2html org)
    (org-to-html org
                 #:link-classes '(text-blue-500 hover:text-blue-600
                                                dark:text-blue:400
                                                hover:dark:text-blue-300
                                                font-bold)
                 #:code-block-classes '(bg-stone-300/75 dark:bg-stone-700/75
                                                        text-stone-800
                                                        dark:text-stone-200
                                                        rounded-lg
                                                        my-3
                                                        py-2
                                                        px-3
                                                        whitespace-pre
                                                        overflow-x-scroll)
                 #:code-classes '(rounded-lg bg-stone-300/75
                                             dark:bg-stone-700/75
                                             text-stone-800
                                             px-2
                                             py-1
                                             dark:text-stone-200)))
  (export org2html))
