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
  #:use-module (orgwebalchemy html)
  #:use-module (artanis route))

(define-public asset-library
  (build-asset-library '("pub/js/tickets.js" "pub/js/symbanta.js")))

(define-public (page-footer G_)
  (footer `(,(em (G_ "lucidplan - free project management for everyone")) ,(em
                                                                            (G_
                                                                             "lucidplan is free software, available under the GNU AGPL v3 or newer"))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center items-center))))
                 ,(em (G_ "find the source code here"))
                 ,(a "codeberg.org/jjba23/lucidplan"
                     #:href "https://codeberg.org/jjba23/lucidplan"
                     #:size '(text-sm)))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center items-center))))
                 ,(em (G_ "find the manual here"))
                 ,(a "jointhefreeworld.org/manuals/lucidplan/"
                     #:href "https://jointhefreeworld.org/manuals/lucidplan/"
                     #:size '(text-sm)))
            ,(em (G_ "Copyright © Josep Bigorra  &lt;jjbigorra@gmail.com&gt;"))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                             align-center items-center))))
                 (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                  align-center items-center))))
                      (a (@ (href "javascript:void(0)")
                            (class ,(mk-class '(no-underline text-onyx-500
                                                dark:text-stone-300
                                                cursor-pointer font-light)))
                            (onclick
                             "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                         "⬆️ back to the top"))
                 (div (@ (class ,(mk-class '(flex flex-row
                                                  flex-wrap
                                                  gap-2
                                                  align-center
                                                  justify-center
                                                  items-center))))
                      ,(em "version:")
                      ,(mono (format #f "~a"
                                     (get-short-commit-sha))
                             #:size '(text-sm)
                             #:weight '(font-light)))))))

(begin
  (define* (html-template G_
                          #:key (title "LucidPlan")
                          body
                          (proj '())
                          (is-logged-in #f))
    (define custom-fonts
      `((link (@ (rel preconnect)
                 (href "https://fonts.googleapis.com")))
        (link (@ (rel preconnect)
                 (href "https://fonts.gstatic.com")
                 (crossorigin "")))
        (link (@ (rel stylesheet)
                 (href
                  "https://fonts.googleapis.com/css2?family=Intel+One+Mono:ital,wght@0,300..700;1,300..700&family=Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800&display=swap")))))

    (define nav
      `(div (@ (class ,(mk-class '(flex flex-row
                                        flex-wrap
                                        gap-3
                                        p-3
                                        bg-asparagus-500/90
                                        dark:bg-asparagus-900/50
                                        items-center
                                        justify-left
                                        shadow-2xl))))
            (div (@ (class ,(mk-class '(px-4 py-1 w-full md:w-48))))
                 (a (@ (href "/")
                       (class ,(mk-class '(flex flex-row gap-2 items-center
                                                no-underline))))
                    ,(h4 (list (icon #:type 'scroll
                                     #:icon-css '(text-xl text-stone-300
                                                  dark:text-stone-300
                                                  hover:text-white
                                                  hover:dark:text-white))
                               "&nbsp;"
                               `(span ,(G_ "LucidPlan")))
                         #:color '(text-stone-300 dark:text-stone-300
                                                  hover:text-white
                                                  hover:dark:text-white)
                         #:size '(text-xl))))
            ,(if (not is-logged-in)
                 `((a (@ (class ,(mk-class '(lowercase font-sans font-medium
                                                       px-2 flex-items-center
                                                       gap-2)))
                         (href "/login"))
                      ,(icon-button #:icon 'sign-in
                                    #:content (G_ "login")
                                    #:size '(text-md)
                                    #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                                    #:icon-css '(text-md text-onyx-700
                                                         dark:text-stone-300))))
                 `((a (@ (class ,(mk-class '(lowercase

                                                       font-sans
                                                       font-medium
                                                       px-2
                                                       flex-items-center
                                                       gap-2)))
                         (href "/me"))
                      ,(icon-button #:icon 'user-circle
                                    #:content (G_ "me")
                                    #:size '(text-md)
                                    #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                                    #:icon-css '(text-md text-onyx-700
                                                         dark:text-stone-300)))))
            (a (@ (class ,(mk-class '(lowercase no-underline
                                                font-sans
                                                font-medium
                                                px-2
                                                flex
                                                items-center
                                                gap-2)))
                  (href "/projects"))
               ,(icon-button #:icon 'align-bottom
                             #:content (G_ "projects")
                             #:size '(text-md)
                             #:extra-classes '(py-1 md:py-2
                                                    px-2
                                                    md:px-3
                                                    max-w-prose
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300)
                             #:icon-css '(text-md text-onyx-700
                                                  dark:text-stone-300)))

            ,(ui-mode-button #:size '(text-md)
                             #:extra-classes '(py-1 md:py-2
                                                    px-2
                                                    md:px-3
                                                    max-w-prose
                                                    cursor-pointer
                                                    text-onyx-700
                                                    dark:text-stone-300)
                             #:icon-css '(text-md text-onyx-700
                                                  dark:text-stone-300))
            ,(if (nil? proj)
                 '()
                 `((select (@ (onchange "window.location.href=this.value;")
                              (class ,(mk-class '(text-md text-stone-100
                                                          dark:text-stone-100
                                                          bg-transparent
                                                          font-medium
                                                          font-sans))))
                           (option "🔗 go to")
                           ,(map (lambda (p)
                                   `(option (@ (value ,(format #f
                                                               "/tickets/~a"
                                                               (assq-ref p
                                                                         'human-id)))
                                               (class ,(mk-class '(text-sm
                                                                   font-light
                                                                   lowercase
                                                                   text-onyx-700))))
                                            ,(assq-ref p
                                                       'title))) proj))))))
    `(,doctype-html (html (@ (lang en))
                          (head (title "LucidPlan")
                                ,(hygguile-recommended-meta)
                                ,(hygguile-recommended-resources #:fonts
                                                                 custom-fonts)

                                (link (@ (rel "shortcut icon")
                                         (href "/favicon.ico")
                                         (type "image/x-icon")) "")
                                (link (@ (rel icon)
                                         (href "/favicon.ico")
                                         (type "image/x-icon")) "")
                                (link (@ (rel stylesheet)
                                         (href "/css/lucidplan.olive.min.css"))
                                      "")

                                (script (@ (type speculationrules))
                                        "
{
  \"prerender\": [
    {
      \"where\": {
        \"selector_matches\": \"a\"
      }
    }
  ]
}
")
                                (script ,(assoc-ref asset-library
                                                    "pub/js/symbanta.js"))
                                (script ,(assoc-ref asset-library
                                                    "pub/js/tickets.js")))
                          (body (@ (class ,(mk-class '(bg-stone-100
                                                       dark:bg-onyx-900
                                                       font-sans))))
                                ,notification-tray
                                ,(with-footer #:main `((div (@ (class ,(mk-class '
                                                                        (flex
                                                                         flex-col
                                                                         justify-between
                                                                         min-h-96))))
                                                            (div (@ (class ,(mk-class '
                                                                             (flex
                                                                              flex-col
                                                                              gap-8
                                                                              justify-start))))
                                                                 ,nav
                                                                 ,body)))
                                              #:footer (page-footer G_))))))
  (export html-template))

(define-public (emoji-for-ticket-status ticket-status)
  (cond
    ((equal? "todo" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-anchor text-onyx-300
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "work-in-progress" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-gear-fine text-onyx-300
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "in-review" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-list-magnifying-glass text-onyx-300
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "done" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-check text-onyx-300 dark:text-stone-300
                                    text-xl)))) "")))
    ((equal? "rejected" ticket-status)
     `((i (@ (class ,(mk-class '(ph ph-x text-onyx-300 dark:text-stone-300
                                    text-xl)))) "")))
    (else "")))

(define-public (emoji-for-ticket-type ticket-type)
  (cond
    ((equal? "task" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-gear-fine text-onyx-300
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "bug" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-bug text-onyx-300 dark:text-stone-300
                                    text-xl)))) "")))
    ((equal? "story" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-book-open-text text-onyx-300
                                    dark:text-stone-300 text-xl)))) "")))
    ((equal? "spike" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-flask text-onyx-300 dark:text-stone-300
                                    text-xl)))) "")))
    ((equal? "time-box" ticket-type)
     `((i (@ (class ,(mk-class '(ph ph-clock text-onyx-300 dark:text-stone-300
                                    text-xl)))) "")))
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
                                                border-stone-300
                                                dark:border-onyx-100
                                                dark:text-stone-300
                                                dark:bg-onyx-700/75
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
    (parameterize ((link-classes '(text-jeans-blue-500
                                   hover:text-jeans-blue-700
                                   dark:text-jeans-blue:300
                                   hover:dark:text-jeans-blue-100
                                   hover:underline font-bold))
                   (separator-classes '(my-8 mx-4 border-2 border-stone-500/50
                                             dark:border-onyx-100/50))
                   (paragraph-classes '(font-sans text-onyx-500
                                                  dark:text-stone-500
                                                  text-md
                                                  leading-7
                                                  max-w-196
                                                  break-words
                                                  truncate))
                   (bold-classes '(font-sans font-bold
                                             text-onyx-500
                                             dark:text-stone-500
                                             text-md
                                             leading-7
                                             max-w-196
                                             break-words
                                             truncate))
                   (example-block-classes '(bg-stone-300/75
                                            dark:bg-onyx-700/75
                                            text-onyx-700
                                            dark:text-stone-300
                                            rounded-lg
                                            my-3
                                            py-2
                                            px-3
                                            whitespace-pre
                                            font-mono
                                            overflow-x-scroll))
                   (quote-block-classes '(bg-stone-300/75 dark:bg-onyx-700/75
                                                          text-onyx-700
                                                          dark:text-stone-300
                                                          rounded-lg
                                                          my-3
                                                          py-2
                                                          px-3
                                                          whitespace-pre
                                                          font-sans
                                                          overflow-x-scroll))
                   (code-block-classes '(bg-stone-300/75 dark:bg-onyx-700/75
                                                         text-onyx-700
                                                         dark:text-stone-300
                                                         rounded-lg
                                                         my-3
                                                         py-2
                                                         px-3
                                                         whitespace-pre
                                                         font-mono
                                                         overflow-x-scroll))
                   (code-classes '(rounded-lg bg-stone-300/75
                                              dark:bg-onyx-700/75
                                              text-onyx-700
                                              px-2
                                              py-1
                                              font-mono
                                              dark:text-stone-300)))
      (org->html org)))
  (export org2html))
