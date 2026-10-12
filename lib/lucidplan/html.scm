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
  #:use-module (hygguile sxml)
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
                                                                             "lucidplan is free software, available under the GNU AGPL v3 or newer")
                                                                            #:size '
                                                                            (text-sm))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             items-center items-center))))
                 ,(em (G_ "find here the:"))
                 ,(a "source code"
                     #:href "https://codeberg.org/jjba23/lucidplan"
                     #:size '(text-sm)))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             items-center items-center))))
                 ,(em (G_ "find here the:"))
                 ,(a "manual"
                     #:href "https://jointhefreeworld.org/manuals/lucidplan/"
                     #:size '(text-sm)))
            ,(em (G_ "Copyright © Josep Bigorra  &lt;jjbigorra@gmail.com&gt;")
                 #:size '(text-sm))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                             items-center items-center))))
                 (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                  items-center items-center))))
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
                                                  items-center
                                                  justify-center
                                                  items-center))))
                      ,(em "version:")
                      ,(mono (format #f "~a"
                                     (get-short-commit-sha))
                             #:size '(text-sm)
                             #:weight '(font-light)))))))

;; The classes a surface wears to read as frosted glass.  `hg-frost' is
;; hygguile's own frost layer: it paints the blur in a rounded layer of the
;; surface's own, so it survives the corner instead of leaving the square patch
;; of frost a plain `backdrop-blur' utility does.  It needs something to show
;; through, so the tint that travels with it is translucent -- a surface that
;; takes these drops any opaque `bg-...' of its own.
(define-public frosted-tint-classes
  '(hg-frost bg-stone-100/75 dark:bg-onyx-900/75))

(define-public (aurora-backdrop)
  "A fixed, purely decorative wash of colour behind the page.

Nothing here is content: it is the colour the frosted panes above it frost,
which is what makes them read as glass rather than as flat cards.  It is fixed
to the viewport, hidden from assistive technology, and never takes a pointer
event."
  `((div (@ (aria-hidden "true")
            (class ,(mk-class '(pointer-events-none
                                fixed
                                inset-0
                                z-0
                                overflow-hidden))))
         (div (@ (class ,(mk-class '(absolute
                                     top-0
                                     left-0
                                     w-128
                                     h-128
                                     rounded-full
                                     blur-3xl
                                     opacity-50
                                     bg-jeans-blue-500/50
                                     dark:bg-jeans-blue-700/50)))))
         (div (@ (class ,(mk-class '(absolute
                                     top-48
                                     right-0
                                     w-128
                                     h-128
                                     rounded-full
                                     blur-3xl
                                     opacity-50
                                     bg-asparagus-500/50
                                     dark:bg-asparagus-700/50)))))
         (div (@ (class ,(mk-class '(absolute
                                     bottom-0
                                     left-64
                                     w-128
                                     h-128
                                     rounded-full
                                     blur-3xl
                                     opacity-50
                                     bg-olive-500/25
                                     dark:bg-olive-700/50))))))))

;;; ===== The page writer =====

;;; A page is data: every view returns an SXML tree and `html-template' turns
;;; that tree into the HTML string a response carries.  The writing is
;;; hygguile's own `(hygguile sxml)' rather than the XML shorthand artanis
;;; ships, because these pages are served as `text/html' and a browser reading
;;; that shorthand takes `<div />' for an open tag: every element without
;;; children would then swallow the rest of the document.  A frosted pane is
;;; built out of exactly those elements -- the colour washes drifting behind
;;; it are childless discs -- so the difference is the whole page.
(define-public (render-page tree)
  "Serialize the SXML @var{tree} of a whole page to an HTML string."
  (call-with-output-string (lambda (port) (sxml->xml tree port))))

(begin
  (define* (html-template G_
                          #:key (title "LucidPlan")
                          body
                          (proj '())
                          (is-logged-in #f))
    (let* ((project-select (if (nil? proj)
                               '()
                               `(select (@ (onchange
                                            "window.location.href=this.value;")
                                           (class ,(mk-class '(text-sm
                                                               md:text-md
                                                               text-stone-900
                                                               dark:text-stone-100
                                                               bg-transparent
                                                               rounded-lg
                                                               px-2
                                                               py-1
                                                               border-0
                                                               font-medium
                                                               font-sans
                                                               cursor-pointer
                                                               hover:bg-stone-300/75
                                                               dark:hover:bg-onyx-700/75
                                                               focus:ring-2
                                                               outline-none))))
                                        (option "🔗 go to")
                                        ,@(map (lambda (p)
                                                 `(option (@ (value ,(format
                                                                             #f
                                                                             "/tickets/~a"
                                                                             (assq-ref
                                                                              p
                                                                              'human-id)))
                                                             (class ,(mk-class '
                                                                      (text-sm
                                                                       font-light
                                                                       lowercase
                                                                       text-onyx-700))))
                                                          ,(assq-ref p
                                                                     'title)))
                                               proj))))
           
           ;; The bar is the framework's frosted glass header: the same pane
           ;; the cards wear, tinted with the brand blue, pinned to the top,
           ;; and left square so the dropdown it holds can escape its edge.
           (nav (glass-panel
                 `(

                      ;; Left Section: Brand/Logo
                      (div (@ (class ,(mk-class '(flex items-center
                                                       justify-center
                                                       md:justify-start w-full
                                                       md:w-auto))))
                           (a (@ (href "/")
                                 (class ,(mk-class '(flex flex-row
                                                          gap-2
                                                          items-center
                                                          no-underline
                                                          hover:opacity-80
                                                          transition-opacity))))
                              ,(h4 (list (icon #:type 'scroll
                                               #:icon-css '(text-xl
                                                            text-stone-300
                                                            dark:text-stone-300
                                                            hover:text-white
                                                            dark:hover:text-white))
                                         "&nbsp;"
                                         `(span ,(G_ "LucidPlan")))
                                   #:color '(text-stone-300
                                             dark:text-stone-300
                                             hover:text-white
                                             dark:hover:text-white)
                                   #:size '(text-xl font-bold))))

                      ;; Right Section: Navigation Links & Controls
                      (div (@ (class ,(mk-class '(flex flex-row
                                                       flex-wrap
                                                       items-center
                                                       justify-center
                                                       md:justify-end
                                                       gap-3
                                                       md:gap-4
                                                       w-full
                                                       md:w-auto))))

                           ;; Auth Link
                           ,(if (not is-logged-in)
                                `(a (@ (class ,(mk-class '(lowercase font-sans
                                                           font-medium
                                                           flex
                                                           items-center
                                                           no-underline
                                                           hover:opacity-80)))
                                       (href "/login"))
                                    ,(icon-button #:icon 'sign-in
                                                  #:content (G_ "login")
                                                  #:size '(text-sm md:text-md)
                                                  #:extra-classes '(py-1
                                                                    md:py-2
                                                                    px-2
                                                                    md:px-3
                                                                    max-w-prose
                                                                    cursor-pointer
                                                                    text-onyx-700
                                                                    dark:text-stone-300)
                                                  #:icon-css '(text-md
                                                               text-onyx-700
                                                               dark:text-stone-300)))
                                `(a (@ (class ,(mk-class '(lowercase font-sans
                                                           font-medium
                                                           flex
                                                           items-center
                                                           no-underline
                                                           hover:opacity-80)))
                                       (href "/me"))
                                    ,(icon-button #:icon 'user-circle
                                                  #:content (G_ "me")
                                                  #:size '(text-sm md:text-md)
                                                  #:extra-classes '(py-1
                                                                    md:py-2
                                                                    px-2
                                                                    md:px-3
                                                                    max-w-prose
                                                                    cursor-pointer
                                                                    text-onyx-700
                                                                    dark:text-stone-300)
                                                  #:icon-css '(text-md
                                                               text-onyx-700
                                                               dark:text-stone-300))))

                           ;; Projects Link
                           (a (@ (class ,(mk-class '(lowercase no-underline
                                                     font-sans
                                                     font-medium
                                                     flex
                                                     items-center
                                                     hover:opacity-80)))
                                 (href "/projects"))
                              ,(icon-button #:icon 'align-bottom
                                            #:content (G_ "projects")
                                            #:size '(text-sm md:text-md)
                                            #:extra-classes '(py-1 md:py-2
                                                              px-2
                                                              md:px-3
                                                              max-w-prose
                                                              cursor-pointer
                                                              text-onyx-700
                                                              dark:text-stone-300)
                                            #:icon-css '(text-md text-onyx-700
                                                         dark:text-stone-300)))

                           ;; Theme Toggle
                           ,(ui-mode-button #:icon ""
                                            #:size '(text-md)
                                            #:icon-css '(text-onyx-300
                                                         dark:text-stone-300
                                                         text-md)
                                            #:family '(font-sans)
                                            #:content `((div ,(icon #:type 'sun
                                                                    #:icon-css '
                                                                    (text-md
                                                                     lg:text-lg
                                                                     text-onyx-300
                                                                     dark:text-stone-300))
                                                             ,(icon #:type 'moon
                                                                    #:icon-css '
                                                                    (text-md
                                                                     lg:text-lg
                                                                     text-onyx-300
                                                                     dark:text-stone-300))))
                                            #:color '(text-onyx-900
                                                      dark:text-stone-100)
                                            #:weight '(font-light)
                                            #:extra-classes '(py-1 md:py-2
                                                              px-2
                                                              lg:px-3
                                                              max-w-prose
                                                              cursor-pointer
                                                              hover:opacity-80
                                                              transition-opacity))
                           ;; Dropdown Select
                           ,project-select))
                 #:tag 'header
                 #:position '(sticky top-0 z-48)
                 #:rounding '()
                 #:border '(border-b-2 border-stone-500/25
                                       dark:border-onyx-100/25)
                 #:tint '(bg-jeans-blue-500/75 dark:bg-jeans-blue-900/75)
                 #:shadow '(shadow-lg)
                 #:padding '(px-4 py-3)
                 #:classes '(w-full))))
      
      ;; Output Page
      (render-page
       `(,doctype-html (html (@ (lang en))
                            (head (title "LucidPlan")
                                  ,(hygguile-recommended-meta)
                                  ,(hygguile-recommended-resources #:fonts (custom-fonts))

                                  (link (@ (rel "shortcut icon")
                                           (href "/favicon.ico")
                                           (type "image/x-icon")) "")
                                  (link (@ (rel icon)
                                           (href "/favicon.ico")
                                           (type "image/x-icon")) "")
                                  (link (@ (rel stylesheet)
                                           (href
                                            "/css/lucidplan.olive.min.css"))
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
                            (body (@ (class ,(mk-class '(min-h-screen
                                                         bg-stone-100
                                                         dark:bg-onyx-900
                                                         font-sans
                                                         relative
                                                         overflow-x-hidden))))
                                  ;; The colour the panes above frost.
                                  ,(aurora-backdrop)
                                  ,notification-tray
                                  ,(with-footer #:main `((div (@ (class ,(mk-class '
                                                                          (flex
                                                                           flex-col
                                                                           justify-between
                                                                           min-h-96
                                                                           relative
                                                                           z-10))))
                                                              (div (@ (class ,
                                                                             (mk-class '
                                                                              (flex
                                                                               flex-col
                                                                               gap-8
                                                                               justify-start))))
                                                                   ,@nav
                                                                   ,body)))
                                                #:footer (page-footer G_))))))))
  (export html-template frosted-tint-classes render-page))

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
                 (div (@ (class ,(mk-class '(flex flex-row
                                                  flex-wrap
                                                  items-center
                                                  items-center
                                                  justify-start
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
                      ,(button (G_ "search")
                               #:type 'submit
                               #:background '(bg-jeans-blue-500
                                              dark:bg-jeans-blue-900)
                               #:color '(text-stone-300)
                               #:hover-background '(hover:bg-jeans-blue-700
                                                    dark:hover:bg-jeans-blue-700)))))))
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
    ;; The control sizes to its own label: hygguile's `select' fills its box
    ;; (`w-full'), so a wrapper a min-content wide would squeeze the chosen
    ;; sort -- the one thing a sort control has to say -- down to its
    ;; chevron.
    `((div (@ (class ,(mk-class '(flex flex-col gap-2 w-fit))))
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
  (define* (org2html org
                     #:key (extra-paragraph-classes '(text-onyx-500
                                                      dark:text-stone-500
                                                      text-md)))
    (parameterize ((link-classes '(text-jeans-blue-500
                                   hover:text-jeans-blue-700
                                   dark:text-jeans-blue:300
                                   dark:hover:text-jeans-blue-100
                                   hover:underline font-bold))
                   (italic-classes '(italic))
                   (ordered-list-classes '(list-decimal ml-3))
                   (ordered-list-item-classes '(font-sans text-onyx-500
                                                          dark:text-stone-500
                                                          text-md leading-7))
                   (unordered-list-classes '(list-disc ml-3))
                   (unordered-list-item-classes '(font-sans text-onyx-500
                                                  dark:text-stone-500 text-md
                                                  leading-7))
                   (heading-classes (lambda (level)
                                      (case level
                                        ((1)
                                         '(my-2 text-4xl font-bold
                                                text-onyx-300
                                                dark:text-stone-300))
                                        ((2)
                                         '(my-2 text-3xl font-bold
                                                text-onyx-300
                                                dark:text-stone-300))
                                        ((3)
                                         '(my-2 text-2xl font-bold
                                                text-onyx-300
                                                dark:text-stone-300))
                                        (else '(my-2 text-xl font-bold
                                                     text-onyx-300
                                                     dark:text-stone-300)))))
                   (separator-classes '(my-8 mx-4 border-2 border-stone-500/50
                                             dark:border-onyx-100/50))
                   (paragraph-classes (append extra-paragraph-classes
                                              '(font-sans leading-7 max-w-196
                                                          wrap-break-word)))
                   (bold-classes '(font-bold text-onyx-500 dark:text-stone-500))
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

(define-public grab-indicator
  `((div (@ (class ,(mk-class '(text-onyx-100 dark:text-stone-300 cursor-grab
                                              active:cursor-grabbing flex
                                              items-center))))
         (svg (@ (class "w-4 h-4")
                 (fill "currentColor")
                 (viewBox "0 0 16 16"))
              (path (@ (d
                        "M7 2a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zM7 6a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm-3 4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm-3 4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0z")))))))
