;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (hygguile embed css)
  #:use-module (hygguile embed js)
  #:use-module (hygguile js)
  #:use-module (hygguile sxml)
  #:use-module (artanis i18n sxml)
  #:use-module (kracht prelude)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (orgwebalchemy html)
  #:use-module (lucidplan config)
  #:use-module (lucidplan domain)
  #:use-module (artanis route))

;; The scripts an instance ships, read once at start-up and inlined into the
;; page that needs them.  Two of them are on every page, because what they do
;; -- dragging a ticket, saying how strong a password is -- can happen on any
;; of them; `poker.js' is only on the page of somebody sitting at a table, so
;; it is read into the library here and spliced in by that page alone.  The
;; last one is the page's polish (see `pub/js/lucidplan-luxe.js'): it is on
;; every page too, because every page has a bar, cards and sections that
;; arrive.
(define-public asset-library
  (build-asset-library '("pub/js/tickets.js" "pub/js/symbanta.js"
                         "pub/js/password-strength.js" "pub/js/poker.js"
                         "pub/js/lucidplan-luxe.js")))

;; The footer says only what a footer is for: what this instance is, under
;; which licence, where to read more, and how to get back to the top.  The
;; instance's own name and the addresses it points at are the operator's, so
;; they come from the configuration rather than from this file.
(define-public (page-footer G_)
  (footer `(div (@ (class ,(mk-class '(flex flex-col
                                            items-center
                                            gap-2
                                            px-4
                                            py-6
                                            text-center))))
                ,(em (format #f "~a - ~a"
                             (config-ref 'site-name)
                             (G_
                              "free and open project management for everyone"))
                     #:size '(text-sm))
                (div (@ (class ,(mk-class '(flex flex-row
                                                 flex-wrap
                                                 items-center
                                                 justify-center
                                                 gap-x-4
                                                 gap-y-1))))
                     ,(a (G_ "about")
                         #:href "/about"
                         #:size '(text-sm))
                     ,(a (G_ "source code")
                         #:href (config-ref 'source-url)
                         #:size '(text-sm))
                     ,(a (G_ "manual")
                         #:href (config-ref 'manual-url)
                         #:size '(text-sm))
                     (a (@ (href "javascript:void(0)")
                           (class ,(mk-class '(no-underline text-onyx-500
                                               dark:text-stone-300
                                               cursor-pointer text-sm)))
                           (onclick
                            "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                        ,(G_ "back to top")))
                ,(em (format #f "© ~a - ~a"
                             (config-ref 'copyright-holder)
                             (G_ "GNU AGPL v3 or newer"))
                     #:size '(text-sm))
                ,(mono (format #f "~a"
                               (get-short-commit-sha))
                       #:size '(text-sm)
                       #:weight '(font-light)))))

;; The classes a surface wears to read as frosted glass.  `hg-frost' is
;; hygguile's own frost layer: it paints the blur in a rounded layer of the
;; surface's own, so it survives the corner instead of leaving the square patch
;; of frost a plain `backdrop-blur' utility does.  It needs something to show
;; through, so the tint that travels with it is translucent -- a surface that
;; takes these drops any opaque `bg-...' of its own.
(define-public frosted-tint-classes
  '(hg-frost bg-stone-100/75 dark:bg-onyx-900/75))

;; A surface set *into* a pane rather than standing on the page in its own
;; right: the same hairline the panes wear, a tint a shade off the glass, and
;; no frost of its own.  The frost is left out on purpose -- two blurs stacked
;; read as fog rather than as depth, and the second one costs a second backdrop
;; pass for nothing -- so this is what a form inside a window, or a section
;; inside a card, is made of.
(define-public recessed-tint-classes
  '(bg-stone-200/45 dark:bg-onyx-800/45
    border-2 border-stone-500/25 dark:border-onyx-100/25))

;; The surface of a chip: a fact, or a field a reader can change where it
;; stands.  It is a tint and nothing else -- no frame, no frost.  Five framed
;; chips in a row are five boxes, and a card that holds them stops reading as
;; a card and starts reading as a form; five tints read as what they are,
;; which is five values.
(define-public chip-tint-classes
  '(bg-stone-200/50 dark:bg-onyx-700/50))

;;; ===== The window a pane can wear =====

;;; A pane of frosted glass is the application's surface; a *window* is the
;;; thing a piece of software is used through.  The difference is a bar across
;;; the top with three lights at its start and the name of what is open in it,
;;; and it is worth borrowing because it places a pane before a reader has read
;;; a word of it: the shape is recognised as something with edges that was
;;; opened rather than as a section of a document that happened.
;;;
;;; Nothing here is a control.  The lights are decoration, which is why the row
;;; is hidden from assistive technology and carries no links: a control that
;;; only looks like a close button is a lie told to a reader who tries it.  What
;;; they look like is drawn by `lucidplan-luxe.css', which is where the three
;;; colours and the glyphs revealed on hover live.

(define-public (window-controls)
  "The three lights a window wears at the start of its title bar."
  `((span (@ (class ,(mk-class '(lp-traffic)))
             (aria-hidden "true"))
          (span (@ (class ,(mk-class '(lp-dot lp-dot-close)))))
          (span (@ (class ,(mk-class '(lp-dot lp-dot-min)))))
          (span (@ (class ,(mk-class '(lp-dot lp-dot-zoom))))))))

(define* (window-titlebar #:key title trailing)
  "The bar across the top of a pane, with its lights, its TITLE and TRAILING.

TITLE is the name of what the pane is, shown in a mono face the way a window
names its document; an empty or absent one draws no name at all rather than an
empty slot.  TRAILING is a node, or a list of them, placed at the far end of
the bar -- the one place in the chrome where a page may put a real control,
which is how the front page's language chooser ends up in its title bar."
  `(div (@ (class ,(mk-class '(lp-titlebar))))
        ,@(window-controls)
        ,@(if (and (string? title)
                   (not (string-null? title)))
              `((span (@ (class ,(mk-class '(lp-window-title min-w-0))))
                      ,title))
              '())
        ,@(if (nil? trailing)
              '()
              `((div (@ (class ,(mk-class '(ml-auto flex flex-row flex-wrap
                                                  items-center gap-2))))
                     ,@(if (list? trailing)
                           trailing
                           (list trailing)))))))

;;; ===== A link in the bar =====

;;; The bar carries the way through the application, and the whole of it is
;;; four or five of these.  They wear no box: see `lp-quiet' in
;;; `pub/css/lucidplan-luxe.css' for why a row of plates is worse than a row
;;; of words.  What they wear instead is the bar's own pale ink -- the ink the
;;; brand at the other end already wears -- so the bar reads as one surface
;;; with marks on it rather than as a shelf of buttons.
;;;
;;; This is one procedure rather than five copies of the same twenty lines
;;; because the five differ only in which word they say: a bar whose controls
;;; drift apart from one another one keyword at a time is the usual way a
;;; toolbar starts to look assembled rather than designed.

(define* (bar-link #:key icon label href)
  "A control of the bar: the mark, the word, and no box around either."
  `(a (@ (class ,(mk-class '(lowercase font-sans font-medium flex
                                      items-center no-underline
                                      hover:opacity-80)))
         (href ,href))
      ,(icon-button #:icon icon
                    #:content label
                    #:size '(text-sm md:text-md)
                    #:background '()
                    #:hover-background '()
                    #:border-size '()
                    #:border-color '()
                    #:shadow '()
                    #:color '(text-stone-300 dark:text-stone-300)
                    #:icon-css '(text-md text-stone-300 dark:text-stone-300)
                    #:extra-classes '(lp-quiet py-1 px-2 md:px-3
                                               max-w-prose cursor-pointer))))

;;; ===== Controls that stay out of the way =====

;;; A card is a surface already.  Filling it with a second row of plates --
;;; every link a button, every button a box -- is how a page ends up shouting:
;;; six things at the same volume, so nothing is the thing to do and the eye
;;; has nowhere to rest.  These two are the shapes a card's actions take
;;; instead.  The class that does the work is `lp-quiet' in
;;; `pub/css/lucidplan-luxe.css' -- the plate comes off, the ink and the
;;; padding and the focus ring stay, and a veil of the control's own colour
;;; appears under the pointer -- and these are what a view reaches for rather
;;; than six copies of the same keyword soup.

(define* (quiet-link #:key icon label href
                     (ink '(text-onyx-700 dark:text-stone-300)))
  "An action inside a card: the mark, the word, and no plate around them.

INK is the colour pair the control is drawn in: the card's ordinary ink by
default, and the red pair for the one action a reader cannot take back."
  `(a (@ (class ,(mk-class '(no-underline)))
         (href ,href))
      ,(icon-button #:icon icon
                    #:content label
                    #:size '(text-sm)
                    #:background '()
                    #:hover-background '()
                    #:border-size '()
                    #:border-color '()
                    #:shadow '()
                    #:color ink
                    #:icon-css (append '(text-md) ink)
                    #:extra-classes '(lp-quiet py-1 px-2
                                               max-w-prose cursor-pointer))))

(define* (quiet-submit content
                       #:key (ink '(text-redwood-700 dark:text-redwood-300)))
  "A form's submit control, quiet: the word in INK and no plate around it."
  (button content
          #:type 'submit
          #:background '()
          #:hover-background '()
          #:border-size '()
          #:border-color '()
          #:shadow '()
          #:size '(text-sm)
          #:color ink
          #:extra-classes '(lp-quiet py-1 px-2 max-w-prose cursor-pointer)))

(define-public (aurora-backdrop)
  "A fixed, purely decorative wash of colour behind the page.

Nothing here is content: it is the colour the frosted panes above it frost,
which is what makes them read as glass rather than as flat cards.  It is fixed
to the viewport, hidden from assistive technology, and never takes a pointer
event.

The discs drift -- `hg-blob' is hygguile's own slow drift, and it stops for
anyone who asks for less motion -- each at its own pace and out of step with
the others, because a still wash behind moving glass reads as a printed
backdrop while a slowly turning one reads as light through a window.  The last
layer is the grain: two or three percent of noise, which is what keeps two
large soft gradients from banding on a wide screen."
  `((div (@ (aria-hidden "true")
            (class ,(mk-class '(pointer-events-none fixed inset-0 z-0
                                                    overflow-hidden))))
         (div (@ (class ,(mk-class '(absolute top-0
                                              left-0
                                              w-128
                                              h-128
                                              rounded-full
                                              blur-3xl
                                              opacity-50
                                              hg-blob
                                              bg-jeans-blue-500/50
                                              dark:bg-jeans-blue-700/50)))
              (style "--hg-blob-duration: 42s; --hg-blob-x: 2rem; --hg-blob-y: 1.5rem")))
         (div (@ (class ,(mk-class '(absolute top-48
                                              right-0
                                              w-128
                                              h-128
                                              rounded-full
                                              blur-3xl
                                              opacity-50
                                              hg-blob
                                              bg-asparagus-500/50
                                              dark:bg-asparagus-700/50)))
              (style "--hg-blob-duration: 58s; --hg-blob-x: -2.5rem; --hg-blob-y: 2rem; animation-delay: -12s")))
         (div (@ (class ,(mk-class '(absolute top-1/3
                                              left-1/2
                                              w-96
                                              h-96
                                              rounded-full
                                              blur-3xl
                                              opacity-50
                                              hg-blob
                                              bg-redwood-500/50
                                              dark:bg-redwood-700/50)))
              (style "--hg-blob-duration: 36s; --hg-blob-x: -1.5rem; --hg-blob-y: -2rem; animation-delay: -24s")))
         (div (@ (class ,(mk-class '(absolute bottom-0
                                              left-64
                                              w-128
                                              h-128
                                              rounded-full
                                              blur-3xl
                                              opacity-50
                                              hg-blob
                                              bg-olive-500/25
                                              dark:bg-olive-700/50)))
              (style "--hg-blob-duration: 50s; --hg-blob-x: 3rem; --hg-blob-y: -1.5rem; animation-delay: -6s")))
         (div (@ (class ,(mk-class '(absolute bottom-1/4
                                              right-1/4
                                              w-96
                                              h-96
                                              rounded-full
                                              blur-3xl
                                              opacity-50
                                              hg-blob
                                              bg-naples-yellow-500/50
                                              dark:bg-naples-yellow-700/50)))
              (style "--hg-blob-duration: 64s; --hg-blob-x: -2rem; --hg-blob-y: 2.5rem; animation-delay: -38s")))
         (div (@ (class ,(mk-class '(lp-grain absolute inset-0))))))))

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
  (call-with-output-string (lambda (port)
                             (sxml->xml tree port))))

;;; ===== What a page says about itself to a machine =====

;;; Search engines and link previews read a page through a few tags rather
;;; than through its prose: a title, a description, a canonical address, the
;;; Open Graph and Twitter cards that turn a shared link into a preview, and a
;;; block of JSON-LD that says what the application is.  They are built from
;;; the instance's configuration rather than written into every view, so a
;;; self-hosted LucidPlan describes itself in its own name and on its own
;;; address -- and a page only has to say what makes it that page.

(define (page-title title)
  "Compose the @code{<title>} of a page from its own @var{title} and the
instance's name.  A page with no title of its own, and the front page, is
titled with the instance's name and its tagline."
  (let ((name (config-ref 'site-name)))
    (cond
      ((or (not title)
           (not (string? title))
           (string-null? title))
       (format #f "~a - ~a" name
               (config-ref 'site-tagline)))
      ((string=? title name)
       name)
      (else (format #f "~a · ~a" title name)))))

(define (meta-tag name content)
  "A @code{<meta name=... content=...>} tag."
  `(meta (@ (name ,name)
            (content ,content))))

(define (property-tag property content)
  "A @code{<meta property=... content=...>} tag, the Open Graph spelling."
  `(meta (@ (property ,property)
            (content ,content))))

(define (instance-logo)
  "The image a link preview should show, as an absolute URL, or @code{#f}.

The configured @code{site-logo} may be an absolute URL, in which case it is
answered as it stands, or a path on this instance -- the project's own mark
lives at @code{/img/logo-small.jpg} -- which is made absolute against the
instance's own address.  A preview image is fetched by a service that has
none of this page's context, so it has to be absolute either way."
  (let ((configured (config-ref 'site-logo)))
    (cond
      ((not (string? configured))
       #f)
      ((string-null? configured)
       #f)
      ((string-contains configured "://")
       configured)
      (else (absolute-url configured)))))

(define* (seo-meta #:key title description canonical-path)
  "The metadata tags a page carries: a description, a canonical address, an
Open Graph and Twitter preview, and the JSON-LD that names the application.

@var{canonical-path} is the page's path on this instance; it is turned into
an absolute URL against the configured @code{site-url} (or the host the
request arrived on) by @code{absolute-url}.  @var{title} and @var{description}
fall back to the instance's own when a page has none of its own."
  (let* ((name (config-ref 'site-name))
         (desc (if (and (string? description)
                        (not (string-null? description))) description
                   (config-ref 'site-description)))
         (url (absolute-url (or canonical-path "/")))
         (logo (instance-logo))
         (has-logo (and logo #t))
         (keywords (config-ref 'site-keywords))
         (json (scm->json-string `((@context . "https://schema.org")
                                   (@type . "WebApplication")
                                   (name unquote name)
                                   (url unquote url)
                                   (description unquote desc)
                                   (applicationCategory . "BusinessApplication")
                                   (operatingSystem . "Any")
                                   (license . "https://www.gnu.org/licenses/agpl-3.0.html")
                                   (offers (@type . "Offer")
                                           (price . "0")
                                           (priceCurrency . "EUR"))))))
    `(,(meta-tag "description" desc) ,(meta-tag "keywords"
                                                (if (list? keywords)
                                                    (string-join keywords ", ")
                                                    keywords))
      ,(meta-tag "author"
                 (config-ref 'copyright-holder))
      (link (@ (rel "canonical")
               (href ,url)))
      ,(property-tag "og:type" "website")
      ,(property-tag "og:site_name" name)
      ,(property-tag "og:title"
                     (page-title title))
      ,(property-tag "og:description" desc)
      ,(property-tag "og:url" url)
      ,(property-tag "og:locale"
                     (config-ref 'site-language))
      ,@(if has-logo
            `(,(property-tag "og:image" logo) ,(property-tag "og:image:alt"
                                                             name)
              ,(meta-tag "twitter:image" logo))
            '())
      ,(meta-tag "twitter:card"
                 (if has-logo "summary_large_image" "summary"))
      ,(meta-tag "twitter:title"
                 (page-title title))
      ,(meta-tag "twitter:description" desc)
      (script (@ (type "application/ld+json"))
              ,json))))

;;; ===== The theme, served by this instance =====

;;; A page used to be assembled partly from other people's servers: the Lucida
;;; stylesheet was written into every head pointing at jointhefreeworld.org,
;;; the icon stylesheet was pulled from a CDN, and two Google Fonts hosts were
;;; woken with a preconnect before either was read.  A self-hosted LucidPlan
;;; should not depend on hosts its operator does not run, and none of those
;;; files is large, so they now live under `pub/' as files of this project
;;; (`maak fetch-assets' refetches them from where they came from) and are
;;; handed out by the instance itself.
;;;
;;; Where they are handed out from is configuration: `asset-url' names a base
;;; -- a static host, a CDN, another origin -- and every address is built on
;;; it.  Left empty it is the instance that answered the page, so the
;;; addresses stay relative and no host has to be written down twice.

(define (asset-href path)
  "The address of one of this instance's own files.

@var{path} is a path on this instance, e.g. @file{/css/fonts.css}.  With
@code{asset-url} configured the address is built on it; with it empty the
path is answered unchanged, which a browser reads against the host that
served the page and this instance therefore answers itself."
  (let ((base (config-ref 'asset-url)))
    (if (and (string? base)
             (not (string-null? base)))
        (string-append (if (string-suffix? "/" base)
                           (substring base 0
                                      (- (string-length base) 1)) base)
                       (if (string-prefix? "/" path) path
                           (string-append "/" path))) path)))

(define (shell-fonts)
  "The stylesheet that declares the faces the interface is set in.

It carries the Lucida faces and the two web families the theme names after
them, all of them files of this instance (see @code{maak fetch-assets})."
  `((link (@ (rel stylesheet)
             (href ,(asset-href "/css/fonts.css"))))))

(define (shell-icon-stylesheet)
  "The stylesheet that declares the icon face."
  `((link (@ (rel stylesheet)
             (href ,(asset-href "/css/phosphor.css"))))))

;; The house stylesheet: the polish every page of this instance wears -- the
;; thicker pane of glass, the light that follows the pointer, the window
;; chrome, the inks a selection and a keyboard focus are painted in.  See
;; `pub/css/lucidplan-luxe.css' for the whole of it.
;;
;; It is linked *after* the framework's own sheets rather than inlined beside
;; them, and that ordering is the one thing about it that matters: this sheet
;; restates a handful of declarations the framework set (the edge of a frosted
;; pane, the shadow of the bar), and a reader's browser settles such a
;; disagreement by load order.  A file is also the one thing a browser can keep
;; between two page loads, which an inlined sheet cannot.
(define (shell-house-style)
  "The stylesheet this application dresses itself in."
  `((link (@ (rel stylesheet)
             (href ,(asset-href "/css/lucidplan-luxe.css"))))))

;; A one-line script, in the head, before anything is painted.  It says the
;; page has scripting, and the house stylesheet reads that as permission to
;; hold a section back until it is scrolled into view (see `.lp-js' there).
;; Without it a reader whose script never ran would be shown sections that had
;; faded in to nothing; with it, the only page that animates is the page that
;; can.
(define (shell-polish-flag)
  "The early script that marks the document as scripted."
  `((script (@ (type "text/javascript"))
            "document.documentElement.classList.add('lp-js');")))

(define (shell-resources)
  "The stylesheets and scripts a page of this shell needs.

This is the framework's own recommended set with the two pieces that
reached another host replaced: the fonts and the icon face are this
instance's, and every other piece was already written into the page."
  (list (shell-fonts)
        (shell-icon-stylesheet)
        (fly-out-css)
        (hygguile-extras-css)
        `(noscript ,@(no-js-css))
        (toggle-dark-theme-js)
        (copy-pre-inner-text-js)
        (show-notifications-from-url-js)
        (carousel-button-js)
        (remember-dark-theme-js)
        (color-show-js)
        (fly-out-js)
        (code-sample-js)
        (tray-js)))

(define (analytics-script)
  "The page-view script, as a list of one script or of none.

The script is the instance's own file, so it is written into the page; where
it reports to and how often are configuration.  With no endpoint configured
there is nothing for it to report to, so it is left out entirely rather than
run against a default address somebody else runs."
  (let ((endpoint (config-ref 'analytics-endpoint)))
    (if (or (not (string? endpoint))
            (string-null? endpoint))
        '()
        `((script (@ (data-endpoint ,endpoint)
                     (data-interval ,(number->string (config-ref 'analytics-interval))))
                  ,(assoc-ref asset-library "pub/js/symbanta.js"))))))

(begin
  (define* (html-template G_
                          #:key (title #f)
                          (description #f)
                          (canonical-path "/")
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
                                        ;; The row the menu opens on is a
                                        ;; placeholder, so it says so with a
                                        ;; value of its own: an option
                                        ;; without one answers with its own
                                        ;; text, and choosing it would then
                                        ;; navigate to `/🔗 go to'.
                                        (option (@ (value "")) "🔗 go to")
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
           (nav (glass-panel `(

                               ;; Left Section: Brand/Logo
                               (div (@ (class ,(mk-class '(flex items-center
                                                           justify-center
                                                           md:justify-start
                                                           w-full md:w-auto))))
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
                                                  `(span ,(config-ref 'site-name)))
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
                                                           md:flex-nowrap
                                                           md:justify-end
                                                           gap-3
                                                           md:gap-4
                                                           w-full
                                                           md:w-auto
                                                           lp-dock))))

                                    ;; The way through the application.  Each
                                    ;; of these is a word with a mark beside
                                    ;; it and no box around either: five plates
                                    ;; in a row are five things asking to be
                                    ;; pressed at once, which is a control
                                    ;; panel rather than a way through.  See
                                    ;; `lp-quiet' in
                                    ;; `pub/css/lucidplan-luxe.css'.
                                    ,(if (not is-logged-in)
                                         (bar-link #:icon 'sign-in
                                                   #:label (G_ "login")
                                                   #:href "/login")
                                         (bar-link #:icon 'user-circle
                                                   #:label (G_ "me")
                                                   #:href "/me"))

                                    ,(bar-link #:icon 'align-bottom
                                               #:label (G_ "projects")
                                               #:href "/projects")

                                    ;; Planning Poker sits with the other
                                    ;; links rather than behind the signed-in
                                    ;; branch, because a table is exactly
                                    ;; where the people without an account on
                                    ;; this instance are.
                                    ,(bar-link #:icon 'cards
                                               #:label (G_ "poker")
                                               #:href "/poker")

                                    ,(bar-link #:icon 'info
                                               #:label (G_ "about")
                                               #:href "/about")

                                    ;; Theme Toggle
                                    ,(ui-mode-button #:icon ""
                                                     #:size '(text-md)
                                                     #:icon-css '(text-stone-300
                                                                  dark:text-stone-300
                                                                  text-md)
                                                     #:family '(font-sans)
                                                     #:background '()
                                                     #:hover-background '()
                                                     #:border-size '()
                                                     #:border-color '()
                                                     #:shadow '()
                                                     #:content `((div ,(icon
                                                                             #:type 'sun
                                                                             #:icon-css '
                                                                             (text-md
                                                                              lg:text-lg
                                                                              text-stone-300
                                                                              dark:text-stone-300))
                                                                      ,(icon
                                                                             #:type 'moon
                                                                             #:icon-css '
                                                                             (text-md
                                                                              lg:text-lg
                                                                              text-stone-300
                                                                              dark:text-stone-300))))
                                                     #:color '(text-stone-300
                                                               dark:text-stone-300)
                                                     #:weight '(font-light)
                                                     #:extra-classes '(lp-quiet
                                                                       py-1
                                                                       px-2
                                                                       lg:px-3
                                                                       max-w-prose
                                                                       cursor-pointer))
                                    ;; Dropdown Select
                                    ,project-select))
                             #:tag 'header
                             #:position '(sticky top-0 z-48)
                             #:rounding '()
                             #:border '(border-b-2 border-stone-500/25
                                                   dark:border-onyx-100/25)
                             #:tint '(bg-jeans-blue-500/75
                                      dark:bg-jeans-blue-900/75)
                             #:shadow '(shadow-lg)
                             #:padding '(px-4 py-3 md:py-2)
                             ;; The bar lays its two ends out itself: stacked on a narrow
                             ;; screen, where there is no room for both, and a single row
                             ;; from `md' up, where there is.  Without this the two ends
                             ;; are block-level siblings and the bar is two rows tall on
                             ;; every screen, which wastes the width it is given.
                             ;; `lp-toolbar' is the hook the house script uses to deepen
                             ;; the shadow once the page has moved under the bar, and
                             ;; `lp-dock' the one that makes its controls magnify around
                             ;; the pointer the way a dock does.
                             #:classes '(w-full flex
                                                flex-col
                                                md:flex-row
                                                md:flex-nowrap
                                                md:items-center
                                                md:justify-between
                                                md:gap-4
                                                lp-toolbar))))

      ;; Output Page
      (render-page `(,doctype-html (html (@ (lang ,(config-ref 'site-language)))
                                         (head (title ,(page-title title))
                                               ,(hygguile-recommended-meta)
                                               ,@(seo-meta #:title title
                                                           #:description
                                                           description
                                                           #:canonical-path
                                                           canonical-path)
                                               ;; The theme: every stylesheet and script the
                                               ;; shell needs, each one this instance's own.
                                               ,@(shell-resources)
                                               ;; Whether this page may animate at
                                               ;; all: said before the body is read.
                                               ,@(shell-polish-flag)

                                               (link (@ (rel "shortcut icon")
                                                        (href ,(asset-href
                                                                "/favicon.ico"))
                                                        (type "image/x-icon"))
                                                     "")
                                               (link (@ (rel icon)
                                                        (href ,(asset-href
                                                                "/favicon.ico"))
                                                        (type "image/x-icon"))
                                                     "")
                                               (link (@ (rel stylesheet)
                                                        (href ,(asset-href
                                                                "/css/lucidplan.olive.min.css")))
                                                     "")
                                               ;; The house style, after the framework's
                                               ;; own sheets: it is the last word on the
                                               ;; handful of declarations it restates.
                                               ,@(shell-house-style)

                                               (script (@ (type
                                                           speculationrules))
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
                                               ;; The page-view script, when the instance
                                               ;; has somewhere of its own to report to.
                                               ,@(analytics-script)
                                               (script ,(assoc-ref
                                                         asset-library
                                                         "pub/js/tickets.js"))
                                               (script ,(assoc-ref
                                                         asset-library
                                                         "pub/js/password-strength.js"))
                                               ;; The page's polish: the light that follows
                                               ;; the pointer, the lean of a card under it,
                                               ;; the reveal of a section, the shadow the bar
                                               ;; takes once the page moves under it.
                                               (script ,(assoc-ref
                                                         asset-library
                                                         "pub/js/lucidplan-luxe.js")))
                                         (body (@ (class ,(mk-class '(min-h-screen
                                                                      bg-stone-100
                                                                      dark:bg-onyx-900
                                                                      font-sans
                                                                      relative
                                                                      overflow-x-hidden))))
                                               ;; The colour the panes above frost.
                                               ,(aurora-backdrop)
                                               ,notification-tray
                                               ,(with-footer #:main `((div (@ (class ,
                                                                               (mk-class '
                                                                                (flex
                                                                                 flex-col
                                                                                 justify-between
                                                                                 min-h-96
                                                                                 relative
                                                                                 z-10))))
                                                                           (div
                                                                            (@
                                                                             (class ,
                                                                              (mk-class '
                                                                               (flex
                                                                                flex-col
                                                                                gap-8
                                                                                justify-start))))
                                                                            ,@nav
                                                                            ,body)))
                                                             #:footer (page-footer
                                                                       G_))))))))
  (export html-template
          frosted-tint-classes
          recessed-tint-classes
          chip-tint-classes
          window-controls
          window-titlebar
          bar-link
          quiet-link
          quiet-submit
          render-page
          asset-href
          emoji-for-ticket-status
          emoji-for-ticket-type
          glyph-for-ticket-status
          glyph-for-ticket-type
          status-label
          status-icon-node
          bucket-heading
          bucket-icon-node
          lucidplan-locales
          locale-choose-form))

;;; ===== How a status is drawn, from the status itself =====
;;;
;;; What marks a status on a page, what it is called and how a closed bucket
;;; is titled are all read from the status ladder the page was built with --
;;; a project's own list or the instance's -- rather than from tables kept
;;; here beside them, so a status an instance adds, renames or removes is
;;; drawn by the same one lookup the rest of the application uses.
;;;
;;; The shipped labels are written as calls of the reader's translator rather
;;; than looked up in a table of plain strings, so that the extractor finds
;;; every one of them: a label that only ever reaches the page as the
;;; argument of a lookup is invisible to `xgettext', and the ladder would
;;; then stay in English inside an otherwise translated page.  A label an
;;; operator wrote is shown exactly as written, since nothing can translate
;;; it.
(define (translate-status-label G_ label)
  (cond
    ((equal? label "To-do")
     (G_ "To-do"))
    ((equal? label "Work in Progress")
     (G_ "Work in Progress"))
    ((equal? label "In Review")
     (G_ "In Review"))
    ((equal? label "Done")
     (G_ "Done"))
    ((equal? label "Rejected")
     (G_ "Rejected"))
    (else label)))

;; The ladder a planning table deals, said in the reader's language.  It is
;; written here beside the status labels because both are the same problem: a
;; page that names a ladder and a page that names a state are both naming
;; something the reader was never shown in English, and neither label may be
;; built somewhere only one of those pages can reach.
(define-public (deck-label G_ deck)
  "The words a reader is shown for the ladder DECK deals."
  (cond
    ((equal? deck "sizes")
     (G_ "t-shirt sizes"))
    ((equal? deck "points")
     (G_ "story points"))
    (else deck)))

(define-public (status-label G_ specs status)
  "The label a reader is shown for STATUS, resolved through SPECS and
translated when it is one of the shipped labels."
  (translate-status-label G_
                          (ticket-status-label specs status)))

(define-public (status-icon-node specs status)
  "The mark STATUS wears on a card or a heading, as an SXML node."
  (list (icon #:type (string->symbol (ticket-status-icon specs status))
              #:icon-css '(text-onyx-300 dark:text-stone-300 text-xl))))

;;; ===== How a deadline is read =====
;;;
;;; A deadline is useful at a glance or not at all: what a reader has to act
;;; on is not the day but how the day stands -- past, today, close enough to
;;; do something about, or comfortably ahead.  The question is put to the
;;; domain (`ticket-due-state'), so a ticket's badge and a milestone's cannot
;;; disagree about what "soon" means, and this module only decides how each
;;; answer is drawn.
;;;
;;; The badge is a colour and a border rather than a filled pill, because it
;;; sits next to the status and the type on a card that is already wearing a
;;; badge for each: three filled pills in a row read as three equal facts, and
;;; only one of them is a warning.
(define-public (due-badge-face state)
  "The classes a deadline in STATE is drawn with."
  (mk-class (append '(inline-flex flex-row gap-1 items-center
                                  text-xs px-2 py-1 rounded-lg border-2
                                  font-medium)
                    (case state
                      ((overdue due-today)
                       '(text-redwood-700 dark:text-redwood-300
                         border-redwood-500/75 dark:border-redwood-500/75))
                      ((due-soon)
                       '(text-olive-700 dark:text-olive-300
                         border-olive-500/75 dark:border-olive-500/75))
                      (else
                       '(text-onyx-500 dark:text-stone-500
                         border-stone-300/75 dark:border-onyx-100/75))))))

(define-public (render-due-badge G_ due-on . today-arg)
  "Answer the badge a deadline DUE-ON is read as, or nothing to draw.

TODAY-ARG is the day to judge by and defaults to the day it is.  A ticket
with no deadline, or with one that is not a day, answers the empty list
rather than a badge saying nothing: the page is laid out as though the
deadline were not there, which is what it is."
  (let ((state (ticket-due-state due-on
                                 (if (pair? today-arg)
                                     (car today-arg)
                                     (today-iso)))))
    (if state
        `((span (@ (class ,(due-badge-face state))
                    (title ,(format #f "~a" due-on)))
                ,(icon #:type 'calendar-blank
                       #:icon-css '(text-xs))
                ,(G_ (due-state-label state))
                "· "
                ,due-on))
        '())))

;;; ===== A day, typed into a form =====
;;;
;;; Three forms ask for a calendar day -- the day time was spent, the day a
;;; ticket or a milestone is due, the two ends of a sprint -- and all of them
;;; ask for it the same way: the browser's own date control, which draws a
;;; calendar in the reader's own locale and submits the one form a server
;;; should have to parse, `2026-10-27'.  A page therefore needs no date-picker
;;; script and no format of its own.
;;;
;;; It is built on hygguile's `input' rather than on a date component of its
;;; own, because that is what the version this project pins offers; the field
;;; is an `<input type="date">' either way, and what a page says about it --
;;; its label, its day -- is the same.

(define-public (date-input label name . value-arg)
  "Answer a date field named NAME, labelled LABEL, showing VALUE-ARG.

VALUE-ARG is the day the field opens on, in ISO form, and is optional: a
form that is creating something rather than editing it has no day to show,
and the field is then empty rather than showing a stand-in.  A value that is
not text -- which is how an absent day arrives, as @code{#f} -- is emptied
rather than written into the markup as it is."
  (let ((value (if (pair? value-arg) (car value-arg) #f)))
    (input #:type 'date
           #:label label
           #:name name
           #:value (if (string? value) value ""))))

;; A closed bucket is titled by the status inside it when it holds exactly
;; one -- so renaming that status renames the section -- and by the bucket's
;; own name when it holds several, since there is then no one name to borrow.
(define-public (bucket-heading G_ specs bucket)
  (let ((values (ticket-status-values-in-bucket specs bucket)))
    (if (and (pair? values)
             (null? (cdr values)))
        (status-label G_ specs
                      (car values))
        (G_ bucket))))

(define-public (bucket-icon-node specs bucket)
  (let ((values (ticket-status-values-in-bucket specs bucket)))
    (if (and (pair? values)
             (null? (cdr values)))
        (status-icon-node specs
                          (car values))
        (list (icon #:type (if (equal? bucket "rejected")
                               'x
                               'check)
                    #:icon-css '(text-onyx-300 dark:text-stone-300 text-xl))))))

;; The two the landing page and the ticket badges answer without a project in
;; hand: they are drawn from the instance's own ladder.
(define-public (emoji-for-ticket-status ticket-status)
  (status-icon-node (default-ticket-statuses) ticket-status))

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

;;; ===== The same two ladders, as the text a dropdown row can hold =====

;;; A native dropdown is drawn by the browser or the operating system, and
;;; it lays out the text of a row: an element nested inside an `<option>' is
;;; not rendered, and one handed to hygguile's `glyph-option' is printed into
;;; the row as the list it is.  The glyph that introduces a row is therefore
;;; a character and not one of the icon components above.  Each one stands
;;; for the same thing its Phosphor icon does, so a status reads the same
;;; whichever control is showing it.
(define-public (glyph-for-ticket-status ticket-status)
  (ticket-status-glyph (default-ticket-statuses) ticket-status))

(define-public (glyph-for-ticket-type ticket-type)
  (cond
    ((equal? "task" ticket-type)
     "🔧")
    ((equal? "bug" ticket-type)
     "🐛")
    ((equal? "story" ticket-type)
     "📖")
    ((equal? "spike" ticket-type)
     "🧪")
    ((equal? "time-box" ticket-type)
     "⏳")
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

;;; The handle a ticket is dragged by.  It is the drag source and the row is
;;; not, which is the whole point: an element marked `draggable' cannot be
;;; read with the mouse -- a drag that starts anywhere inside it moves the
;;; ticket instead of selecting the sentence the reader meant to copy -- so
;;; only this grip carries the attribute, and the script resolves the row to
;;; move from the grip that was grabbed.  The backlog row and the kanban card
;;; are both dragged by this same grip, so both start with the one handler.
;;; It is a value rather than a procedure because that is how the views use
;;; it: `,grab-indicator' splices what the name holds, and a procedure there
;;; would splice the procedure itself -- a grip that silently disappears.
(define-public grab-indicator
  `((div (@ (class ,(mk-class '(text-onyx-100 dark:text-stone-300 cursor-grab
                                              active:cursor-grabbing flex
                                              items-center)))
            (draggable "true")
            (data-drag-handle "true")
            (ondragstart "dragTicketStart(event)")
            (ondragend "dragTicketEnd(event)"))
         (svg (@ (class "w-4 h-4")
                 (fill "currentColor")
                 (viewBox "0 0 16 16"))
              (path (@ (d
                        "M7 2a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zM7 6a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm-3 4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm-3 4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm3 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0z")))))))

;;; ===== The language a reader reads the interface in =====

;; The languages this instance answers in: the name of the catalog under
;; `sys/i18n/locale', and the language as it is written in that language.
;; The list is the page's -- it is what a reader is offered -- so a catalog
;; added under that directory is offered here, and one removed stops being
;; offered.  See `sys/i18n/locale' and `maak i18n-compile'.
(define-public (lucidplan-locales)
  "Answer the languages this instance offers its readers, as a list of
@code{(catalog-name . the-language-as-it-names-itself)} pairs.

The list is configuration: a deployment sets @code{locales} in its env module
and so changes what the language control offers, or leaves the languages that
ship with LucidPlan.  Every catalog named here must exist under
@code{sys/i18n/locale}; see @code{maak i18n-compile}."
  (config-ref 'locales))

(define* (locale-choose-form G_
                             #:key selected-locale return-to)
  "The control a reader changes the interface's language with.

The choice is posted to @code{/preferences/locale}, which writes it to the
@code{lang} cookie -- the cookie every route builds its translator from --
and sends the reader back to the page they were reading.  The language a
page is *being* read in is the server's to state, since it is the language
that page was rendered in, so the row the menu shows is the one the caller
passes rather than one this control guesses.

The control wears the page's own glass and names itself: a translate glyph
and the word @code{language} stand beside the menu rather than hiding
behind it, because a reader who cannot read the page around them cannot be
asked to recognise a control that shows nothing but the language it is
already set to.  The name is bound to the menu with @code{for}, so it
reaches assistive technology as well."
  `((form (@ (action "/preferences/locale")
             (method "POST")
             (enctype "application/x-www-form-urlencoded")
             (class ,(mk-class (append '(flex flex-row
                                              flex-wrap
                                              items-center
                                              gap-1
                                              pl-2
                                              pr-1
                                              py-1
                                              w-fit
                                              rounded-full
                                              border-2
                                              border-jeans-blue-500/50
                                              dark:border-jeans-blue-300/25
                                              shadow-md) frosted-tint-classes))))
          ,@(if return-to
                `((input (@ (type "hidden")
                            (name "return-to")
                            (value ,return-to))))
                '())
          ,(icon #:type 'translate
                 #:icon-css '(text-lg text-jeans-blue-700
                                      dark:text-jeans-blue-300))
          (label (@ (class ,(mk-class '(text-md font-medium font-sans px-1
                                                text-onyx-700
                                                dark:text-stone-300)))
                    (for user-lang))
                 ,(G_ "language"))
          ,(select #:name 'user-lang
                   ;; No `lowercase' here: a language writes its own name,
                   ;; and `Nederlands' or `Español' is how it is spelled.
                   #:select-classes '(text-md font-medium
                                              font-sans
                                              text-onyx-900
                                              dark:text-stone-100
                                              bg-transparent
                                              rounded-lg
                                              border-0
                                              px-2
                                              py-1
                                              outline-none
                                              cursor-pointer
                                              hover:bg-stone-300/75
                                              dark:hover:bg-onyx-700/75
                                              focus:ring-2)
                   #:select-flex-css '(flex flex-row items-center my-0 w-fit)
                   #:options (map (lambda (locale)
                                    (option #:value (car locale)
                                            #:content (cdr locale)
                                            #:selected-value selected-locale))
                                  (lucidplan-locales)))
          ,(icon-button #:icon 'check
                        #:content ""
                        #:type 'submit
                        #:extra-classes '(py-1 px-2 max-w-prose cursor-pointer
                                               text-onyx-700
                                               dark:text-stone-300)
                        #:icon-css '(text-lg text-onyx-700 dark:text-stone-300)))))
