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

;;; The front page is the way in.
;;;
;;; A reader who has never heard of the place lands here, and the question the
;;; first screen has to answer is what LucidPlan is and what they can do with
;;; it -- not what somebody else's tickets happen to be.  So the page opens on
;;; a pane that says what the place is, offers the two things a visitor came to
;;; do (browse the projects, start one), and shows how much this instance
;;; holds; then it says what the software does, in a grid a reader can take in
;;; at a glance; then it shows the workflow itself, the columns a ticket moves
;;; through; and it closes with the one invitation the page is for.
;;;
;;; Nothing here is a lie about a particular instance: the count is the
;;; instance's own rows, and every other figure is the software's, so a fresh
;;; checkout and a busy deployment both read as the same page.

(define-module (app views index)
  #:use-module (lucidplan html)
  #:use-module (lucidplan config)
  #:use-module (lucidplan domain)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (welcome-page))

;;; ===== What the page is built out of =====

;; The pane every panel on this page is cut from: the framework's frosted tint
;; plus a rounded, hairline-bordered card.  It is the same recipe the about
;; page and the board's own columns wear, so the front page reads as the
;; application rather than as a poster for it.
(define landing-card-classes
  (append frosted-tint-classes
          '(rounded-2xl
            shadow-md
            border-2
            border-stone-500/25
            dark:border-onyx-100/25)))

;; One field of one configured feature, or FALLBACK: the front page's copy is
;; configuration (see `(lucidplan config)'), and a hand-written entry is not a
;; promise about its own shape, so every field is read as text and a feature
;; with no icon of its own still draws one.
(define (landing-feature-field feature n fallback)
  (let ((value (and (list? feature)
                    (> (length feature) n)
                    (list-ref feature n))))
    (if (string? value) value fallback)))

(define* (welcome-page G_
                       #:key is-logged-in projects selected-locale)
  (let* ((project-count (length (if (nil? projects) '() projects))))
    (html-template G_
                   ;; The front page is titled with the instance's own name
                   ;; and described with its own description: it is the page
                   ;; a search engine and a link preview meet first.
                   #:title (config-ref 'site-name)
                   #:description (config-ref 'site-description)
                   #:canonical-path "/"
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(w-full
                                                       max-w-256
                                                       mx-auto
                                                       flex
                                                       flex-col
                                                       gap-6
                                                       px-3
                                                       md:px-6
                                                       py-6))))
                                ,(landing-hero G_ project-count selected-locale)
                                ,(landing-features G_)
                                ,(landing-workflow G_)
                                ,(landing-closing G_)))))

;;; ====== The first screen ======

;; What the place is called and what it is for, on a pane of glass with three
;; colour washes drifting behind it -- a blue, a warm one and a gold one -- so
;; the frost has something of the page's own to catch rather than reading as a
;; white card laid on a white page.  The washes sit deep under a near-white
;; tint and a heavy blur, which is what keeps them a tint in the glass instead
;; of a shape on it.
;;
;; The name is the one heading on the page a reader is meant to read first, so
;; it is the page's own `h1', set at display size: a visitor who reads nothing
;; else has read what this is.  Under it the copy keeps a reading measure of
;; its own, so the figures sit beside a column of text rather than at the far
;; side of a wide emptiness.
(define (landing-hero G_ project-count selected-locale)
  (glass-panel
   `(;; What the reader reads the page in comes first, and it is the one
     ;; control here that names itself: a reader who cannot read the copy has
     ;; to be able to find it without reading the copy.
     (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                       items-center
                                       justify-start
                                       gap-2))))
          ,(locale-choose-form G_
                               #:selected-locale (if selected-locale
                                                     selected-locale
                                                     "en_US")))
     ;; The sparkle is a mark rather than a word, and it is a child of the
     ;; heading rather than a part of its text, so the gradient ink the
     ;; heading is painted with reaches it the same way it reaches the name.
     ,(h1 (list (G_ "Welcome to LucidPlan") " ✨")
          #:color '(hg-gradient-text)
          #:size '(text-3xl md:text-5xl)
          #:weight '(font-black))
     ,(em (G_ "free and open project management for everyone")
          #:size '(text-sm))
     ;; What the page says on the left, what the instance holds on the right:
     ;; side by side once there is room for both, so the pane uses the width it
     ;; is given rather than running long and narrow down the page.
     (div (@ (class ,(mk-class '(flex flex-col
                                       md:flex-row
                                       md:items-center
                                       md:justify-between
                                       md:gap-10))))
          (div (@ (class ,(mk-class '(flex flex-col
                                            grow
                                            min-w-0
                                            max-w-prose
                                            gap-3))))
               ,(p (G_
                    "Take control of your projects with LucidPlan, the open and flexible project management solution built for teams of every shape and size.")
                   #:weight '(font-light)
                   #:size '(text-sm md:text-md))
               ,(p (G_
                    "We believe powerful tools shouldn't be complicated or restricted, so LucidPlan keeps your workflow simple and approachable.")
                   #:weight '(font-light)
                   #:size '(text-sm md:text-md))
               (div (@ (class ,(mk-class '(flex flex-row
                                                 flex-wrap
                                                 gap-3
                                                 my-3
                                                 items-center))))
                    (a (@ (href "/projects"))
                       ,(icon-button #:icon 'align-bottom
                                     #:content (G_ "browse projects")))
                    (a (@ (href "/projects/new"))
                       ,(icon-button #:icon 'plus
                                     #:content (G_ "start a project")))))
          (div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            gap-8
                                            md:flex-col
                                            md:gap-6
                                            shrink-0
                                            my-2))))
               ,(stat-card #:value (format #f "~a" project-count)
                           #:label (G_ "projects"))
               ,(stat-card #:value "5"
                           #:label (G_ "workflow columns")))))
   ;; Near-white glass: the tint the framework's own panes wear, lifted off its
   ;; milkiness just enough for the washes to reach the reader as a colour in
   ;; the glass rather than a colour of their own.
   #:tint '(bg-stone-100/75 dark:bg-onyx-900/75)
   #:frost 3
   ;; Blue, warm and gold, one to a corner and one across the middle, each
   ;; drifting at its own pace and out of step with the others.
   #:washes (list (glass-wash '(bg-jeans-blue-500/50
                                w-96 h-96
                                top-0 left-4)
                              #:delay -6
                              #:pace 44
                              #:distance '(3 -2))
                  (glass-wash '(bg-olive-500/50
                                w-96 h-96
                                top-1/2 left-1/3)
                              #:delay -18
                              #:pace 56
                              #:distance '(-2 2))
                  (glass-wash '(bg-redwood-500/50
                                w-96 h-96
                                bottom-0 right-4)
                              #:delay -30
                              #:pace 38
                              #:distance '(-3 -2)))
   #:drift? #t
   #:content-classes '(flex flex-col gap-6)))

;;; ====== What the software does ======

;; The promises, on the page a visitor lands on rather than only on the page
;; they would have to go looking for: what a stranger wants after "what is
;; this" is "what does it do", and the answer is short enough to be glanced at.
;; One feature to a card, each with the glyph for what it says, so the six can
;; be taken in at a glance instead of read as six sentences in a row.  The
;; heading, the line under it and the features themselves are configuration
;; (see `(lucidplan config)'), so an instance says what it does in its own
;; words without this view being edited.
(define (landing-features G_)
  `(section (@ (class ,(mk-class '(flex flex-col gap-4))))
            ,(h2 (G_ (config-ref 'landing-features-title))
                 #:weight '(font-bold)
                 #:size '(text-2xl md:text-3xl))
            ,(p (G_ (config-ref 'landing-features-intro))
                #:weight '(font-light)
                #:size '(text-md))
            (ul (@ (class ,(mk-class '(list-none
                                       p-0
                                       grid
                                       grid-cols-1
                                       sm:grid-cols-2
                                       lg:grid-cols-3
                                       gap-4))))
                ,@(map (lambda (feature)
                         `(li (@ (class ,(mk-class (append landing-card-classes
                                                          '(p-5
                                                            flex
                                                            flex-col
                                                            gap-3)))))
                              ,(icon #:type (string->symbol
                                             (landing-feature-field
                                              feature 0 "circle"))
                                     #:icon-css '(text-3xl
                                                  text-jeans-blue-700
                                                  dark:text-jeans-blue-300))
                              ,(h4 (G_ (landing-feature-field feature 1 "")))
                              ,(p (G_ (landing-feature-field feature 2 ""))
                                  #:weight '(font-light)
                                  #:size '(text-sm))))
                       (config-ref 'landing-features)))))

;;; ====== A board, drawn from the board ======

;; The one picture the page shows of itself, and it is not a screenshot: the
;; columns are the open statuses the instance configured, in the order the
;; board shows them and under the names it gives them, drawn from the same
;; helpers that draw the board's own column headings.  The bars under each
;; heading are decoration -- a suggestion of the cards that would be there --
;; so they are hidden from assistive technology and the headings carry the
;; meaning.
(define (landing-workflow G_)
  (let ((specs (default-ticket-statuses)))
    `(section (@ (class ,(mk-class '(flex flex-col gap-4))))
              ,(h2 (G_ (config-ref 'landing-workflow-title))
                   #:weight '(font-bold)
                   #:size '(text-2xl md:text-3xl))
              ,(p (G_ (config-ref 'landing-workflow-intro))
                  #:weight '(font-light)
                  #:size '(text-md))
              (div (@ (class ,(mk-class '(flex flex-row
                                                flex-wrap
                                                items-stretch
                                                gap-3))))
                   ,@(map (lambda (status)
                            (landing-workflow-column G_ specs status))
                          (ticket-status-values-in-bucket specs "open"))))))

;; One column of that board: its heading carries the status's own mark and the
;; name the board gives it, and the bars below suggest the cards it would hold.
(define (landing-workflow-column G_ specs status)
  `(div (@ (class ,(mk-class (append landing-card-classes
                                     '(flex-1
                                       min-w-32
                                       min-h-24
                                       p-3
                                       flex
                                       flex-col
                                       gap-2)))))
        (div (@ (class ,(mk-class '(flex flex-row gap-2 items-center))))
             ,(status-icon-node specs status)
             (span (@ (class ,(mk-class '(font-medium text-sm))))
                   ,(status-label G_ specs status)))
        ,(landing-board-bar 100)
        ,(landing-board-bar 72)
        ,(landing-board-bar 45)))

(define (landing-board-bar percent)
  "One decorative card-shaped bar of WIDTH percent, hidden from assistive tech."
  `(div (@ (class ,(mk-class '(h-2
                               rounded-md
                               bg-stone-500/25
                               dark:bg-onyx-100/25)))
           (style ,(format #f "width: ~a%" percent))
           (aria-hidden "true"))))

;;; ====== The way in ======

;; The invitation the page has been building up to, on its own pane so it reads
;; as the page's last word rather than as one more section: the two doors a
;; visitor has, offered once more where they land after reading everything
;; above.  The text under them is the software's licence in one line, which is
;; the promise the rest of the page has been making.
(define (landing-closing G_)
  (glass-panel
   `((div (@ (class ,(mk-class '(flex flex-col
                                       items-center
                                       gap-3
                                       text-center))))
          ,(h2 (G_ "ready to plan something?")
               #:weight '(font-bold)
               #:size '(text-2xl md:text-3xl))
          ,(p (G_
               "Start a project in seconds, or read more about what LucidPlan is and how this instance is run.")
              #:weight '(font-light)
              #:size '(text-md))
          (div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            gap-3
                                            items-center
                                            justify-center
                                            pt-1))))
               (a (@ (href "/projects"))
                  ,(icon-button #:icon 'align-bottom
                                #:content (G_ "browse projects")))
               (a (@ (href "/projects/new"))
                  ,(icon-button #:icon 'plus
                                #:background '(bg-asparagus-500/90
                                               dark:bg-asparagus-700/90)
                                #:content (G_ "start a project")))
               (a (@ (href "/about"))
                  ,(button (G_ "about") #:weight '(font-light))))))
   #:frost 3
   #:padding '(p-6 sm:p-10)
   #:classes '(flex flex-col gap-4 w-full)))
