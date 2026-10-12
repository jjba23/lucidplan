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

(define-module (app views index)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (welcome-page))

(define* (welcome-page G_
                       #:key is-logged-in projects selected-locale)
  (let* ((project-count (length (if (nil? projects) '() projects)))
         (hero
          ;; The welcome is a pane of glass with three colour washes drifting
          ;; behind it -- a blue, a warm one and a gold one -- so the frost
          ;; has something of the page's own to catch rather than reading as
          ;; a white card laid on a white page.  The washes sit deep under a
          ;; near-white tint and a heavy blur, which is what keeps them a
          ;; tint in the glass instead of a shape on it.
          (glass-panel
           `(;; What the reader reads the page in comes first, and it is the
             ;; one control here that names itself: a reader who cannot read
             ;; the copy has to be able to find it without reading the copy.
             (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                               items-center
                                               justify-start
                                               gap-2))))
                  ,(locale-choose-form G_
                                       #:selected-locale (if
                                                          selected-locale
                                                          selected-locale
                                                          "en_US")))
             ,(sub-h2 `(div (@ (class ,(mk-class '(flex flex-row gap-2))))
                            (span ,(G_ "Welcome to LucidPlan"))
                            (span "✨"))
                      #:weight '(font-light)
                      #:size '(text-xl md:text-2xl))
             ,(em (G_ "free and open project management for everyone")
                  #:size '(text-sm))
             ;; What the page says on the left, what the instance holds on
             ;; the right: side by side once there is room for both, so the
             ;; card uses the width it is given rather than running long and
             ;; narrow down the page.
             (div (@ (class ,(mk-class '(flex flex-col
                                               md:flex-row
                                               md:items-center
                                               md:justify-between
                                               md:gap-10))))
                  ;; The copy keeps a reading measure of its own -- 84
                  ;; characters -- so the figures sit beside a column of text
                  ;; rather than at the far side of a wide emptiness.
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
           ;; Near-white glass: the tint the framework's own panes wear,
           ;; lifted off its milkiness just enough for the washes to reach
           ;; the reader as a colour in the glass rather than a colour of
           ;; their own.
           #:tint '(bg-stone-100/75 dark:bg-onyx-900/75)
           #:frost 3
           ;; Blue, warm and gold, one to a corner and one across the middle,
           ;; each drifting at its own pace and out of step with the others.
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
           #:content-classes '(flex flex-col gap-6))))
    (html-template G_
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(px-3
                                                       md:px-6
                                                       py-6
                                                       max-w-256))))
                                ,hero))))
