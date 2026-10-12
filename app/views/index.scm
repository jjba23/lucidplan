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
          ;; The welcome is a pane of white glass -- hygguile's
          ;; `glass-panel', the same component every frosted surface in the
          ;; framework is built from -- and it carries no colour wash of its
          ;; own, so the only colour it frosts is the page behind it.
          (glass-panel
           `(,(sub-h2 `(div (@ (class ,(mk-class '(flex flex-row gap-2))))
                            (span ,(G_ "Welcome to LucidPlan"))
                            (span "✨"))
                      #:weight '(font-light))
             ,(em (G_ "free and open project management for everyone")
                  #:size '(text-md))
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
                                                    gap-4))))
                       ,(p (G_
                            "Take control of your projects with LucidPlan, the open and flexible project management solution built for teams of every shape and size.")
                           #:weight '(font-light))
                       ,(p (G_
                            "We believe powerful tools shouldn't be complicated or restricted, so LucidPlan keeps your workflow simple and approachable.")
                           #:weight '(font-light))
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
                                   #:label (G_ "workflow columns"))
                       ;; The language the rest of the page was rendered in.
                       ;; It is here, on the page a reader arrives at and
                       ;; can come back to, because the choice is written to
                       ;; a cookie the whole application reads: it changes
                       ;; every page from here on, not just this one.
                       ,(locale-choose-form G_
                                            #:selected-locale (if
                                                               selected-locale
                                                               selected-locale
                                                               "en_US")))))
           ;; White glass: the tint is the near-white the framework's own
           ;; panes wear, at an opacity that reads as milk rather than as a
           ;; veil, and it is the pane's only colour.
           #:tint '(bg-stone-100/90 dark:bg-onyx-900/90)
           #:frost 3
           #:content-classes '(flex flex-col gap-6))))
    (html-template G_
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(px-3
                                                       md:px-6
                                                       py-6
                                                       max-w-256))))
                                ,hero))))
