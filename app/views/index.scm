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
                       #:key is-logged-in projects)
  (let* ((project-count (length (if (nil? projects) '() projects)))
         (hero
          ;; The welcome is a pane of glass with a slow, out-of-phase colour
          ;; wash drifting behind it -- hygguile's `glass-panel', the same
          ;; component every frosted surface in the framework is built from.
          (glass-panel
           `((div (@ (class ,(mk-class '(flex flex-col gap-4))))
                  ,(sub-h2 `(div (@ (class ,(mk-class '(flex flex-row gap-2))))
                                 (span ,(G_ "Welcome to LucidPlan"))
                                 (span "✨"))
                           #:weight '(font-light))
                  ,(em (G_ "free and open project management for everyone")
                       #:size '(text-md))
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
                                        #:content (G_ "start a project"))))
                  (div (@ (class ,(mk-class '(flex flex-row
                                                    flex-wrap
                                                    gap-8
                                                    items-start
                                                    my-2))))
                       ,(stat-card #:value (format #f "~a" project-count)
                                   #:label (G_ "projects"))
                       ,(stat-card #:value "5"
                                   #:label (G_ "workflow columns")))))
           #:washes (list (glass-wash '(w-96 h-96 left-4 top-4
                                               bg-jeans-blue-500/50)
                                      #:delay -6)
                          (glass-wash '(w-96 h-96 right-4 bottom-4
                                               bg-asparagus-500/50)
                                      ;; out of phase, and a slower, wider drift
                                      #:delay -22 #:pace 44 #:distance '(3 -2)))
           #:drift? #t
           #:frost 3
           #:content-classes '(flex flex-col gap-4))))
    (html-template G_
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(px-3
                                                       md:px-6
                                                       py-6
                                                       max-w-128))))
                                ,hero))))
