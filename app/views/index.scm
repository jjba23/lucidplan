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
  #:use-module (ice-9 string-fun))

(begin
  (define* (welcome-page G_
                         #:key is-logged-in projects)
    (let ((project-logo `(((div (@ (class ,(mk-class '(bg-stone-200
                                                       dark:bg-stone-900
                                                       rounded-2xl p-4
                                                       shadow-md))))
                                (img (@ (loading lazy)
                                        (class ,(mk-class '(w-64 h-auto
                                                                 rounded-2xl)))
                                        (src
                                         "https://jointhefreeworld.org/static-assets/lucidplan/img/logo.jpg"))))))))
      (html-template G_
                     #:proj projects
                     #:is-logged-in #f
                     #:body `(div (@ (class ,(mk-class '(container mx-auto))))
                                  ,(h2 (G_ "welcome to lucidplan"))
                                  ,(em (G_
                                        "free and open project management for everyone"))
                                  ((div (@ (class ,(mk-class '(flex flex-row
                                                                    flex-wrap
                                                                    gap-8 my-8))))
                                        (div (@ (class ,(mk-class '(max-w-prose
                                                                    bg-stone-200
                                                                    dark:bg-stone-900
                                                                    rounded-2xl
                                                                    p-4
                                                                    shadow-md
                                                                    flex
                                                                    flex-col
                                                                    gap-3))))
                                             ,(p (G_
                                                  "Take control of your projects with LucidPlan, the open and flexible project management solution built for teams of every shape and size."))
                                             ,(p (G_
                                                  "We believe powerful tools shouldn't be complicated or restricted. LucidPlan is designed to simplify workflows and make project management accessible to everyone.")))

                                        (div (@ (class ,(mk-class '(max-w-prose
                                                                    bg-stone-200
                                                                    dark:bg-stone-900
                                                                    rounded-2xl
                                                                    p-4
                                                                    shadow-md
                                                                    flex
                                                                    flex-col
                                                                    gap-3))))
                                             ,(p (G_
                                                  "Discover how our fast-paced, no-nonsense workflows and deep customizability support Agile methodologies effectively."))
                                             ,(p (G_
                                                  "Built with the power of Lisp (Guile Scheme), LucidPlan offers a responsive and adaptable experience unlike those frustrating proprietary systems you might be used to.")))

                                        (div ,project-logo)))))))
  (export welcome-page))

