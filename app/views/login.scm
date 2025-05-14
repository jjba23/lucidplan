;;; lucidplan - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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

(define-module (app views login)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page G_)
  (let ((login-form `(form (@ (action "/login")
                              (class ,(mk-class '(w-full rounded-xl
                                                         bg-stone-200/50
                                                         dark:bg-stone-700/50
                                                         p-4
                                                         flex
                                                         flex-row
                                                         flex-wrap
                                                         items-center
                                                         gap-4
                                                         shadow-md)))
                              (method "POST"))
                           ,(input #:label (G_ "username")
                                   #:name "username"
                                   #:value ""
                                   #:required #t)
                           ,(input #:label (G_ "password")
                                   #:name "password"
                                   #:type 'password
                                   #:value ""
                                   #:required #t)
                           (div ,(submit-button (G_ "login"))))))
    (html-template G_
                   #:body `(div (@ (class ,(mk-class '(container mx-auto))))
                                ((div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-8))))
                                      ,(h2 (G_ "login to lucidplan"))
                                      (div ,login-form)))))))

