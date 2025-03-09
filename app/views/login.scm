;;; lucidplan - free and open project management for everyone

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

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
  #:use-module (hygguile components input)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components button)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page G_)
  (let ((login-form `(form (@ (action "/login")
                              (class
                                "w-full rounded-xl bg-stone-200/50 p-4 flex flex-row flex-wrap items-center gap-4")
                              (method "POST"))
                           ,(hyg-input #:label (G_ "username")
                                       #:name "username"
                                       #:value "Joe"
                                       #:required #t)
                           ,(hyg-input #:label (G_ "password")
                                       #:name "password"
                                       #:type 'password
                                       #:value ""
                                       #:required #t)
                           (div ,(hyg-submit-button #:content (G_ "login"))))))
    (html-template G_
                   #:body `(div (@ (class "container mx-auto"))
                                ((h2 (@ (class
                                          "font-bold text-stone-800/75 text-md"))
                                     ,(G_ "login to lucidplan"))
                                 (div ,login-form))))))

