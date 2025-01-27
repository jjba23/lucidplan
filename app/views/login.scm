;;; lucidplan - free and open project management for everyone

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

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
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page G_)
  (html-template G_
                 #:body `(div (@ (class "container mx-auto"))
                              ((h2 (@ (class
                                       "font-bold text-stone-800/75 text-md"))
                                   ,(G_ "login to lucidplan"))
                               (div (form (@ (action "/login")
                                             (class
                                              "w-full rounded-xl bg-stone-200/50 p-4 flex flex-row flex-wrap items-center gap-4 align-center")
                                             (method "POST"))
                                          (label ,(G_ "username"))
                                          (input (@ (type "text")
                                                    (class "p-3 rounded-xl")
                                                    (name "username")) "")
                                          (label ,(G_ "password"))
                                          (input (@ (type "password")
                                                    (class "p-3 rounded-xl")
                                                    (name "password")) "")
                                          (button (@ (type "submit")
                                                     (class
                                                      "p-2 rounded-xl w-min bg-green-700/75 text-white"))
                                                  "login")))))))

