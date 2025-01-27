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
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page)
  (html-template #:body `((h2 (@ (class "")) "login to lucidplan")
                          (div (@ (class
                                    "w-full rounded-xl bg-stone-100 p-4 flex flex-col gap-4 align-center"))
                               (form (@ (action "/login")
                                        (method "POST"))
                                     (label (G_ "username"))
                                     (input (@ (type "text")
                                               (class "")
                                               (name "username")) "")
                                     (label (G_ "password:"))
                                     (input (@ (type "password")
                                               (class "")
                                               (name "password")) "")
                                     (button (@ (type "submit")
                                                (class "")) "login"))))))

