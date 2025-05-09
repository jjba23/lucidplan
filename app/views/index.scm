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

(define-module (app views index)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile components input)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components typography)
  #:use-module (hygguile components button)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (welcome-page G_)
  (let ((project-logo `(((div (@ (class ,(->css '(bg-stone-700 rounded-2xl p-4 shadow))))
                              (img (@ (loading lazy)
                                      (class ,(->css '(w-64 h-auto rounded-2xl)))
                                      (src
                                       "https://jointhefreeworld.org/static-assets/lucidplan/logo.jpg"))))))))
    (html-template G_
                   #:body `(div (@ (class ,(->css '(container mx-auto))))
                                ((div (@ (class ,(->css '(flex flex-col gap-8))))
                                      ,(hyg-h2 (G_ "welcome to lucidplan"))
                                      (div ,project-logo)))))))

