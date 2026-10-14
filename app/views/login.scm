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

(define-module (app views login)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (login-page))

(define* (login-page G_
                     #:key (notices '()))
  (let* ((login-form `(form (@ (action "/login")
                               (class ,(mk-class (append '(w-full rounded-xl
                                                           p-4
                                                           flex
                                                           flex-row
                                                           flex-wrap
                                                           items-center
                                                           align-middle
                                                           gap-4
                                                           shadow-md)
                                                         frosted-tint-classes)))
                               (method "POST"))
                            ,(input #:label (G_ "email")
                                    #:name "email"
                                    #:type 'email
                                    #:value ""
                                    #:autocomplete "email"
                                    #:required #t)
                            ,(input #:label (G_ "password")
                                    #:name "password"
                                    #:type 'password
                                    #:value ""
                                    #:autocomplete "current-password"
                                    #:required #t)
                            (div ,(submit-button (G_ "login")))))
         (reset-form `((form (@ (action "/password/reset")
                                (class ,(mk-class (append '(w-full rounded-xl
                                                            p-4
                                                            flex
                                                            flex-row
                                                            flex-wrap
                                                            items-center
                                                            items-center
                                                            gap-4
                                                            shadow-md)
                                                   frosted-tint-classes)))
                                (method POST))
                             ,(input #:label (G_ "account to reset (email)")
                                     #:name "email"
                                     #:type 'email
                                     #:value ""
                                     #:autocomplete "email"
                                     #:required #t)
                             (div ,(icon-button #:icon 'password
                                                #:type 'submit
                                                #:content (G_ "reset")
                                                #:icon-css '(text-md
                                                             text-onyx-700
                                                             dark:text-stone-300)))))))
    (html-template G_
                   #:title (G_ "login")
                   #:canonical-path "/login"
                   #:body `((div (@ (class ,(mk-class '(px-4 pt-4 pb-24))))
                                 ;; What happened on the last attempt -- a
                                 ;; failed sign-in, a reset that went
                                 ;; through or a link that had aged out --
                                 ;; is said here, above the forms, in a live
                                 ;; region, so a reader is told before they
                                 ;; fill anything in again.
                                 ,@(map (lambda (notice)
                                          `(div (@ (class ,(mk-class '(my-2
                                                                       px-4
                                                                       py-2
                                                                       max-w-prose
                                                                       rounded-xl
                                                                       border-2
                                                                       border-jeans-blue-500/50
                                                                       text-onyx-700
                                                                       dark:text-stone-300)))
                                                   (role "status"))
                                                ,notice)) notices)
                                 (div (@ (class ,(mk-class '(w-full flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    gap-10))))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  gap-8))))
                                           ,(h2 (G_ "login to lucidplan"))
                                           (div ,login-form))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  gap-8))))
                                           ,(h2 (G_ "reset password"))
                                           (div ,reset-form))))))))

