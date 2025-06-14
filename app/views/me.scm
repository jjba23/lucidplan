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

(define-module (app views me)
  #:use-module (lucidplan html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (me-page))

(define* (me-page G_
                  #:key is-logged-in
                  token
                  user
                  projects)
  (let ((user-details `((div (@ (class ,(mk-class '())))
                             ,(h3 (format #f "~a &lt;~a&gt;"
                                          (assoc-ref user "username")
                                          (assoc-ref user "email"))))))
        (session-duration `((div (@ (class ,(mk-class '())))
                                 ,(em (format #f "~a: ~a"
                                              (G_ "logged in until")
                                              (strftime "%c"
                                                        (localtime (assoc-ref
                                                                    token
                                                                    'token-expires-at)))))))))
    (html-template G_
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(container mx-auto))))
                                (div (@ (class ,(mk-class '(flex flex-col
                                                                 gap-6))))
                                     ,user-details
                                     ,session-duration)
                                (div (@ (class ,(mk-class '(py-6 flex flex-row flex-wrap gap-6))))
                                     (a (@ (class ,(mk-class '(text-sm
                                                               lowercase
                                                               text-white
                                                               dark:text-black
                                                               font-sans
                                                               font-medium
                                                               px-2
                                                               flex-items-center
                                                               gap-2)))
                                           (href "/users/manage"))
                                        ,(icon-button #:icon 'user-switch
                                                      #:content (G_
                                                                 "go to user management")))
                                     (a (@ (class ,(mk-class '(text-sm
                                                               lowercase
                                                               text-white
                                                               dark:text-black
                                                               font-sans
                                                               font-medium
                                                               px-2
                                                               flex-items-center
                                                               gap-2)))
                                           (href "/logout"))
                                        ,(icon-button #:icon 'sign-out
                                                      #:content (G_
                                                                 "logout"))))))))
unbalanced
unbalanced

