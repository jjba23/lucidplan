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

(define-artanis-controller index)

(import (lucidplan html)
        (web uri)
        (ice-9 string-fun)
        (app models user)
        (lucidplan env)
        (app views me)
        (bewaking bewaking))

(get "/me"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((t (decrypt-token #:token (:cookies-value rc
                                         "x-bewaking-lucidplan")
                                #:encryption-key encryption-key
                                #:signing-key signing-key))
              (proj ($project 'get
                              #:order-by '(human_id asc)))
              (user ($user 'get
                           #:ret 'top
                           #:condition (where #:id (assoc-ref t
                                                              'owner-id)))))
         (write (car user))
         (response-emit (:mime rc
                               (me-page (:i18n rc)
                                        #:user (car user)
                                        #:projects proj
                                        #:is-logged-in (:cookies-value rc
                                                        "x-bewaking-lucidplan")))))))

