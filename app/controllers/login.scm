;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-artanis-controller login)

(import (lucidplan html)
        (lucidplan auth)
        (web uri)
        (ice-9 string-fun)
        (app models user)
        (app models project)
        (app views login))

(get "/login"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (login-page (:i18n rc))))))

(post "/login"
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (cond
          ((:session rc
                     'check)
           (redirect-to rc
                        (string->uri (format #f "~a"
                                             (current-myhost)))
                        #:status 302))
          ((:auth rc)
           (:session rc
                     'spawn)
           (redirect-to rc
                        (string->uri (format #f "~a"
                                             (current-myhost)))
                        #:status 302))
          (else (redirect-to rc
                             (string->uri (format #f
                                           "~a/login?login_failed=true"
                                           (current-myhost)))
                             #:status 302)))))
