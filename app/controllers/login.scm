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

(define-artanis-controller login)

(import (lucidplan html)
        (lucidplan auth)
        (web uri)
        (ice-9 string-fun)
        (bewaking bewaking)
        (artanis security nss)
        (lucidplan env)
        (kracht prelude)
        (lucidplan sqlite user)
        (lucidplan domain user)
        (app views login))

(get "/login"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (login-page (:i18n rc))))))

(post "/login"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (password (assoc-ref body "password"))
               (user-repo (make-sqlite-user-repository rc))
               (u (get-user-by-email user-repo email))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assq-ref u
                                                    'id))
                                      (email unquote
                                             (assq-ref u
                                                       'email))
                                      (name unquote
                                            (assq-ref u
                                                      'username))
                                      (permissions unquote
                                                   (eval-string (assq-ref u
                                                                          'permissions))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))
          
          (cond
            ((equal? (string->sha-256 (string-append password
                                                     (assq-ref u
                                                               'salt)))
                     (assq-ref u
                               'password))
             (begin
               (:cookies-set! rc
                              'x-bewaking-lucidplan "x-bewaking-lucidplan" t)
               (:cookies-setattr! rc
                                  'x-bewaking-lucidplan
                                  #:expires (* 3600 24 7)
                                  #:path "/"
                                  #:secure #t
                                  #:http-only #t)

               (redirect-to rc
                            (string->uri (format #f
                                          "~a/projects?login_succeeded=true"
                                          (current-myhost)))
                            #:status 302)))
            (else (redirect-to rc
                               (string->uri (format #f
                                             "~a/login?login_failed=true"
                                             (current-myhost)))
                               #:status 302))))))

(get "/logout"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (begin
         (:cookies-set! rc
                        'x-bewaking-lucidplan "x-bewaking-lucidplan" "")
         (:cookies-setattr! rc
                            'x-bewaking-lucidplan
                            #:expires 1
                            #:path "/"
                            #:secure #t
                            #:http-only #t)

         (redirect-to rc
                      (string->uri (format #f "~a/"
                                           (current-myhost)))
                      #:status 302))))
