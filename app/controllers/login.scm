;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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

;; The sign-in page is also where the outcome of an attempt is read: a wrong
;; password, a reset that went through, a reset link that had aged out, or a
;; request that may or may not have matched an account.  Each is written as a
;; short notice above the forms rather than as a redirect to a bare page, so
;; the reader is told what happened and can try again without losing the page.
(get "/login"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((G_ (:i18n rc))
              (flag? (lambda (key)
                       (equal? "true"
                               (get-from-qstr rc key))))
              (notices (filter (lambda (notice)
                                 notice)
                               (list (if (flag? "login_failed")
                                         (G_
                                          "that email and password did not match, please try again")
                                         #f)
                                     (if (flag? "has-reset-password")
                                         (G_
                                          "your password was reset, please sign in with the new one")
                                         #f)
                                     (if (flag? "invalid-reset-token")
                                         (G_
                                          "that password reset link is no longer valid, please request a new one")
                                         #f)
                                     (if (flag? "sent-password-reset")
                                         (G_
                                          "if that account exists, a reset link is on its way to its email address")
                                         #f)))))
         (response-emit (:mime rc
                               (login-page G_
                                           #:notices notices))))))

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
                                  ;; token is valid for 31 days
                                  #:expires (* 3600 24 31)
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
