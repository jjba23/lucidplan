;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-restful-api v1)

(import (lucidplan auth)
        (kracht prelude)
        (bewaking bewaking)
        (ice-9 textual-ports)
        (artanis security nss)
        (lucidplan env)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain user)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite user))

(get "/api/v1/tickets/:project"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (tickets (get-tickets ticket-repo
                                      (params rc "project")
                                      '((human-id asc)))))
           (:mime rc
                  `((data unquote
                          (list->vector tickets))))))
       (bewaking-api-endpoint #:token-name 'x-bewaking-lucidplan
                              #:needed-permissions '()
                              #:rc rc
                              #:happy-flow do-get
                              #:unhappy-flow unauthorized-api-message)))

(get "/api/v1/projects"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (projects (get-projects project-repo)))
           (:mime rc
                  `((data unquote
                          (list->vector projects))))))
       (bewaking-api-endpoint #:token-name 'x-bewaking-lucidplan
                              #:needed-permissions '()
                              #:rc rc
                              #:happy-flow do-get
                              #:unhappy-flow unauthorized-api-message)))

(post "/api/v1/login"
      #:mime 'json
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (password (assoc-ref body "password"))
               (user-repo (make-sqlite-user-repository rc))
               (u (get-user-by-email user-repo email))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assoc-ref u
                                                     'id))
                                      (email unquote
                                             (assoc-ref u
                                                        'email))
                                      (name unquote
                                            (assoc-ref u
                                                       'username))
                                      (permissions unquote
                                                   (eval-string (assoc-ref u
                                                                           'permissions))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))

          (cond
            ((equal? (string->sha-256 (string-append password
                                                     (assoc-ref u
                                                                'salt)))
                     (assoc-ref u
                                'password))
             (response-emit ""
                            #:status 204
                            #:headers `((x-bewaking-lucidplan unquote t))))
            (else (response-emit '((error . "invalid credentials, could not make token"))
                                 #:status 401
                                 #:headers '()))))))
