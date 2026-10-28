;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller me)

(import (lucidplan html)
        (lucidplan config)
        (lucidplan mail)
        (web uri)
        (ice-9 string-fun)
        (lucidplan env)
        (kracht prelude)
        (lucidplan auth)
        (app views me)
        (uuid generate)
        (artanis security nss)
        (lucidplan sqlite project)
        (lucidplan domain project)
        (lucidplan sqlite user)
        (lucidplan domain user)
        (lucidplan domain password-reset)
        (lucidplan sqlite password-reset)
        (bewaking bewaking)
        (bewaking password))

(get "/me"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((t (decrypt-token #:token (:cookies-value rc
                                         "x-bewaking-lucidplan")
                                #:encryption-key encryption-key
                                #:signing-key signing-key))
              (project-repo (make-sqlite-project-repository rc))
              (user-repo (make-sqlite-user-repository rc))
              (proj (get-projects project-repo))
              (user (get-user-by-id user-repo
                                    (assq-ref t
                                              'owner-id))))
         (catch #t
                (lambda ()
                  (response-emit (:mime rc
                                        (me-page (:i18n rc)
                                                 #:user user
                                                 #:projects proj
                                                 #:token t
                                                 #:is-logged-in (:cookies-value
                                                                 rc
                                                                 "x-bewaking-lucidplan")))))
                (lambda (key . args)
                  (display (format #f
                            "error getting user details-me page: ~a: ~a" key
                            args))
                  (redirect-to rc
                               (string->uri (format #f "~a/login"
                                                    (current-myhost)))
                               #:status 302))))))

(get "/users/manage"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (go-to-users-management)
         (let* ((G_ (:i18n rc))
                (project-repo (make-sqlite-project-repository rc))
                (proj (get-projects project-repo))
                (user-repo (make-sqlite-user-repository rc))
                (users (get-users user-repo))
                (notice (lambda (flag text)
                          (if (equal? "true"
                                      (get-from-qstr rc flag)) text #f)))
                (notices (filter (lambda (n)
                                   n)
                                 (list (notice "created"
                                               (G_
                                                "the account was created and an invite is on its way"))
                                       (notice "resent"
                                               (G_ "the invite was sent again"))
                                       (notice "edited"
                                               (G_ "the account was updated"))
                                       (notice "deleted"
                                               (G_ "the account was deleted"))
                                       (notice "mail-failed"
                                               (G_
                                                "the account was saved, but the invite email could not be delivered; try sending it again"))))))
           (response-emit (:mime rc
                                 (users-page G_
                                             #:users users
                                             #:projects proj
                                             #:notices notices)))))

       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-users-management
                          #:unhappy-flow back-to-login)))

(get "/password/reset/token/:token"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((G_ (:i18n rc))
              (token (params rc "token"))
              (password-reset-repo (make-sqlite-password-reset-repository rc))
              (tt (catch #t
                         (lambda ()
                           (get-password-reset-by-token password-reset-repo
                                                        token))
                         (lambda _
                           #f)))
              (now (current-time))
              (expired? (and tt
                             (> now
                                (+ (password-reset-validity)
                                   (assq-ref tt
                                             'created-at)))))
              (user-repo (make-sqlite-user-repository rc))
              (u (if (and tt
                          (not expired?))
                     (get-user-by-id user-repo
                                     (assq-ref tt
                                               'user-id)) #f)))
         (cond
           ;; The link is gone -- used, aged out, or never valid at all.
           ;; Sending the reader to the sign-in page, which carries the
           ;; request form, gives them the one thing that helps: a way to
           ;; ask for a new one.
           ((or (not tt) expired?
                (not u))
            (redirect-to rc
                         (string->uri (format #f
                                       "~a/login?invalid-reset-token=true"
                                       (current-myhost)))
                         #:status 302))
           (else (response-emit (:mime rc
                                       (reset-password-page G_
                                                            #:token token
                                                            #:email (assq-ref
                                                                     u
                                                                     'email)
                                                            #:error (cond
                                                                      ((equal?
                                                                        "mismatch"
                                                                        (get-from-qstr
                                                                         rc
                                                                         "error"))
                                                                       (G_
                                                                        "the two passwords did not match, please try again"))
                                                                      ((equal?
                                                                        "weak"
                                                                        (get-from-qstr
                                                                         rc
                                                                         "error"))
                                                                       (G_
                                                                        "that password was too weak, aim for a longer passphrase mixing words, case, digits and symbols"))
                                                                      (else #f))))))))))

(define (password-reset-validity)
  "How long a password-reset link stays good, in seconds."
  (config-ref 'password-reset-validity))

(post "/password/reset"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (let* ((existing-token (if (:cookies-value rc "x-bewaking-lucidplan")
                                   (decrypt-token #:token (:cookies-value rc
                                                           "x-bewaking-lucidplan")
                                                  #:encryption-key
                                                  encryption-key
                                                  #:signing-key signing-key)
                                   #f))
               (user-repo (make-sqlite-user-repository rc))
               (password-reset-repo (make-sqlite-password-reset-repository rc))
               (body (if (not (rc-body rc)) #f
                         (alist-from-url-encoded-body (rc-body rc))))
               (email (if (not body) #f
                          (assoc-ref body "email")))
               (u (if (not email)
                      (get-user-by-id user-repo
                                      (assq-ref existing-token
                                                'owner-id))
                      (get-user-by-email user-repo email))))
          ;; Whether the message went out or no such account exists, the
          ;; reader is answered the same way: a reset request must not be a
          ;; way to learn which addresses have an account here.
          (request-password-reset password-reset-repo u)
          (redirect-to rc
                       (string->uri (format #f
                                     "~a/login?sent-password-reset=true"
                                     (current-myhost)))
                       #:status 302))))

(post "/password/do-reset"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (reset-token (assoc-ref body "reset-token"))
               (new-password (assoc-ref body "new-password"))
               (confirm-password (assoc-ref body "confirm-password"))
               (password-reset-repo (make-sqlite-password-reset-repository rc))
               (token-url (format #f "~a/password/reset/token/~a"
                                  (current-myhost) reset-token))
               (tt (catch #t
                          (lambda ()
                            (get-password-reset-by-token password-reset-repo
                                                         reset-token))
                          (lambda _
                            #f)))
               (now (current-time))
               (expired? (and tt
                              (> now
                                 (+ (password-reset-validity)
                                    (assq-ref tt
                                              'created-at)))))
               (user-repo (make-sqlite-user-repository rc))
               (u (if (and tt
                           (not expired?))
                      (get-user-by-id user-repo
                                      (assq-ref tt
                                                'user-id)) #f)))
          ;; A reset is refused by sending the reader back to the form with
          ;; the reason, not by throwing: a mistyped confirmation or a
          ;; password that is too weak is something to fix, not a server
          ;; error, and the link they hold is still theirs to use.
          (cond
            ((or (not tt) expired?
                 (not u))
             (redirect-to rc
                          (string->uri (format #f
                                        "~a/login?invalid-reset-token=true"
                                        (current-myhost)))
                          #:status 302))
            ((not (and (string? new-password)
                       (not (string-null? new-password))))
             (redirect-to rc
                          (string->uri (format #f "~a?error=weak" token-url))
                          #:status 302))
            ((not (equal? new-password confirm-password))
             (redirect-to rc
                          (string->uri (format #f "~a?error=mismatch"
                                               token-url))
                          #:status 302))
            ;; bewaking scores the candidate itself, so a weak or common
            ;; password is refused before it ever reaches the salt.  The
            ;; score the server insists on is configuration.
            ((not (strong-password? #:password new-password
                                    #:min-score (config-ref 'password-min-score)))
             (redirect-to rc
                          (string->uri (format #f "~a?error=weak" token-url))
                          #:status 302))
            (else (let* ((new-salt (generate-string-uuid))
                         (h (string->sha-256 (string-append new-password
                                                            new-salt)))
                         (_ (update-user-by-id user-repo
                                               (assq-ref u
                                                         'id)
                                               `((salt unquote new-salt)
                                                 (password unquote h))))
                         (_ (delete-password-reset-by-user-id
                             password-reset-repo
                             (assq-ref u
                                       'id))))
                    (:cookies-set! rc
                                   'x-bewaking-lucidplan
                                   "x-bewaking-lucidplan" "")
                    (:cookies-setattr! rc
                                       'x-bewaking-lucidplan
                                       #:expires 1
                                       #:path "/"
                                       #:secure #t
                                       #:http-only #t)
                    (redirect-to rc
                                 (string->uri (format #f
                                               "~a/login?has-reset-password=true"
                                               (current-myhost)))
                                 #:status 302)))))))

(define (request-password-reset password-reset-repo u)
  "Create a fresh password-reset link for U and hand it to the mail service.

Answer @code{#t} when the message was accepted by the service and @code{#f}
when it could not be delivered after retrying, so a caller can tell the
reader their link is on its way -- or is not.  A user that does not exist
answers @code{#f} without writing a token, which is what lets the reset
request stay silent about whether an account exists at all."
  (if (not u) #f
      (let* ((_ (delete-password-reset-by-user-id password-reset-repo
                                                  (assq-ref u
                                                            'id)))
             (reset-token (base16-encode (format #f "~a-~a-~a"
                                                 (generate-string-uuid)
                                                 (generate-string-uuid)
                                                 (generate-string-uuid))))
             (_ (insert-password-reset password-reset-repo
                                       `((user-id unquote
                                                  (assq-ref u
                                                            'id))
                                         (reset-token unquote reset-token)
                                         (created-at unquote
                                                     (current-time)))))
             (mailed? (send-email! #:template 'password-reset
                                   #:subject "Password Reset"
                                   #:recipients (list (cons (assq-ref u
                                                                      'username)
                                                            (assq-ref u
                                                                      'email)))
                                   #:vars `((system-name unquote
                                                         (config-ref 'site-name))
                                            (user unquote
                                                  (assq-ref u
                                                            'email))
                                            (reset-link unquote
                                                        (format #f
                                                         "~a/password/reset/token/~a"
                                                         (current-myhost)
                                                         reset-token))))))
        mailed?)))

(get "/users/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (go-to-new-user)
         (let* ((G_ (:i18n rc))
                (error (cond
                         ((equal? "missing"
                                  (get-from-qstr rc "error"))
                          (G_
                           "a username and an email are both needed to create an account"))
                         ((equal? "email-taken"
                                  (get-from-qstr rc "error"))
                          (G_ "an account with that email already exists"))
                         (else #f))))
           (response-emit (:mime rc
                                 (new-user-page G_
                                                #:error error)))))

       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-new-user
                          #:unhappy-flow back-to-login)))

(get "/users/edit/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (go-to-edit-user)
         (let* ((user-repo (make-sqlite-user-repository rc))
                (user (get-user-by-id user-repo
                                      (params rc "id"))))
           (response-emit (:mime rc
                                 (edit-user-page (:i18n rc) user)))))

       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-edit-user
                          #:unhappy-flow back-to-login)))

(define (from-checkbox body n)
  (if (assoc-ref body
                 (symbol->string n)) n #f))

;; An account is created by an operator and claimed by its owner: the operator
;; names it and picks its permissions, and the account is handed to its owner
;; to set a password through an emailed link.  So a name and an address are
;; both required -- an account with neither is not an account -- and an
;; address already in use is refused, because two accounts sharing one address
;; could each reset the other's password.
(post "/users/new"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-new-user)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (username (html-escape (or (assoc-ref body "username") "")))
                 (email (html-escape (or (assoc-ref body "email") "")))
                 (permissions (filter (lambda (p)
                                        (not (equal? #f p)))
                                      (map (lambda (p)
                                             (from-checkbox body p))
                                           '(lucidplan:ticket-create
                                             lucidplan:ticket-delete
                                             lucidplan:project-create
                                             lucidplan:project-delete
                                             lucidplan:user-create
                                             lucidplan:user-delete
                                             lucidplan:modify-permissions))))
                 (user-repo (make-sqlite-user-repository rc))
                 (password-reset-repo (make-sqlite-password-reset-repository
                                       rc))
                 (existing (get-user-by-email user-repo email)))
            (cond
              ((or (string-null? username)
                   (string-null? email))
               (redirect-to rc
                            (string->uri (format #f
                                                 "~a/users/new?error=missing"
                                                 (current-myhost)))
                            #:status 302))
              ((not (equal? #f existing))
               (redirect-to rc
                            (string->uri (format #f
                                          "~a/users/new?error=email-taken"
                                          (current-myhost)))
                            #:status 302))
              (else (let* ((new-id (generate-string-uuid))
                           (_ (insert-user user-repo
                                           `((id unquote new-id)
                                             (username unquote username)
                                             (email unquote email)
                                             (password . "")
                                             (salt . "")
                                             (permissions unquote
                                                          (format #f "`~s"
                                                                  permissions)))))
                           (u (get-user-by-id user-repo new-id))
                           (mailed? (request-password-reset
                                     password-reset-repo u)))
                      (redirect-to rc
                                   (string->uri (format #f
                                                 "~a/users/manage/?created=true~a"
                                                 (current-myhost)
                                                 (if mailed? ""
                                                     "&mail-failed=true")))
                                   #:status 302))))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:modify-permissions)
                           #:rc rc
                           #:happy-flow do-new-user
                           #:unhappy-flow back-to-login)))

;; An invite that never arrived is not a reason to delete and recreate an
;; account: this hands the same reset link to the mail service again, on
;; demand, and says whether it went out.
(post "/users/resend-invite"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-resend-invite)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (user-repo (make-sqlite-user-repository rc))
                 (password-reset-repo (make-sqlite-password-reset-repository
                                       rc))
                 (u (get-user-by-id user-repo
                                    (assoc-ref body "id")))
                 (mailed? (request-password-reset password-reset-repo u)))
            (redirect-to rc
                         (string->uri (format #f
                                       "~a/users/manage/?resent=true~a"
                                       (current-myhost)
                                       (if mailed? "" "&mail-failed=true")))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:modify-permissions)
                           #:rc rc
                           #:happy-flow do-resend-invite
                           #:unhappy-flow back-to-login)))

(post "/users/delete"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete-user)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (user-repo (make-sqlite-user-repository rc))
                 (_ (delete-user-by-id user-repo
                                       (assoc-ref body "id"))))
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/users/manage/?deleted=true"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:modify-permissions)
                           #:rc rc
                           #:happy-flow do-delete-user
                           #:unhappy-flow back-to-login)))

(post "/users/edit/:id"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit-user)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (username (html-escape (assoc-ref body "username")))
                 (email (html-escape (assoc-ref body "email")))
                 (user-repo (make-sqlite-user-repository rc))
                 (permissions (filter (lambda (p)
                                        (not (equal? #f p)))
                                      (map (lambda (p)
                                             (from-checkbox body p))
                                           '(lucidplan:ticket-create
                                             lucidplan:ticket-delete
                                             lucidplan:project-create
                                             lucidplan:project-delete
                                             lucidplan:user-create
                                             lucidplan:user-delete
                                             lucidplan:modify-permissions))))
                 (_ (update-user-by-id user-repo
                                       (params rc "id")
                                       `((username unquote username)
                                         (email unquote email)
                                         (permissions unquote
                                                      (format #f "`~s"
                                                              permissions))))))
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/users/manage/?edited=true"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:modify-permissions)
                           #:rc rc
                           #:happy-flow do-edit-user
                           #:unhappy-flow back-to-login)))
