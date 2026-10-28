;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain password-reset)
  #:export (make-sqlite-password-reset-repository))

;; --- SQLite implementation ---
(define (make-sqlite-password-reset-repository rc)
  (define columns
    '(user-id reset-token created-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  ;; The row is answered whole: a caller needs the user the token belongs to
  ;; *and* when the token was made, and the two are read back by name -- the
  ;; earlier shape, a bare `(token . user-id)' cons, dropped the timestamp the
  ;; caller then looked for, so every reset link answered as invalid.
  (define (get-password-reset-by-token token)
    (let ((rows (execute-read "get-password-reset-by-token"
                              (sql-select 'password_reset
                                          #:columns columns
                                          #:where (list (cons 'reset-token
                                                              token))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (delete-password-reset-by-user-id user-id)
    (execute-write "delete-password-reset-by-user-id"
                   (sql-delete 'password_reset
                               #:where (list (cons 'user-id user-id)))))

  (define (insert-password-reset attrs)
    (execute-write "insert-password-reset"
                   (sql-insert 'password_reset attrs)))

  (mk-password-reset-repository #:get-password-reset-by-token-proc
                                get-password-reset-by-token
                                #:delete-password-reset-by-user-id-proc
                                delete-password-reset-by-user-id
                                #:insert-password-reset-proc
                                insert-password-reset))
