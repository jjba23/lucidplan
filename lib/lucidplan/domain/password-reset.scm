;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan domain password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-password-reset-by-token delete-password-reset-by-user-id
                                        insert-password-reset))

;; -- Record definition ---

(define-record-with-kw (<password-reset-repository>
                        %make-password-reset-repository
                        password-reset-repository?)
                       mk-password-reset-repository
                       (get-password-reset-by-token-proc
                        repo-get-password-reset-by-token)
                       (delete-password-reset-by-user-id-proc
                        repo-delete-password-reset-by-user-id)
                       (insert-password-reset-proc repo-insert-password-reset))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-password-reset-by-token
 repo-get-password-reset-by-token
 "Retrieves the password reset associated with a specific TOKEN from the REPO.")

(define-repo-method delete-password-reset-by-user-id
 repo-delete-password-reset-by-user-id
 "Deletes the password reset associated with a specific USER-ID from the REPO")

(define-repo-method insert-password-reset repo-insert-password-reset
 "Inserts a new password-reset using ATTRS into the given REPO.")
