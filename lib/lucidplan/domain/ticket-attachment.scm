;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; A file a ticket carries.  What the uploader called it is kept as the row
;;; says -- so a download is named the way it was sent -- while the bytes are
;;; stored under a generated name that no submitted text can influence.  That
;;; separation is the whole reason the table has two name columns.

(define-module (lucidplan domain ticket-attachment)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-attachments get-ticket-attachment-by-id
                                   insert-ticket-attachment
                                   soft-delete-ticket-attachment
                                   move-ticket-attachments))

;; -- Record definition ---

(define-record-with-kw (<ticket-attachment-repository>
                        %make-ticket-attachment-repository
                        ticket-attachment-repository?)
                       mk-ticket-attachment-repository
                       (get-ticket-attachments-proc
                        repo-get-ticket-attachments)
                       (get-ticket-attachment-by-id-proc
                        repo-get-ticket-attachment-by-id)
                       (insert-ticket-attachment-proc
                        repo-insert-ticket-attachment)
                       (soft-delete-ticket-attachment-proc
                        repo-soft-delete-ticket-attachment)
                       (move-ticket-attachments-proc
                        repo-move-ticket-attachments))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-ticket-attachments repo-get-ticket-attachments
 "Retrieves the files TICKET-ID carries from REPO, newest first.

A removed file is not answered: unlike a comment, nothing hangs from an
attachment, so its row has no reason to be read again.")

(define-repo-method get-ticket-attachment-by-id
 repo-get-ticket-attachment-by-id
 "Retrieves one attachment by ATTACHMENT-ID from REPO, or @code{#f}.")

(define-repo-method insert-ticket-attachment repo-insert-ticket-attachment
 "Inserts a new attachment using ATTRS into the given REPO.")

(define-repo-method soft-delete-ticket-attachment
 repo-soft-delete-ticket-attachment
 "Marks ATTACHMENT-ID as removed in REPO.  The bytes are left where they are
until an operator prunes them; the row is what the page and the download
endpoint read.")

(define-repo-method move-ticket-attachments repo-move-ticket-attachments
 "Rewrites the PROJECT-ID of every attachment of TICKET-ID in REPO.")
