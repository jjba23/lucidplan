;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; A file a ticket carries.  What the uploader called it is kept as the row
;;; says -- so a download is named the way it was sent -- while the bytes live
;;; in the row itself, and the name this instance gave the upload is a name no
;;; submitted text can influence.  That separation is why the table has two
;;; name columns and a data column.

(define-module (lucidplan domain ticket-attachment)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-attachments get-ticket-attachment-by-id
                                   get-ticket-attachment-data
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
                       (get-ticket-attachment-data-proc
                        repo-get-ticket-attachment-data)
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

(define-repo-method get-ticket-attachment-data repo-get-ticket-attachment-data
 "Retrieves the bytes of ATTACHMENT-ID from REPO, as the base64 text the row
carries, or @code{#f} when the row is not there.

The bytes are asked for apart from the row that describes them so that a list
of a ticket's files never has to carry them.")

(define-repo-method insert-ticket-attachment repo-insert-ticket-attachment
 "Inserts a new attachment using ATTRS into the given REPO.")

(define-repo-method soft-delete-ticket-attachment
 repo-soft-delete-ticket-attachment
 "Marks ATTACHMENT-ID as removed in REPO.  The bytes stay in the row: a removal
here is soft -- the trail still says the file was attached -- and the one thing
that would make restoring it impossible is throwing away what it holds.")

(define-repo-method move-ticket-attachments repo-move-ticket-attachments
 "Rewrites the PROJECT-ID of every attachment of TICKET-ID in REPO.")
