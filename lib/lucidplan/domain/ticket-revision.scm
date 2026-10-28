;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Version control, in the sense a ticket needs: every time the content of
;;; a ticket changes, the content it now has is written down as a numbered
;;; snapshot.  A version is never rewritten.  Restoring an old one is an
;;; ordinary edit whose new content happens to be the old content, so it
;;; leaves the trail of what happened intact rather than erasing the versions
;;; between.
;;;
;;; What is versioned is the ticket's content -- title, description, status,
;;; type, priority and both estimates.  Comments and attachments are not
;;; snapshotted: they are append-only records of their own, and the audit
;;; trail says when they were added.

(define-module (lucidplan domain ticket-revision)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-revisions get-ticket-revision-by-version
                                 insert-ticket-revision next-ticket-version))

;; -- Record definition ---

(define-record-with-kw (<ticket-revision-repository>
                        %make-ticket-revision-repository
                        ticket-revision-repository?)
                       mk-ticket-revision-repository
                       (get-ticket-revisions-proc repo-get-ticket-revisions)
                       (get-ticket-revision-by-version-proc
                        repo-get-ticket-revision-by-version)
                       (insert-ticket-revision-proc
                        repo-insert-ticket-revision))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-ticket-revisions repo-get-ticket-revisions
 "Retrieves the versions of TICKET-ID from REPO, newest first.")

(define-repo-method get-ticket-revision-by-version
 repo-get-ticket-revision-by-version
 "Retrieves version VERSION of TICKET-ID from REPO, or @code{#f}.")

(define-repo-method insert-ticket-revision repo-insert-ticket-revision
                    "Inserts a snapshot using ATTRS into the given REPO.")

(define (next-ticket-version repo ticket-id)
  "Answer the number the next snapshot of TICKET-ID should carry in REPO.

Versions start at one.  The answer is taken from the highest version already
written -- not from a counter kept elsewhere -- so a ticket imported with a
history, or one whose versions were written by another process, continues the
sequence instead of colliding with it."
  (let ((revisions (get-ticket-revisions repo ticket-id)))
    (if (null? revisions) 1
        (+ 1
           (or (assq-ref (car revisions)
                         'version) 0)))))
