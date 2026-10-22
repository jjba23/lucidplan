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
