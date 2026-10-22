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

;;; A ticket's comments are a thread, not a list: every reply names the
;;; comment it answers, and the page is drawn by following those names down
;;; from the comments that answer the ticket itself.  The shape is the one
;;; readers already know from discussion sites -- a reply sits under what it
;;; answers, indented by its depth -- which is why the parent is a column of
;;; its own rather than a quoting convention inside the body.
;;;
;;; Deleting a comment does not remove its row.  A reply that answers a
;;; comment which is no longer shown would otherwise lose its place in the
;;; thread, so a deleted comment is kept as the empty node the replies hang
;;; from, and the page says so where the body would be.

(define-module (lucidplan domain ticket-comment)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-comments insert-ticket-comment
                                soft-delete-ticket-comment
                                move-ticket-comments comment-children
                                comment-depth))

;; -- Record definition ---

(define-record-with-kw (<ticket-comment-repository>
                        %make-ticket-comment-repository
                        ticket-comment-repository?)
                       mk-ticket-comment-repository
                       (get-ticket-comments-proc repo-get-ticket-comments)
                       (insert-ticket-comment-proc repo-insert-ticket-comment)
                       (soft-delete-ticket-comment-proc
                        repo-soft-delete-ticket-comment)
                       (move-ticket-comments-proc repo-move-ticket-comments))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-ticket-comments repo-get-ticket-comments
 "Retrieves every comment of TICKET-ID from REPO, oldest first.

Deleted comments are included: they are the nodes their replies hang from,
so dropping them would promote an answer to a question that is no longer
there.  The page reads their DELETED-AT column and draws them as removed.")

(define-repo-method insert-ticket-comment repo-insert-ticket-comment
                    "Inserts a new comment using ATTRS into the given REPO.")

(define-repo-method soft-delete-ticket-comment repo-soft-delete-ticket-comment
 "Marks the comment COMMENT-ID as deleted in REPO, keeping its place in the
thread.")

(define-repo-method move-ticket-comments repo-move-ticket-comments
 "Rewrites the PROJECT-ID of every comment of TICKET-ID in REPO.

A comment belongs to the ticket it was written on, so moving the ticket to
another project moves its discussion with it.")

;; --- The thread, read back ---

(define (comment-same-id? a b)
  "Are A and B the same identifier, written as text?"
  (and a b
       (equal? (format #f "~a" a)
               (format #f "~a" b))))

(define (comment-children comments parent-id)
  "Answer the comments in COMMENTS that answer PARENT-ID, in their order.

PARENT-ID is converted to a string before it is compared, so a parent read
back from the database and a parent taken from a request -- which is text --
name the same comment.  A comment whose PARENT-ID is missing is a comment on
the ticket itself, and is answered when PARENT-ID is @code{#f}."
  (filter (lambda (comment)
            (let ((parent (assq-ref comment
                                    'parent-id)))
              (if parent-id
                  (comment-same-id? parent parent-id)
                  (not parent)))) comments))

(define (comment-depth comments comment)
  "Answer how many comments sit between COMMENT and the ticket it is on.

The count is what the page indents by.  A malformed thread -- a parent that
is not in COMMENTS, or a cycle somebody wrote around the application -- is
answered at the depth reached so far rather than followed forever."
  (let loop
    ((current comment)
     (depth 0)
     (seen '()))
    (let ((id (assq-ref current
                        'id))
          (parent-id (assq-ref current
                               'parent-id)))
      (if (or (not parent-id)
              (>= depth 64)
              (member id seen)) depth
          (let ((parent (find (lambda (c)
                                (equal? (format #f "~a"
                                                (assq-ref c
                                                          'id))
                                        (format #f "~a" parent-id))) comments)))
            (if parent
                (loop parent
                      (+ depth 1)
                      (cons id seen)) depth))))))
