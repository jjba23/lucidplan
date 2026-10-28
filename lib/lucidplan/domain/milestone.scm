;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; A point in a project's future that tickets count toward.
;;;
;;; A milestone is not a sprint and is deliberately not shaped like one.  A
;;; sprint is a promise the team makes to itself about a stretch of work; a
;;; milestone is a promise made outside the team about a result, which is why
;;; it has a date of its own -- several sprints may feed one milestone, and
;;; the milestone's date does not move when a sprint does.
;;;
;;; As with a sprint, the reference lives on the ticket: a ticket carries the
;;; milestone it counts toward, so a milestone that is deleted leaves its
;;; tickets alone.

(define-module (lucidplan domain milestone)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-milestones get-milestone-by-id insert-milestone
                         update-milestone-by-id soft-delete-milestone))

;; -- Record definition ---

(define-record-with-kw (<milestone-repository> %make-milestone-repository
                                              milestone-repository?)
                       mk-milestone-repository
                       (get-milestones-proc repo-get-milestones)
                       (get-milestone-by-id-proc repo-get-milestone-by-id)
                       (insert-milestone-proc repo-insert-milestone)
                       (update-milestone-by-id-proc repo-update-milestone-by-id)
                       (soft-delete-milestone-proc repo-soft-delete-milestone))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-milestones repo-get-milestones
 "Retrieves the milestones of PROJECT-ID from REPO, soonest first.

A milestone with no date sorts before the dated ones, because SQLite puts
NULL first in an ascending order -- and a milestone nobody has dated yet is
exactly the one a reader has to notice and put a date on.")

(define-repo-method get-milestone-by-id repo-get-milestone-by-id
                    "Retrieves the milestone MILESTONE-ID from REPO.")

(define-repo-method insert-milestone repo-insert-milestone
                    "Inserts a new milestone using ATTRS into the given REPO.")

(define-repo-method update-milestone-by-id repo-update-milestone-by-id
                    "Updates the milestone MILESTONE-ID with ATTRS in REPO.")

(define-repo-method soft-delete-milestone repo-soft-delete-milestone
                    "Marks the milestone MILESTONE-ID as deleted in REPO.")
