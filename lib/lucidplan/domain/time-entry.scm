;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; What somebody did and how long it took.
;;;
;;; A time entry belongs to the ticket it was logged on and to the project
;;; that ticket is in, and says three things: how many minutes it took, which
;;; day it was, and what was done.  The comment is not a note attached to the
;;; entry -- it is the half of the entry that says what the hours bought, so a
;;; time log that could not carry one would be a timesheet rather than a
;;; record of work.
;;;
;;; Deleting an entry is a soft delete, like every other piece of a ticket's
;;; record: the row stays, the page stops drawing it, and the trail keeps
;;; saying that the work was logged and then taken back.

(define-module (lucidplan domain time-entry)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-time-entries insert-time-entry soft-delete-time-entry
                             move-time-entries))

;; -- Record definition ---

(define-record-with-kw (<time-entry-repository> %make-time-entry-repository
                                                time-entry-repository?)
                       mk-time-entry-repository
                       (get-time-entries-proc repo-get-time-entries)
                       (insert-time-entry-proc repo-insert-time-entry)
                       (soft-delete-time-entry-proc repo-soft-delete-time-entry)
                       (move-time-entries-proc repo-move-time-entries))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-time-entries repo-get-time-entries
 "Retrieves the time entries of TICKET-ID from REPO, newest day first.

An entry that was removed is left out: unlike a comment, which is the node
its replies hang from and therefore has to stay in the list, a time entry
is a leaf -- nothing answers it, nothing points at it -- so once it is
taken back there is nothing left to draw.")

(define-repo-method insert-time-entry repo-insert-time-entry
                    "Inserts a new time entry using ATTRS into the given REPO.")

(define-repo-method soft-delete-time-entry repo-soft-delete-time-entry
                    "Marks the time entry ENTRY-ID as removed in REPO.")

(define-repo-method move-time-entries repo-move-time-entries
 "Rewrites the PROJECT-ID of every time entry of TICKET-ID in REPO.

Time was spent on a ticket, so moving the ticket to another project moves
the record of that time with it -- otherwise the hours would be counted in
a project whose board no longer shows the ticket they were spent on.")
