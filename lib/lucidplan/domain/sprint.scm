;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; A stretch of work a team commits to.
;;;
;;; A sprint has a beginning, an end, and a state -- planned, active or
;;; closed -- and the tickets that are worked in it carry its id.  What it
;;; does *not* own is the tickets themselves: a ticket that leaves a sprint
;;; is still a ticket, so the reference lives on the ticket's side and a
;;; sprint is deleted without taking anything with it.
;;;
;;; Every procedure here addresses a project by its internal id rather than
;;; by the human id a URL carries.  The caller resolves it once -- see
;;; `project-id-from-human-id' -- because unlike a ticket, which is linked to
;;; from outside the application, a sprint is only ever reached through the
;;; project it belongs to.

(define-module (lucidplan domain sprint)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-sprints get-sprint-by-id get-running-sprint insert-sprint
                         update-sprint-by-id soft-delete-sprint))

;; -- Record definition ---

(define-record-with-kw (<sprint-repository> %make-sprint-repository
                                           sprint-repository?)
                       mk-sprint-repository
                       (get-sprints-proc repo-get-sprints)
                       (get-sprint-by-id-proc repo-get-sprint-by-id)
                       (get-running-sprint-proc repo-get-running-sprint)
                       (insert-sprint-proc repo-insert-sprint)
                       (update-sprint-by-id-proc repo-update-sprint-by-id)
                       (soft-delete-sprint-proc repo-soft-delete-sprint))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-sprints repo-get-sprints
 "Retrieves the sprints of PROJECT-ID from REPO, newest first.

Every sprint is answered, whatever state it is in: a closed sprint is what
the team reads when it asks how the last one went, so it is not hidden the
way a deleted row is.")

(define-repo-method get-sprint-by-id repo-get-sprint-by-id
                    "Retrieves the sprint SPRINT-ID from REPO.")

(define-repo-method get-running-sprint repo-get-running-sprint
 "Retrieves the sprint PROJECT-ID is currently working in, from REPO.

A project works in at most one sprint at a time, so this answers the single
active sprint, or @code{#f} when the team is between sprints -- which is a
state, not an error, and is what the pages draw as a backlog with nowhere to
pour it.")

(define-repo-method insert-sprint repo-insert-sprint
                    "Inserts a new sprint using ATTRS into the given REPO.")

(define-repo-method update-sprint-by-id repo-update-sprint-by-id
                    "Updates the sprint SPRINT-ID with ATTRS in the given REPO.")

(define-repo-method soft-delete-sprint repo-soft-delete-sprint
 "Marks the sprint SPRINT-ID as deleted in REPO.

Nothing else is touched: the tickets that were in it keep their own rows
and are simply no longer in a sprint, which is what the endpoint behind
this makes true before it deletes anything.")
