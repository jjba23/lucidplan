;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The body of work a ticket is a piece of.
;;;
;;; A project's plan is read at three heights, and this is the middle one: the
;;; project says what the whole thing is, a ticket says what one person does
;;; next, and an epic says what a group of those tickets is *for*.  Without it
;;; a backlog is a list of unrelated sentences -- forty tickets that are all
;;; equally important, which is the same as none of them being.
;;;
;;; An epic keeps no state of its own.  Its progress is the progress of its
;;; tickets, counted where the page that shows it already has them, so the two
;;; can never disagree; and it is finished when they are.  That is why this
;;; module stores a name and an account of one, and nothing else.

(define-module (lucidplan domain epic)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-epics get-epic-by-id get-epic-by-project-and-human-id
                       insert-epic update-epic-by-id soft-delete-epic))

;; -- Record definition ---

(define-record-with-kw (<epic-repository> %make-epic-repository
                                          epic-repository?)
                       mk-epic-repository
                       (get-epics-proc repo-get-epics)
                       (get-epic-by-id-proc repo-get-epic-by-id)
                       (get-epic-by-project-and-human-id-proc
                        repo-get-epic-by-project-and-human-id)
                       (insert-epic-proc repo-insert-epic)
                       (update-epic-by-id-proc repo-update-epic-by-id)
                       (soft-delete-epic-proc repo-soft-delete-epic))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-epics repo-get-epics
 "Retrieves the epics of a project from the given REPO, in the order their
names sort in.

A removed epic is not answered.  The tickets that pointed at it are not
removed with it -- a ticket is a fact about work and an epic is a way of
grouping facts -- so what a deleted epic leaves behind is tickets with no epic,
which is where they started.")

(define-repo-method get-epic-by-id repo-get-epic-by-id
 "Retrieves one epic by ID from the REPO, or @code{#f}.")

(define-repo-method get-epic-by-project-and-human-id
 repo-get-epic-by-project-and-human-id
 "Retrieves the epic a project's HUMAN-ID names from the REPO, or
@code{#f}.  It is how a route asks for one: the slug is the epic's name inside
its project, and two projects may each have their own.")

(define-repo-method insert-epic repo-insert-epic
 "Inserts a new epic using ATTRS into the given REPO.")

(define-repo-method update-epic-by-id repo-update-epic-by-id
 "Updates the epic ID with ATTRS in the given REPO.")

(define-repo-method soft-delete-epic repo-soft-delete-epic
 "Marks the epic ID as removed in the REPO.  The tickets that belonged to it
are left as they are: they keep pointing at a row the pages no longer draw,
which reads as a ticket with no epic until somebody gives it another.")
