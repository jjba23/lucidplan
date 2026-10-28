;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan domain ticket)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-tickets search-tickets
                        get-ticket-by-project-human-id-and-human-id
                        get-ticket-of-latest-human-id
                        update-ticket-by-project-human-id-and-human-id
                        update-ticket-estimates-by-id
                        update-ticket-planning-by-id
                        get-tickets-by-sprint
                        unassign-sprint
                        unassign-milestone
                        ticket-id-from-human-id
                        insert-ticket))

;; -- Record definition ---

(define-record-with-kw (<ticket-repository> %make-ticket-repository
                                            ticket-repository?)
                       mk-ticket-repository
                       (get-tickets-proc repo-get-tickets)
                       (get-ticket-of-latest-human-id-proc
                        repo-get-ticket-of-latest-human-id)
                       (search-tickets-proc repo-search-tickets)
                       (get-ticket-by-project-human-id-and-human-id-proc
                        repo-get-ticket-by-project-human-id-and-human-id)
                       (update-ticket-by-project-human-id-and-human-id-proc
                        repo-update-ticket-by-project-human-id-and-human-id)
                       (update-ticket-estimates-by-id-proc
                        repo-update-ticket-estimates-by-id)
                       (update-ticket-planning-by-id-proc
                        repo-update-ticket-planning-by-id)
                       (get-tickets-by-sprint-proc repo-get-tickets-by-sprint)
                       (unassign-sprint-proc repo-unassign-sprint)
                       (unassign-milestone-proc repo-unassign-milestone)
                       (insert-ticket-proc repo-insert-ticket))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-tickets repo-get-tickets
 "Retrieves active tickets for a given PROJECT-ID, ordered by SORT, from REPO.")

(define-repo-method get-ticket-of-latest-human-id
                    repo-get-ticket-of-latest-human-id "")

(define-repo-method search-tickets repo-search-tickets
 "Searches active tickets in PROJECT-ID matching query Q, ordered by SORT, from REPO.")

(define-repo-method get-ticket-by-project-human-id-and-human-id
 repo-get-ticket-by-project-human-id-and-human-id
 "Retrieves a specific active ticket matching PROJECT-ID and HUMAN-ID from REPO.")

(define-repo-method update-ticket-by-project-human-id-and-human-id
 repo-update-ticket-by-project-human-id-and-human-id
 "Updates a ticket identified by PROJECT-ID and HUMAN-ID with ATTRS in the given REPO.")

(define-repo-method update-ticket-estimates-by-id
 repo-update-ticket-estimates-by-id
 "Writes the two estimates of TICKET-ID in REPO, either of which may be @code{#f}
for an unestimated ticket.  They are written apart from the other columns
because an estimate that is cleared is a NULL, and the general update builder
cannot bind one.")

(define-repo-method insert-ticket repo-insert-ticket
                    "Inserts a new ticket using ATTRS into the given REPO.")

(define-repo-method update-ticket-planning-by-id repo-update-ticket-planning-by-id
 "Writes the deadline, the sprint and the milestone of TICKET-ID in REPO,
each of which may be @code{#f} for a ticket that has none.

They are written apart from the other columns for the reason the estimates
are: a reference that is cleared is a NULL, and the general update builder
cannot bind one.")

(define-repo-method get-tickets-by-sprint repo-get-tickets-by-sprint
 "Retrieves the active tickets of SPRINT-ID from REPO, ordered by SORT.")

(define-repo-method unassign-sprint repo-unassign-sprint
 "Takes every ticket of SPRINT-ID out of it in REPO, leaving the tickets
themselves untouched.")

(define-repo-method unassign-milestone repo-unassign-milestone
 "Takes every ticket out of MILESTONE-ID in REPO, leaving the tickets
themselves untouched.")

(define (ticket-id-from-human-id repo project-id human-id)
  "Resolves the internal ticket ID from a given PROJECT-ID and HUMAN-ID in REPO."
  (let ((t (get-ticket-by-project-human-id-and-human-id repo project-id
                                                        human-id)))
    (assq-ref t
              'id)))
