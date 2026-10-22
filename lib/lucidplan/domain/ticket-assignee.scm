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

(define-module (lucidplan domain ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-assignee-by-project-human-id
            get-ticket-assignee-by-ticket-id upsert-ticket-assignee
            unassign-ticket-assignee move-ticket-assignee))

;; -- Record definition ---

(define-record-with-kw (<ticket-assignee-repository>
                        %make-ticket-assignee-repository
                        ticket-assignee-repository?)
                       mk-ticket-assignee-repository
                       (get-ticket-assignee-by-project-human-id-proc
                        repo-get-ticket-assignee-by-project-human-id)
                       (get-ticket-assignee-by-ticket-id-proc
                        repo-get-ticket-assignee-by-ticket-id)
                       (upsert-ticket-assignee-proc
                        repo-upsert-ticket-assignee)
                       (unassign-ticket-assignee-proc
                        repo-unassign-ticket-assignee)
                       (move-ticket-assignee-proc repo-move-ticket-assignee))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-ticket-assignee-by-project-human-id
 repo-get-ticket-assignee-by-project-human-id
 "Retrieves the ticket assignees associated with a specific PROJECT-ID from the REPO.")

(define-repo-method get-ticket-assignee-by-ticket-id
 repo-get-ticket-assignee-by-ticket-id
 "Retrieves the assignee details for a specific TICKET-ID from the REPO.")

(define-repo-method upsert-ticket-assignee repo-upsert-ticket-assignee
 "Inserts or updates a ticket assignee record using the provided ATTRS alist in the REPO.")

(define-repo-method unassign-ticket-assignee repo-unassign-ticket-assignee
 "Removes the assignee record of a TICKET-ID from the REPO, leaving the ticket
unassigned.  Sending a ticket back to nobody is the row's absence rather than
a row naming an empty user, so the clearing of an assignment is stated rather
than left to whichever value a form submits.")

(define-repo-method move-ticket-assignee repo-move-ticket-assignee
 "Rewrites the PROJECT-ID of the assignment of TICKET-ID in REPO.

An assignment names the project it was made in as well as the ticket, so a
ticket moved to another project takes its assignee row with it.")

