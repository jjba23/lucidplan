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

(define-module (lucidplan domain ticket)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-tickets search-tickets
                        get-ticket-by-project-human-id-and-human-id
                        get-ticket-of-latest-human-id
                        update-ticket-by-project-human-id-and-human-id
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

(define-repo-method insert-ticket repo-insert-ticket
                    "Inserts a new ticket using ATTRS into the given REPO.")

(define (ticket-id-from-human-id repo project-id human-id)
  "Resolves the internal ticket ID from a given PROJECT-ID and HUMAN-ID in REPO."
  (let ((t (get-ticket-by-project-human-id-and-human-id repo project-id
                                                        human-id)))
    (assq-ref t
              'id)))
