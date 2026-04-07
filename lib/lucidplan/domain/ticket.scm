;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

;; Copyright (C) Josep Bigorra <jjbigorra@gmail.com>

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
  #:export (<ticket-repository> ticket-repository?
                                get-tickets
                                search-tickets
                                get-ticket-by-project-id-and-human-id
                                update-ticket-by-human-id
                                mk-ticket-repository
                                ticket-id-from-human-id
                                insert-ticket))

;; -- Record definition ---

(define-record-type <ticket-repository>
  (make-ticket-repository active-tickets-proc search-tickets-proc
                          active-ticket-by-project-id-and-human-id-proc
                          update-ticket-by-human-id-proc insert-ticket-proc)
  ticket-repository?
  (active-tickets-proc repo-get-tickets)
  (search-tickets-proc repo-search-tickets)
  (active-ticket-by-project-id-and-human-id-proc
   repo-get-ticket-by-project-id-and-human-id)
  (update-ticket-by-human-id-proc repo-update-ticket-by-human-id)
  (insert-ticket-proc repo-insert-ticket))

;; --- Constructor (keyword arguments for ergonomics) ---

(define* (mk-ticket-repository #:key get-tickets-proc
          search-tickets-proc
          get-ticket-by-project-id-and-human-id-proc
          update-ticket-by-human-id-proc
          insert-ticket-proc)
  "Creates a new ticket repository instance with the provided backend procedures."
  (make-ticket-repository get-tickets-proc search-tickets-proc
                          get-ticket-by-project-id-and-human-id-proc
                          update-ticket-by-human-id-proc insert-ticket-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-tickets repo project-id sort)
  "Retrieves active tickets for a given PROJECT-ID, ordered by SORT, from REPO."
  ((repo-get-tickets repo)
   project-id sort))

(define (search-tickets repo project-id q sort)
  "Searches active tickets in PROJECT-ID matching query Q, ordered by SORT, from REPO."
  ((repo-search-tickets repo)
   project-id q sort))

(define (get-ticket-by-project-id-and-human-id repo project-id human-id)
  "Retrieves a specific active ticket matching PROJECT-ID and HUMAN-ID from REPO."
  ((repo-get-ticket-by-project-id-and-human-id repo)
   project-id human-id))

(define (update-ticket-by-human-id repo human-id attrs)
  "Updates a ticket identified by HUMAN-ID with ATTRS in the given REPO."
  ((repo-update-ticket-by-human-id repo)
   human-id attrs))

(define (insert-ticket repo attrs)
  "Inserts a new ticket using ATTRS into the given REPO."
  ((repo-insert-ticket repo)
   attrs))

(define (ticket-id-from-human-id repo project-id human-id)
  "Resolves the internal ticket ID from a given PROJECT-ID and HUMAN-ID in REPO."
  ;; Switched let* to let since there is only a single binding
  (let ((t (get-ticket-by-project-id-and-human-id repo project-id
                                                         human-id)))
    (assq-ref t
              'id)))
