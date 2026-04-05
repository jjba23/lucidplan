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

(define-module (lucidplan domain ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:export (<ticket-assignee-repository> ticket-assignee-repository?
                                         get-ticket-assignee-by-project-id
                                         get-ticket-assignee-by-ticket-id
                                         upsert-ticket-assignee
                                         mk-ticket-assignee-repository))

;; -- Record definition ---

(define-record-type <ticket-assignee-repository>
  (make-ticket-assignee-repository ticket-assignee-by-project-id-proc
                                   ticket-assignee-by-ticket-id-proc
                                   upsert-ticket-assignee-proc)
  ticket-assignee-repository?
  (ticket-assignee-by-project-id-proc repo-get-ticket-assignee-by-project-id)
  (ticket-assignee-by-ticket-id-proc repo-get-ticket-assignee-by-ticket-id)
  (upsert-ticket-assignee-proc repo-upsert-ticket-assignee))

;; --- Constructor (keyword arguments for ergonomics) ---

(define* (mk-ticket-assignee-repository #:key
          get-ticket-assignee-by-project-id-proc
          get-ticket-assignee-by-ticket-id-proc upsert-ticket-assignee-proc)
  "Creates a new ticket assignee repository instance with the provided backend procedures."
  (make-ticket-assignee-repository get-ticket-assignee-by-project-id-proc
                                   get-ticket-assignee-by-ticket-id-proc
                                   upsert-ticket-assignee-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-ticket-assignee-by-project-id repo project-id)
  "Retrieves the ticket assignees associated with a specific PROJECT-ID from the REPO."
  ((repo-get-ticket-assignee-by-project-id repo)
   project-id))

(define (get-ticket-assignee-by-ticket-id repo ticket-id)
  "Retrieves the assignee details for a specific TICKET-ID from the REPO."
  ((repo-get-ticket-assignee-by-ticket-id repo)
   ticket-id))

(define (upsert-ticket-assignee repo attrs)
  "Inserts or updates a ticket assignee record using the provided ATTRS alist in the REPO."
  ((repo-upsert-ticket-assignee repo)
   attrs))
