;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

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
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:export (ticket-repository ticket-repository?
                              get-active-tickets
                              search-active-tickets
                              get-active-ticket-by-project-id-and-human-id
                              update-ticket-by-human-id
                              mk-ticket-repository
                              insert-ticket))

;; -- Record definition ---
(define-record-type ticket-repository
  (make-ticket-repository get-active-tickets-proc search-active-tickets-proc
                          get-active-ticket-by-project-id-and-human-id-proc
                          update-ticket-by-human-id-proc insert-ticket-proc)
  ticket-repository?
  (get-active-tickets-proc repo-get-active-tickets)
  (search-active-tickets-proc repo-search-active-tickets)
  (get-active-ticket-by-project-id-and-human-id-proc
   repo-get-active-ticket-by-project-id-and-human-id)
  (update-ticket-by-human-id-proc repo-update-ticket-by-human-id)
  (insert-ticket-proc repo-insert-ticket))

(define* (mk-ticket-repository #:key get-active-tickets-proc
         search-active-tickets-proc
         get-active-ticket-by-project-id-and-human-id-proc
         update-ticket-by-human-id-proc insert-ticket-proc)
  (make-ticket-repository get-active-tickets-proc search-active-tickets-proc
                          get-active-ticket-by-project-id-and-human-id-proc
                          update-ticket-by-human-id-proc insert-ticket-proc))

;; --- eDSL and wrappers ---
(define (get-active-tickets repo project-id sort)
  (apply (repo-get-active-tickets repo)
         (list project-id sort)))

(define (search-active-tickets repo project-id q sort)
  (apply (repo-search-active-tickets repo)
         (list project-id q sort)))

(define (get-active-ticket-by-project-id-and-human-id repo project-id human-id)
  (apply (repo-get-active-ticket-by-project-id-and-human-id repo)
         (list project-id human-id)))

(define (update-ticket-by-human-id repo human-id attrs)
  (apply (repo-update-ticket-by-human-id repo)
         (list human-id attrs)))

(define (insert-ticket repo attrs)
  (apply (repo-insert-ticket repo)
         (list attrs)))

