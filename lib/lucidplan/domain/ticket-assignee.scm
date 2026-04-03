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

(define-module (lucidplan domain ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:export (ticket-assignee-repository ticket-assignee-repository?
                                       get-ticket-assignee-by-project-id
                                       get-ticket-assignee-by-ticket-id
                                       mk-ticket-assignee-repository))

;; -- Record definition ---
(define-record-type ticket-assignee-repository
  (make-ticket-assignee-repository get-ticket-assignee-by-project-id-proc
                                   get-ticket-assignee-by-ticket-id-proc)
  ticket-assignee-repository?
  (get-ticket-assignee-by-project-id-proc
   repo-get-ticket-assignee-by-project-id)
  (get-ticket-assignee-by-ticket-id-proc repo-get-ticket-assignee-by-ticket-id))

(define* (mk-user-repository #:key get-ticket-assignee-by-project-id-proc
                             get-ticket-assignee-by-ticket-id-proc)
  (make-ticket-assignee-repository get-ticket-assignee-by-project-id-proc
                                   get-ticket-assignee-by-ticket-id-proc))

;; --- eDSL and wrappers ---
(define (get-ticket-assignee-by-project-id repo project-id)
  (apply (repo-get-ticket-assignee-by-project-id repo)
         (list project-id)))

(define (get-ticket-assignee-by-ticket-id repo ticket-id)
  (apply (repo-get-ticket-assignee-by-ticket-id repo)
         (list ticket-id)))
