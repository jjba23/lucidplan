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

(define-module (lucidplan domain user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:export (<user-repository> user-repository?
                              mk-user-repository
                              get-id-to-username
                              get-user-by-id
                              get-active-users
                              get-user-by-email))

;; -- Record definition ---

(define-record-type <user-repository>
  (make-user-repository id-to-username-proc user-by-email-proc user-by-id-proc
                        active-users-proc)
  user-repository?
  (id-to-username-proc repo-get-id-to-username)
  (user-by-email-proc repo-get-user-by-email)
  (user-by-id-proc repo-get-user-by-id)
  (active-users-proc repo-get-active-users))

;; --- Constructor (keyword arguments for ergonomics) ---
(define* (mk-user-repository #:key get-id-to-username-proc
                             get-user-by-email-proc get-user-by-id-proc
                             get-active-users-proc)
  "Creates a new user repository instance with the provided backend procedures."
  (make-user-repository get-id-to-username-proc get-user-by-email-proc
                        get-user-by-id-proc get-active-users-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-id-to-username repo)
  "Retrieves a mapping of user IDs to usernames from the given REPO."
  ((repo-get-id-to-username repo)))

(define (get-user-by-email repo email)
  "Retrieves a user by their EMAIL string from the given REPO."
  ((repo-get-user-by-email repo)
   email))

(define (get-user-by-id repo id)
  "Retrieves a user by their ID from the given REPO."
  ((repo-get-user-by-id repo)
   id))

(define (get-active-users repo)
  "Retrieves a list of all active users from the given REPO."
  ((repo-get-active-users repo)))
