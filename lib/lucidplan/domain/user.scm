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

(define-module (lucidplan domain user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:export (user-repository user-repository? get-id-to-username
                            get-user-by-email mk-user-repository))

;; -- Record definition ---
(define-record-type user-repository
  (make-user-repository get-id-to-username-proc get-user-by-email-proc)
  user-repository?
  (get-id-to-username-proc repo-get-id-to-username)
  (get-user-by-email-proc repo-get-user-by-email))

(define* (mk-user-repository #:key get-id-to-username-proc
                             get-user-by-email-proc)
  (make-user-repository get-id-to-username-proc get-user-by-email-proc))

;; --- eDSL and wrappers ---
(define (get-id-to-username repo)
  (apply (repo-get-id-to-username repo)
         '()))

(define (get-user-by-email repo email)
  (apply (repo-get-user-by-email repo)
         (list email)))
