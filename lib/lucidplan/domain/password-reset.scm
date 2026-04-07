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

(define-module (lucidplan domain password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:export (<password-reset-repository> password-reset-repository?
                                         get-password-reset-by-token
                                         mk-password-reset-repository))

;; -- Record definition ---

(define-record-type <password-reset-repository>
  (make-password-reset-repository get-password-reset-by-token-proc)
  password-reset-repository?
  (password-reset-by-token-proc repo-get-password-reset-by-token))

;; --- Constructor (keyword arguments for ergonomics) ---

(define* (mk-password-reset-repository #:key get-password-reset-by-token-proc)
  "Creates a new password reset repository instance with the provided backend procedures."
  (make-password-reset-repository get-password-reset-by-token-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-password-reset-by-token repo token)
  "Retrieves the password reset associated with a specific TOKEN from the REPO."
  ((repo-get-password-reset-by-token repo)
   token))

