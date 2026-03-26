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

(define-module (lucidplan persistence user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan persistence util)
  #:export (user-repository make-sqlite-user-repository user-repository?
                            get-id-to-username))

;; -- Record definition ---
(define-record-type user-repository
  (make-user-repository get-id-to-username-proc) user-repository?
  (get-id-to-username-proc repo-get-id-to-username))

;; --- DSL and wrappers ---
(define (get-id-to-username repo)
  (apply (repo-get-id-to-username repo)
         '()))

;; --- SQLite implementation ---
(define (make-sqlite-user-repository rc)
  (define user-columns
    '(id username email password salt permissions))

  (define (get-id-to-username)
    (let* ((query (format #f "SELECT ~a
                   FROM user
                   ORDER BY username ASC"
                          (symbols->sql-columns-list user-columns)))
           (_ (log-info "get-id-to-username query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'id)
                                     (assq-ref r
                                               'username))) rows))
           (_ (log-info "get-id-to-username mapped-rows: ~a\n"
                        (length mapped-rows))))
      mapped-rows))

  (make-user-repository get-id-to-username))
