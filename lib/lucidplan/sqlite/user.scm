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

(define-module (lucidplan sqlite user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain user)
  #:export (make-sqlite-user-repository))

;; --- SQLite implementation ---
(define (make-sqlite-user-repository rc)
  (define user-columns
    '(id username email password salt permissions))

  (define (get-id-to-username)
    (let* ((query (format #f "SELECT ~a FROM user
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

  (define (get-user-by-email email)
    (let* ((query (format #f "SELECT ~a FROM user WHERE email = ~a LIMIT 1"
                          (symbols->sql-columns-list user-columns)
                          (value->sql-literal email)))
           (_ (log-info "get-user-by-email query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-user-by-email rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (mk-user-repository #:get-id-to-username-proc get-id-to-username
                      #:get-user-by-email-proc get-user-by-email))
