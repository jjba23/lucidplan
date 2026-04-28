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
  (define columns
    '(id username email password salt permissions))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-id-to-username)
    (let* ((query "SELECT id, username FROM user
                   ORDER BY username ASC"))
      (execute-read "get-id-to-username" query)))

  (define (get-user-by-email email)
    (let* ((query (format #f "SELECT ~a FROM user WHERE email = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal email)))
           (rows (execute-read "get-user-by-email" query)))
      (if (nil? rows) #f
          (car rows))))

  (define (get-user-by-id id)
    (let* ((query (format #f "SELECT ~a FROM user WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (rows (execute-read "get-user-by-id" query)))
      (if (nil? rows) #f
          (car rows))))

  (define (get-users)
    (let* ((query (format #f "SELECT ~a FROM user
                   ORDER BY username ASC"
                          (symbols->sql-columns-list columns))))
      (execute-read "get-users" query)))

  (define (update-user-by-id id attrs)
    (let* ((query (format #f "UPDATE user SET ~a WHERE id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal id))))
      (execute-write "update-user-by-id" query)))

  (define (delete-user-by-id id)
    (let* ((query (format #f "DELETE FROM user WHERE id = ~a"
                          (value->sql-literal id))))
      (execute-write "delete-user-by-id" query)))

  (define (insert-user attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x))) columns))
           (query (format #f "INSERT INTO user (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", "))))
      (execute-write "insert-user" query)))

  (mk-user-repository #:get-id-to-username-proc get-id-to-username
                      #:get-user-by-email-proc get-user-by-email
                      #:get-user-by-id-proc get-user-by-id
                      #:get-users-proc get-users
                      #:insert-user-proc insert-user
                      #:delete-user-by-id-proc delete-user-by-id
                      #:update-user-by-id-proc update-user-by-id))
