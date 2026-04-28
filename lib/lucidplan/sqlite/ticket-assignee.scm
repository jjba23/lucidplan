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

(define-module (lucidplan sqlite ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-assignee)
  #:export (make-sqlite-ticket-assignee-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-assignee-repository rc)
  (define columns
    '(project-id ticket-id user-id))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-ticket-assignee-by-project-human-id project-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket_assignee WHERE project_id =
                        (SELECT id FROM project WHERE human_id = ~a LIMIT 1)"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal project-id))))
      (execute-read "get-ticket-assignee-by-project-human-id" query)))

  (define (get-ticket-assignee-by-ticket-id ticket-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket_assignee WHERE ticket_id = ~a LIMIT 1"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal ticket-id)))
           (rows (execute-read "get-ticket-assignee-by-ticket-id" query)))
      (if (nil? rows) #f
          (car rows))))

  (define (upsert-ticket-assignee attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x))) columns))
           (query (format #f
                   "INSERT INTO ticket_assignee (~a)
                        VALUES (~a) ON CONFLICT(ticket_id) DO UPDATE SET user_id = excluded.user_id"
                   (symbols->sql-columns-list columns)
                   (string-join vals ", "))))
      (execute-write "upsert-ticket-assignee" query)))

  (mk-ticket-assignee-repository
                                 #:get-ticket-assignee-by-project-human-id-proc
                                 get-ticket-assignee-by-project-human-id
                                 #:get-ticket-assignee-by-ticket-id-proc
                                 get-ticket-assignee-by-ticket-id
                                 #:upsert-ticket-assignee-proc
                                 upsert-ticket-assignee))
