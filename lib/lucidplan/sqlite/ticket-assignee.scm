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

(define-module (lucidplan sqlite ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-assignee)
  #:export (make-sqlite-ticket-assignee-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-assignee-repository rc)
  (define ticket-assignee-columns
    '(project-id ticket-id user-id))

  (define ticket-assignee-upsert-columns
    '(project-id ticket-id user-id))

  (define (get-ticket-assignee-by-project-id project-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket_assignee WHERE project_id = ~a"
                   (symbols->sql-columns-list ticket-assignee-columns)
                   (value->sql-literal project-id)))
           (_ (log-info "get-ticket-assignee-by-project-id query:
	~a
" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'ticket-id)
                                     (assq-ref r
                                               'user-id))) rows))
           (_ (log-info "get-ticket-assignee-by-project-id mapped-rows: ~a
"
                        (length mapped-rows))))
      mapped-rows))

  (define (get-ticket-assignee-by-ticket-id ticket-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket_assignee WHERE ticket_id = ~a LIMIT 1"
                   (symbols->sql-columns-list ticket-assignee-columns)
                   (value->sql-literal ticket-id)))
           (_ (log-info "get-ticket-assignee-by-ticket-id query:
	~a
" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'ticket-id)
                                     (assq-ref r
                                               'user-id))) rows))
           (_ (log-info "get-ticket-assignee-by-ticket-id mapped-rows: ~a
"
                        (length mapped-rows))))
      (if (null? mapped-rows) #f
          (car mapped-rows))))

  (define (upsert-ticket-assignee attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      ticket-assignee-upsert-columns))
           (query (format #f
                   "INSERT INTO ticket_assignee (~a)
                        VALUES (~a) ON CONFLICT(ticket_id) DO UPDATE SET user_id = excluded.user_id"
                   (symbols->sql-columns-list ticket-assignee-upsert-columns)
                   (string-join vals ", ")))
           (_ (log-info "upsert-ticket-assignee query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-ticket-assignee-repository #:get-ticket-assignee-by-project-id-proc
                                 get-ticket-assignee-by-project-id
                                 #:get-ticket-assignee-by-ticket-id-proc
                                 get-ticket-assignee-by-ticket-id
                                 #:upsert-ticket-assignee-proc
                                 upsert-ticket-assignee))
