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

(define-module (lucidplan sqlite ticket)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket)
  #:export (make-sqlite-ticket-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-repository rc)
  (define columns
    '(id human-id
         project-id
         title
         ticket-status
         ticket-type
         description
         created-at
         updated-at
         deleted-at
         priority))

  (define insert-columns
    '(id
         project-id
         title
         ticket-status
         ticket-type
         description
         created-at
         priority))

    (define conn (DB-open rc))
    
    (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-tickets project-id sort)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM ticket WHERE deleted_at IS NULL
                   AND project_id = (SELECT id FROM project WHERE human_id = ~a LIMIT 1)
                   ORDER BY ~a"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal project-id) sort))
           (_ (log-info "get-tickets query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-tickets rows: ~a\n"
                        (length rows))))
      rows))

  (define (search-tickets project-id q sort)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM ticket WHERE deleted_at IS NULL
                   AND project_id = (SELECT id FROM project WHERE human_id = ~a LIMIT 1)
                   AND (title LIKE '%~a%' OR description LIKE '%~a%')
                   ORDER BY ~a"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal project-id)
                   q
                   q
                   sort))
           (_ (log-info "search-tickets query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "search-tickets rows: ~a\n"
                        (length rows))))
      rows))

  (define (get-ticket-by-project-human-id-and-human-id project-id human-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket
                    WHERE deleted_at IS NULL AND project_id = (SELECT id FROM project WHERE human_id = ~a LIMIT 1)
                    AND human_id = ~a
                    LIMIT 1"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal project-id)
                   (value->sql-literal human-id)))
           (_ (log-info "get-ticket-by-project-human-id-and-human-id query:
	~a
"
                        query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-ticket-by-project-human-id-and-human-id rows: ~a
"
               (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (update-ticket-by-human-id human-id attrs)
    (let* ((query (format #f "UPDATE ticket SET ~a WHERE human_id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal human-id)))
           (_ (log-info "update-ticket-by-human-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-ticket attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO ticket (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-ticket query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-ticket-repository #:get-tickets-proc get-tickets
                        #:search-tickets-proc search-tickets
                        #:get-ticket-by-project-human-id-and-human-id-proc
                        get-ticket-by-project-human-id-and-human-id
                        #:update-ticket-by-human-id-proc
                        update-ticket-by-human-id
                        #:insert-ticket-proc insert-ticket))
