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

(define-module (lucidplan sqlite ticket)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
    #:use-module (lucidplan domain ticket)
  #:export ( make-sqlite-ticket-repository
                              ))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-repository rc)
  (define ticket-columns
    '(id human-id
         project-id         
         title
         ticket-status
         ticket-type
         description
         created-at
         updated-at
         deleted-at
         priority
         ))

  (define ticket-insert-columns
    '(id human-id
         project-id         
         title
         ticket-status
         ticket-type
         description
         created-at
         priority))

  (define (get-active-tickets project-id sort)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM ticket WHERE deleted_at IS NULL
                   AND project_id = '~a'
                   ORDER BY ~a"
                   (symbols->sql-columns-list ticket-columns) project-id sort))
           (_ (log-info "get-active-tickets query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-tickets rows: ~a\n"
                        (length rows))))
      rows))

  (define (search-active-tickets project-id q sort)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM ticket WHERE deleted_at IS NULL
                   AND project_id = '~a'
                   AND (title LIKE '%~a%' OR description LIKE '%~a%')
                   ORDER BY ~a"
                   (symbols->sql-columns-list ticket-columns) project-id q q sort))
           (_ (log-info "search-active-tickets query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "search-active-tickets rows: ~a\n"
                        (length rows))))
      rows))

  (define (get-active-ticket-by-project-id-and-human-id project-id human-id)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket
                    WHERE deleted_at IS NULL AND project_id = '~a' AND human_id = '~a'
                    LIMIT 1"
                   (symbols->sql-columns-list ticket-columns) project-id human-id))
           (_ (log-info "get-active-ticket-by-project-id-and-human-id query:
	~a
" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-ticket-by-project-id-and-human-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (update-ticket-by-human-id human-id attrs)
    (let* ((query (format #f "UPDATE ticket SET ~a WHERE human_id = '~a'"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          human-id))
           (_ (log-info "update-ticket-by-human-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-ticket attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      ticket-insert-columns))
           (query (format #f "INSERT INTO ticket (~a) VALUES (~a)"
                          (symbols->sql-columns-list ticket-insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-ticket query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (make-ticket-repository get-active-tickets search-active-tickets
                           get-active-ticket-by-project-id-and-human-id
                           update-ticket-by-human-id insert-ticket))
