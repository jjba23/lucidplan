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

(define-module (lucidplan sqlite project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain project)
  #:export (make-sqlite-project-repository))

;; --- SQLite implementation ---
(define (make-sqlite-project-repository rc)
  (define project-columns
    '(id human-id
         title
         url
         vcs-url
         description
         created-at
         updated-at
         deleted-at))

  (define project-insert-columns
    '(id human-id
         title
         url
         vcs-url
         description
         created-at))

  (define (get-active-projects)
    (let* ((query (format #f
                   "SELECT ~a\n                   FROM project WHERE deleted_at IS NULL\n                   ORDER BY human_id ASC"
                   (symbols->sql-columns-list project-columns)))
           (_ (log-info "get-active-projects query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-projects rows: ~a\n"
                        (length rows))))
      rows))

  (define (search-active-projects q)
    (let* ((query (format #f
                   "SELECT ~a\n                   FROM project WHERE deleted_at IS NULL\n                   AND (title LIKE '%~a%' OR description LIKE '%~a%')\n                   ORDER BY human_id ASC"
                   (symbols->sql-columns-list project-columns) q q))
           (_ (log-info "search-active-projects query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "search-active-projects rows: ~a\n"
                        (length rows))))
      rows))

  (define (get-active-project-by-human-id human-id)
    (let* ((query (format #f
                   "SELECT ~a FROM project\n                    WHERE deleted_at IS NULL AND human_id = '~a'\n                    LIMIT 1"
                   (symbols->sql-columns-list project-columns) human-id))
           (_ (log-info "get-active-project-by-human-id query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-project-by-human-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (update-project-by-human-id human-id attrs)
    (let* ((query (format #f "UPDATE project SET ~a WHERE human_id = '~a'"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          human-id))
           (_ (log-info "update-project-by-human-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-project attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      project-insert-columns))
           (query (format #f "INSERT INTO project (~a) VALUES (~a)"
                          (symbols->sql-columns-list project-insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-project query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-project-repository #:get-active-projects-proc get-active-projects
                         #:search-active-projects-proc search-active-projects
                         #:get-active-project-by-human-id-proc
                         get-active-project-by-human-id
                         #:update-project-by-human-id-proc
                         update-project-by-human-id
                         #:insert-project-proc insert-project))
