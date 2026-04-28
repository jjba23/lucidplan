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

(define-module (lucidplan sqlite project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain project)
  #:export (make-sqlite-project-repository))

;; --- SQLite implementation ---
(define (make-sqlite-project-repository rc)
  (define columns
    '(id human-id
         title
         url
         vcs-url
         description
         created-at
         updated-at
         deleted-at))

  (define insert-columns
    '(id human-id
         title
         url
         vcs-url
         description
         created-at))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-projects)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM project WHERE deleted_at IS NULL
                   ORDER BY human_id ASC"
                   (symbols->sql-columns-list columns))))
      (execute-read "get-projects" query)))

  (define (search-projects q)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM project WHERE deleted_at IS NULL
                   AND (title LIKE '%~a%' OR description LIKE '%~a%')
                   ORDER BY human_id ASC"
                   (symbols->sql-columns-list columns) q q)))
      (execute-read "search-projects" query)))

  (define (get-project-by-human-id human-id)
    (let* ((query (format #f
                   "SELECT ~a FROM project
                    WHERE deleted_at IS NULL AND human_id = ~a
                    LIMIT 1"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal human-id)))
           (rows (execute-read "get-project-by-human-id" query)))
      (if (nil? rows) #f
          (car rows))))

  (define (update-project-by-human-id human-id attrs)
    (let* ((query (format #f "UPDATE project SET ~a WHERE human_id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal human-id))))
      (execute-write "update-project-by-human-id" query)))

  (define (insert-project attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO project (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", "))))
      (execute-write "insert-project" query)))

  (mk-project-repository #:get-projects-proc get-projects
                         #:search-projects-proc search-projects
                         #:get-project-by-human-id-proc
                         get-project-by-human-id
                         #:update-project-by-human-id-proc
                         update-project-by-human-id
                         #:insert-project-proc insert-project))
