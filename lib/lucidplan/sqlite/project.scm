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
         ;; The statuses this project's own tickets move through, as the text
         ;; its edit page was given, or nothing -- in which case the project
         ;; uses the instance's statuses.  See `effective-ticket-statuses'.
         ticket-statuses
         created-at
         updated-at
         deleted-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-projects)
    (execute-read "get-projects"
                  (sql-select 'project
                              #:columns columns
                              #:where '((deleted-at is null))
                              #:order-by '((human-id asc)))))

  (define (search-projects q)
    ;; A search matches the title *or* the description, and the builders join
    ;; their conditions with AND only, so this one statement is written as a
    ;; template and its two matched patterns are bound with `sql-parameterize'.
    (let ((pattern (format #f "%~a%" q)))
      (execute-read
       "search-projects"
       (sql-parameterize
        (format #f
                "SELECT ~a FROM project WHERE deleted_at IS NULL AND (title LIKE ? OR description LIKE ?) ORDER BY human_id ASC"
                (quoted-sql-columns columns))
        (list pattern pattern)))))

  (define (get-project-by-human-id human-id)
    (let ((rows (execute-read
                 "get-project-by-human-id"
                 (sql-select 'project
                             #:columns columns
                             #:where (list '(deleted-at is null)
                                           (cons 'human-id human-id))
                             #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (update-project-by-human-id human-id attrs)
    (execute-write "update-project-by-human-id"
                   (sql-update 'project attrs
                               #:where (list (cons 'human-id human-id)))))

  (define (insert-project attrs)
    (execute-write "insert-project"
                   (sql-insert 'project attrs)))

  (mk-project-repository #:get-projects-proc get-projects
                         #:search-projects-proc search-projects
                         #:get-project-by-human-id-proc
                         get-project-by-human-id
                         #:update-project-by-human-id-proc
                         update-project-by-human-id
                         #:insert-project-proc insert-project))
