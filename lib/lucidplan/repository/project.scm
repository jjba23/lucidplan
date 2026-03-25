;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>

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

(define-module (lucidplan repository project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan repository util)
  #:export (project-repository make-sqlite-project-repository
                               project-repository? get-active-projects
                               get-active-project-by-human-id))

;; -- Record definition ---
(define-record-type project-repository
  (make-project-repository get-active-projects-proc
                           get-active-project-by-human-id-proc)
  project-repository?
  (get-active-projects-proc repo-get-active-projects)
  (get-active-project-by-human-id-proc repo-get-active-project-by-human-id))

;; --- DSL and wrappers ---
(define (get-active-projects repo)
  (apply (repo-get-active-projects repo)
         '()))

(define (get-active-project-by-human-id repo human-id)
  (apply (repo-get-active-project-by-human-id repo)
         (list human-id)))

;; --- SQLite implementation ---
(define (make-sqlite-project-repository rc)
  (define project-columns
    (to-column-string '(id human-id
                           title
                           url
                           vcs-url
                           description
                           created-at
                           updated-at
                           deleted-at)))

  (define (get-active-projects)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM project WHERE deleted_at IS NULL
                   ORDER BY human_id ASC"
                   project-columns))
           (_ (log-info "get-active-projects query:\n\t~a\n" query))
           (rows (map db-row-sexp (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-projects rows:\n~a\n" rows)))
      rows))

  (define (get-active-project-by-human-id human-id)
    (let* ((query (format #f
                   "SELECT ~a
                   FROM project WHERE deleted_at IS NULL AND human_id = '~a'
                   LIMIT 1"
                   project-columns human-id))
           (_ (log-info "get-active-project-by-human-id query:
	~a
" query))
           (rows (map db-row-sexp (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-active-project-by-human-id row:\n~a\n"
                        (car rows))))
      (if (nil? rows) #f
          (car rows))))

  (make-project-repository get-active-projects get-active-project-by-human-id))
