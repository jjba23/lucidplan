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

(define-module (lucidplan persistence project)
  #:use-module (srfi srfi-9)
  #:use-module (artanis db)
  #:export (project-repository make-project-repository
                               make-sqlite-project-repository
                               project-repository? get-all-active-projects))

(define-record-type project-repository
  (make-project-repository repo-get-all-active-projects) project-repository?
  (repo-get-all-active-projects get-all-active-projects))

(define (make-sqlite-project-repository rc)
  (define (sqlite-get-all-active-projects rc)
    (lambda ()
      (DB-get-all-rows (DB-query (DB-open rc)
                        "SELECT id, human_id, title, url, vcs_url, description,
                       created_at, updated_at, deleted_at
                       FROM project WHERE deleted_at IS NULL ORDER BY human_id ASC"))))
  (make-project-repository (sqlite-get-all-active-projects rc)))


