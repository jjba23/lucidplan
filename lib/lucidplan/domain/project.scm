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

(define-module (lucidplan domain project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-projects search-projects get-project-by-human-id
                         update-project-by-human-id insert-project
                         project-id-from-human-id))

;; -- Record definition ---

(define-record-with-kw (<project-repository> %make-project-repository
                                             project-repository?)
                       mk-project-repository
                       (get-projects-proc repo-get-projects)
                       (search-projects-proc repo-search-projects)
                       (get-project-by-human-id-proc
                        repo-get-project-by-human-id)
                       (update-project-by-human-id-proc
                        repo-update-project-by-human-id)
                       (insert-project-proc repo-insert-project))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-projects repo-get-projects
 "Retrieves a list of all active projects from the given REPO.")

(define-repo-method search-projects repo-search-projects
 "Searches for active projects matching query Q from the given REPO.")

(define-repo-method get-project-by-human-id repo-get-project-by-human-id
 "Retrieves a specific active project matching HUMAN-ID from the REPO.")

(define-repo-method update-project-by-human-id repo-update-project-by-human-id
 "Updates a project identified by HUMAN-ID with ATTRS in the given REPO.")

(define-repo-method insert-project repo-insert-project
                    "Inserts a new project using ATTRS into the given REPO.")

(define (project-id-from-human-id repo human-id)
  "Resolves the internal project ID from a given HUMAN-ID in the REPO."
  (let ((p (get-project-by-human-id repo human-id)))
    (assq-ref p
              'id)))
