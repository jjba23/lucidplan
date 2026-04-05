;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

;; Copyright (C) Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (kracht prelude)
  #:export (<project-repository> project-repository?
                                 get-active-projects
                                 search-active-projects
                                 get-active-project-by-human-id
                                 update-project-by-human-id
                                 mk-project-repository
                                 insert-project
                                 project-id-from-human-id))

;; -- Record definition ---

(define-record-type <project-repository>
  (make-project-repository active-projects-proc search-active-projects-proc
                           active-project-by-human-id-proc
                           update-project-by-human-id-proc insert-project-proc)
  project-repository?
  (active-projects-proc repo-get-active-projects)
  (search-active-projects-proc repo-search-active-projects)
  (active-project-by-human-id-proc repo-get-active-project-by-human-id)
  (update-project-by-human-id-proc repo-update-project-by-human-id)
  (insert-project-proc repo-insert-project))

;; --- Constructor (keyword arguments for ergonomics) ---

(define* (mk-project-repository #:key get-active-projects-proc
                                search-active-projects-proc
                                get-active-project-by-human-id-proc
                                update-project-by-human-id-proc
                                insert-project-proc)
  "Creates a new project repository instance with the provided backend procedures."
  (make-project-repository get-active-projects-proc
                           search-active-projects-proc
                           get-active-project-by-human-id-proc
                           update-project-by-human-id-proc insert-project-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-active-projects repo)
  "Retrieves a list of all active projects from the given REPO."
  ((repo-get-active-projects repo)))

(define (search-active-projects repo q)
  "Searches for active projects matching query Q from the given REPO."
  ((repo-search-active-projects repo)
   q))

(define (get-active-project-by-human-id repo human-id)
  "Retrieves a specific active project matching HUMAN-ID from the REPO."
  ((repo-get-active-project-by-human-id repo)
   human-id))

(define (update-project-by-human-id repo human-id attrs)
  "Updates a project identified by HUMAN-ID with ATTRS in the given REPO."
  ((repo-update-project-by-human-id repo)
   human-id attrs))

(define (insert-project repo attrs)
  "Inserts a new project using ATTRS into the given REPO."
  ((repo-insert-project repo)
   attrs))

(define (project-id-from-human-id repo human-id)
  "Resolves the internal project ID from a given HUMAN-ID in the REPO."
  (let ((p (get-active-project-by-human-id repo human-id)))
    (assq-ref p
              'id)))
