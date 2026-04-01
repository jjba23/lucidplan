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

(define-module (lucidplan domain project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:export (project-repository project-repository?
                               get-active-projects
                               search-active-projects
                               get-active-project-by-human-id
                               update-project-by-human-id
                               mk-project-repository
                               insert-project))


;; -- Record definition ---
(define-record-type project-repository
  (make-project-repository get-active-projects-proc
                           search-active-projects-proc
                           get-active-project-by-human-id-proc
                           update-project-by-human-id-proc insert-project-proc)
  project-repository?
  (get-active-projects-proc repo-get-active-projects)
  (search-active-projects-proc repo-search-active-projects)
  (get-active-project-by-human-id-proc repo-get-active-project-by-human-id)
  (update-project-by-human-id-proc repo-update-project-by-human-id)
  (insert-project-proc repo-insert-project))

(define* (mk-project-repository #:key get-active-projects-proc
                           search-active-projects-proc
                           get-active-project-by-human-id-proc
                           update-project-by-human-id-proc insert-project-proc)
  (make-project-repository get-active-projects-proc
                           search-active-projects-proc
                           get-active-project-by-human-id-proc
                           update-project-by-human-id-proc insert-project-proc))
;; --- eDSL and wrappers ---
(define (get-active-projects repo)
  (apply (repo-get-active-projects repo)
         '()))

(define (search-active-projects repo q)
  (apply (repo-search-active-projects repo)
         (list q)))

(define (get-active-project-by-human-id repo human-id)
  (apply (repo-get-active-project-by-human-id repo)
         (list human-id)))

(define (update-project-by-human-id repo human-id attrs)
  (apply (repo-update-project-by-human-id repo)
         (list human-id attrs)))

(define (insert-project repo attrs)
  (apply (repo-insert-project repo)
         (list attrs)))
