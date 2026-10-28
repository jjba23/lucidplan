;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan domain project)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-projects search-projects get-project-by-human-id
                         get-project-avatar
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
                       (get-project-avatar-proc repo-get-project-avatar)
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

(define-repo-method get-project-avatar repo-get-project-avatar
 "Retrieves the picture a specific active project matching HUMAN-ID wears from
the REPO: the bytes of one version of it and the type they are served as.")

(define-repo-method update-project-by-human-id repo-update-project-by-human-id
 "Updates a project identified by HUMAN-ID with ATTRS in the given REPO.")

(define-repo-method insert-project repo-insert-project
                    "Inserts a new project using ATTRS into the given REPO.")

(define (project-id-from-human-id repo human-id)
  "Resolves the internal project ID from a given HUMAN-ID in the REPO."
  (let ((p (get-project-by-human-id repo human-id)))
    (assq-ref p
              'id)))
