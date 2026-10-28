;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
         ;; The token naming one version of the picture this project wears, or
         ;; nothing -- in which case the pages draw the letter avatar its name
         ;; generates (see `(lucidplan avatars)').  The token is what a page
         ;; puts after the picture's address, so that a replaced picture is a
         ;; new address; the bytes themselves are *not* read here, because a
         ;; list of projects must not drag every picture in the instance
         ;; through memory to draw a column of names.  See
         ;; `get-project-avatar'.
         avatar
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
      (execute-read "search-projects"
                    (sql-parameterize (format #f
                                       "SELECT ~a FROM project WHERE deleted_at IS NULL AND (title LIKE ? OR description LIKE ?) ORDER BY human_id ASC"
                                       (quoted-sql-columns columns))
                                      (list pattern pattern)))))

  (define (get-project-by-human-id human-id)
    (let ((rows (execute-read "get-project-by-human-id"
                              (sql-select 'project
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'human-id
                                                              human-id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  ;; The picture of one project: the bytes and the type they are served as,
  ;; and nothing else.  It is a query of its own rather than a column of the
  ;; one above for the reason written there -- a picture is for one row and
  ;; for one request, not for every list of every project.
  (define (get-project-avatar human-id)
    (let ((rows (execute-read "get-project-avatar"
                              (sql-select 'project
                                          #:columns '(avatar avatar-content-type
                                                             avatar-data)
                                          #:where (list '(deleted-at is null)
                                                        (cons 'human-id
                                                              human-id))
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
                         #:get-project-avatar-proc
                         get-project-avatar
                         #:update-project-by-human-id-proc
                         update-project-by-human-id
                         #:insert-project-proc insert-project))
