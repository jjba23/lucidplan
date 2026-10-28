;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite epic)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain epic)
  #:export (make-sqlite-epic-repository))

;; --- SQLite implementation ---
(define (make-sqlite-epic-repository rc)
  (define columns
    '(id project-id
         human-id
         title
         description
         created-at
         updated-at
         deleted-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-epics project-id)
    (execute-read "get-epics"
                  (sql-select 'epic
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'project-id project-id))
                              #:order-by '((title asc)))))

  (define (get-epic-by-id id)
    (let ((rows (execute-read "get-epic-by-id"
                              (sql-select 'epic
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-epic-by-project-and-human-id project-id human-id)
    (let ((rows (execute-read "get-epic-by-project-and-human-id"
                              (sql-select 'epic
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'project-id
                                                              project-id)
                                                        (cons 'human-id
                                                              human-id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-epic attrs)
    (execute-write "insert-epic"
                   (sql-insert 'epic attrs)))

  (define (update-epic-by-id id attrs)
    (execute-write "update-epic-by-id"
                   (sql-update 'epic attrs
                               #:where (list (cons 'id id)))))

  (define (soft-delete-epic id)
    (execute-write "soft-delete-epic"
                   (sql-update 'epic
                               `((deleted-at unquote
                                             (current-time))
                                 (updated-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (mk-epic-repository #:get-epics-proc get-epics
                      #:get-epic-by-id-proc get-epic-by-id
                      #:get-epic-by-project-and-human-id-proc
                      get-epic-by-project-and-human-id
                      #:insert-epic-proc insert-epic
                      #:update-epic-by-id-proc update-epic-by-id
                      #:soft-delete-epic-proc soft-delete-epic))
