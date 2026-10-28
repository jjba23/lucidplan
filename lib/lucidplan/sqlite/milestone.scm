;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite milestone)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain milestone)
  #:export (make-sqlite-milestone-repository))

;; --- SQLite implementation ---
(define (make-sqlite-milestone-repository rc)
  (define columns
    '(id project-id
         title
         description
         due-on
         created-at
         updated-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-milestones project-id)
    (execute-read "get-milestones"
                  (sql-select 'milestone
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'project-id project-id))
                              #:order-by '((due-on asc)
                                           (created-at asc)))))

  (define (get-milestone-by-id id)
    (let ((rows (execute-read "get-milestone-by-id"
                              (sql-select 'milestone
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-milestone attrs)
    (execute-write "insert-milestone"
                   (sql-insert 'milestone attrs)))

  (define (update-milestone-by-id id attrs)
    (execute-write "update-milestone-by-id"
                   (sql-update 'milestone attrs
                               #:where (list (cons 'id id)))))

  (define (soft-delete-milestone id)
    (execute-write "soft-delete-milestone"
                   (sql-update 'milestone
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (mk-milestone-repository #:get-milestones-proc get-milestones
                           #:get-milestone-by-id-proc get-milestone-by-id
                           #:insert-milestone-proc insert-milestone
                           #:update-milestone-by-id-proc
                           update-milestone-by-id
                           #:soft-delete-milestone-proc
                           soft-delete-milestone))
