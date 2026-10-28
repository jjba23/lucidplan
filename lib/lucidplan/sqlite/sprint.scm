;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite sprint)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain sprint)
  #:export (make-sqlite-sprint-repository))

;; --- SQLite implementation ---
(define (make-sqlite-sprint-repository rc)
  (define columns
    '(id project-id
         title
         goal
         starts-on
         ends-on
         sprint-status
         created-at
         updated-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-sprints project-id)
    (execute-read "get-sprints"
                  (sql-select 'sprint
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'project-id project-id))
                              #:order-by '((created-at desc)))))

  (define (get-sprint-by-id id)
    (let ((rows (execute-read "get-sprint-by-id"
                              (sql-select 'sprint
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  ;; A project works in one sprint at a time, so a second active sprint --
  ;; which the endpoints below take care not to create -- is still answered
  ;; as one sprint rather than as a list the page would have to choose from.
  (define (get-running-sprint project-id)
    (let ((rows (execute-read "get-running-sprint"
                              (sql-select 'sprint
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'project-id
                                                              project-id)
                                                        (cons 'sprint-status
                                                              "active"))
                                          #:order-by '((created-at desc))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-sprint attrs)
    (execute-write "insert-sprint"
                   (sql-insert 'sprint attrs)))

  (define (update-sprint-by-id id attrs)
    (execute-write "update-sprint-by-id"
                   (sql-update 'sprint attrs
                               #:where (list (cons 'id id)))))

  (define (soft-delete-sprint id)
    (execute-write "soft-delete-sprint"
                   (sql-update 'sprint
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (mk-sprint-repository #:get-sprints-proc get-sprints
                        #:get-sprint-by-id-proc get-sprint-by-id
                        #:get-running-sprint-proc get-running-sprint
                        #:insert-sprint-proc insert-sprint
                        #:update-sprint-by-id-proc update-sprint-by-id
                        #:soft-delete-sprint-proc soft-delete-sprint))
