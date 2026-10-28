;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite ticket-assignee)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-assignee)
  #:export (make-sqlite-ticket-assignee-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-assignee-repository rc)
  (define columns
    '(project-id ticket-id user-id))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (project-id-of project-human-id)
    (let ((rows (execute-read "resolve-project-id"
                              (sql-select 'project
                                          #:columns '(id)
                                          #:where (list (cons 'human-id
                                                         project-human-id))
                                          #:limit 1))))
      (and (not (nil? rows))
           (assq-ref (car rows)
                     'id))))

  (define (get-ticket-assignee-by-project-human-id project-id)
    (let ((pid (project-id-of project-id)))
      (if (not pid)
          '()
          (execute-read "get-ticket-assignee-by-project-human-id"
                        (sql-select 'ticket_assignee
                                    #:columns columns
                                    #:where (list (cons 'project-id pid)))))))

  (define (get-ticket-assignee-by-ticket-id ticket-id)
    (let ((rows (execute-read "get-ticket-assignee-by-ticket-id"
                              (sql-select 'ticket_assignee
                                          #:columns columns
                                          #:where (list (cons 'ticket-id
                                                              ticket-id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (upsert-ticket-assignee attrs)
    ;; The conflict key is the ticket: assigning a ticket that already has an
    ;; assignee rewrites that row rather than failing on the primary key.
    (execute-write "upsert-ticket-assignee"
                   (sql-upsert 'ticket_assignee
                               attrs
                               #:dialect 'sqlite
                               #:conflict-target 'ticket-id
                               #:update '(user-id))))

  (define (unassign-ticket-assignee ticket-id)
    (execute-write "unassign-ticket-assignee"
                   (sql-delete 'ticket_assignee
                               #:where (list (cons 'ticket-id ticket-id)))))

  (define (move-ticket-assignee ticket-id project-id)
    (execute-write "move-ticket-assignee"
                   (sql-update 'ticket_assignee
                               `((project-id unquote project-id))
                               #:where (list (cons 'ticket-id ticket-id)))))

  (mk-ticket-assignee-repository
                                 #:get-ticket-assignee-by-project-human-id-proc
                                 get-ticket-assignee-by-project-human-id
                                 #:get-ticket-assignee-by-ticket-id-proc
                                 get-ticket-assignee-by-ticket-id
                                 #:upsert-ticket-assignee-proc
                                 upsert-ticket-assignee
                                 #:unassign-ticket-assignee-proc
                                 unassign-ticket-assignee
                                 #:move-ticket-assignee-proc
                                 move-ticket-assignee))
