;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite ticket-event)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-event)
  #:export (make-sqlite-ticket-event-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-event-repository rc)
  (define columns
    '(id ticket-id
         project-id
         actor-id
         action
         field
         old-value
         new-value
         created-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-ticket-events ticket-id)
    (execute-read "get-ticket-events"
                  (sql-select 'ticket_event
                              #:columns columns
                              #:where (list (cons 'ticket-id ticket-id))
                              #:order-by '((created-at desc)))))

  (define (insert-ticket-event attrs)
    (execute-write "insert-ticket-event"
                   (sql-insert 'ticket_event attrs)))

  (mk-ticket-event-repository #:get-ticket-events-proc get-ticket-events
                              #:insert-ticket-event-proc insert-ticket-event))
