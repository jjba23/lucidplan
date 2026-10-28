;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite ticket-revision)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-revision)
  #:export (make-sqlite-ticket-revision-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-revision-repository rc)
  (define columns
    '(id ticket-id
         project-id
         version
         author-id
         title
         description
         ticket-status
         ticket-type
         priority
         estimate-points
         estimate-size
         created-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-ticket-revisions ticket-id)
    (execute-read "get-ticket-revisions"
                  (sql-select 'ticket_revision
                              #:columns columns
                              #:where (list (cons 'ticket-id ticket-id))
                              #:order-by '((version desc)))))

  (define (get-ticket-revision-by-version ticket-id version)
    (let ((rows (execute-read "get-ticket-revision-by-version"
                              (sql-select 'ticket_revision
                                          #:columns columns
                                          #:where (list (cons 'ticket-id
                                                              ticket-id)
                                                        (cons 'version version))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-ticket-revision attrs)
    (execute-write "insert-ticket-revision"
                   (sql-insert 'ticket_revision attrs)))

  (mk-ticket-revision-repository #:get-ticket-revisions-proc
                                 get-ticket-revisions
                                 #:get-ticket-revision-by-version-proc
                                 get-ticket-revision-by-version
                                 #:insert-ticket-revision-proc
                                 insert-ticket-revision))
