;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite ticket-attachment)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-attachment)
  #:export (make-sqlite-ticket-attachment-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-attachment-repository rc)
  (define columns
    '(id ticket-id
         project-id
         uploader-id
         filename
         stored-name
         content-type
         size
         created-at
         deleted-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-ticket-attachments ticket-id)
    (execute-read "get-ticket-attachments"
                  (sql-select 'ticket_attachment
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'ticket-id ticket-id))
                              #:order-by '((created-at desc)))))

  (define (get-ticket-attachment-by-id id)
    (let ((rows (execute-read "get-ticket-attachment-by-id"
                              (sql-select 'ticket_attachment
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-ticket-attachment attrs)
    (execute-write "insert-ticket-attachment"
                   (sql-insert 'ticket_attachment attrs)))

  (define (soft-delete-ticket-attachment id)
    (execute-write "soft-delete-ticket-attachment"
                   (sql-update 'ticket_attachment
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (define (move-ticket-attachments ticket-id project-id)
    (execute-write "move-ticket-attachments"
                   (sql-update 'ticket_attachment
                               `((project-id unquote project-id))
                               #:where (list (cons 'ticket-id ticket-id)))))

  (mk-ticket-attachment-repository #:get-ticket-attachments-proc
                                   get-ticket-attachments
                                   #:get-ticket-attachment-by-id-proc
                                   get-ticket-attachment-by-id
                                   #:insert-ticket-attachment-proc
                                   insert-ticket-attachment
                                   #:soft-delete-ticket-attachment-proc
                                   soft-delete-ticket-attachment
                                   #:move-ticket-attachments-proc
                                   move-ticket-attachments))
