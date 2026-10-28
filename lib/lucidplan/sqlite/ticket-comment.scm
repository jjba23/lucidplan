;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite ticket-comment)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket-comment)
  #:export (make-sqlite-ticket-comment-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-comment-repository rc)
  (define columns
    '(id ticket-id
         project-id
         parent-id
         author-id
         body
         created-at
         updated-at
         deleted-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  ;; `deleted-at is null' is deliberately absent: a removed comment is still
  ;; the node its replies answer, so it is read back and drawn as removed.
  (define (get-ticket-comments ticket-id)
    (execute-read "get-ticket-comments"
                  (sql-select 'ticket_comment
                              #:columns columns
                              #:where (list (cons 'ticket-id ticket-id))
                              #:order-by '((created-at asc)))))

  (define (insert-ticket-comment attrs)
    (execute-write "insert-ticket-comment"
                   (sql-insert 'ticket_comment attrs)))

  (define (soft-delete-ticket-comment id)
    (execute-write "soft-delete-ticket-comment"
                   (sql-update 'ticket_comment
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (define (move-ticket-comments ticket-id project-id)
    (execute-write "move-ticket-comments"
                   (sql-update 'ticket_comment
                               `((project-id unquote project-id))
                               #:where (list (cons 'ticket-id ticket-id)))))

  (mk-ticket-comment-repository #:get-ticket-comments-proc get-ticket-comments
                                #:insert-ticket-comment-proc
                                insert-ticket-comment
                                #:soft-delete-ticket-comment-proc
                                soft-delete-ticket-comment
                                #:move-ticket-comments-proc
                                move-ticket-comments))
