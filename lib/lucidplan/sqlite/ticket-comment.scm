;;; lucidplan - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

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
