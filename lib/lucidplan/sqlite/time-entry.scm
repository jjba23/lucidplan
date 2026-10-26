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

(define-module (lucidplan sqlite time-entry)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain time-entry)
  #:export (make-sqlite-time-entry-repository))

;; --- SQLite implementation ---
(define (make-sqlite-time-entry-repository rc)
  ;; The columns a row is read back with.  `deleted-at' is deliberately not
  ;; among them: a removed entry is filtered out by the statement below, so a
  ;; page is never handed one, and a column nothing reads is a column that
  ;; would only ever be a temptation to draw it.
  (define columns
    '(id ticket-id
         project-id
         author-id
         duration-minutes
         spent-on
         comment
         created-at
         updated-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  ;; A removed entry stops counting toward the ticket's total and stops being
  ;; drawn, so the question is put to the database rather than to the page.
  (define (get-time-entries ticket-id)
    (execute-read "get-time-entries"
                  (sql-select 'time_entry
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'ticket-id ticket-id))
                              #:order-by '((spent-on desc)
                                           (created-at desc)))))

  (define (insert-time-entry attrs)
    (execute-write "insert-time-entry"
                   (sql-insert 'time_entry attrs)))

  (define (soft-delete-time-entry id)
    (execute-write "soft-delete-time-entry"
                   (sql-update 'time_entry
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (define (move-time-entries ticket-id project-id)
    (execute-write "move-time-entries"
                   (sql-update 'time_entry
                               `((project-id unquote project-id))
                               #:where (list (cons 'ticket-id ticket-id)))))

  (mk-time-entry-repository #:get-time-entries-proc get-time-entries
                            #:insert-time-entry-proc insert-time-entry
                            #:soft-delete-time-entry-proc
                            soft-delete-time-entry
                            #:move-time-entries-proc move-time-entries))
