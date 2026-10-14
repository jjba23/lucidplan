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

(define-module (lucidplan sqlite ticket)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain ticket)
  #:export (make-sqlite-ticket-repository))

;; --- SQLite implementation ---
(define (make-sqlite-ticket-repository rc)
  (define columns
    '(id human-id
         project-id
         title
         ticket-status
         ticket-type
         description
         created-at
         updated-at
         deleted-at
         priority))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  ;; The public API addresses projects by their human id, but tickets reference
  ;; the project's internal id.  Resolving it once, here, keeps every ticket
  ;; statement a plain equality the builders can express -- and lets an unknown
  ;; project answer with #f (or an empty list) rather than a silently empty
  ;; subquery.
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

  (define (get-tickets project-id order-by)
    (let ((pid (project-id-of project-id)))
      (if (not pid)
          '()
          (execute-read "get-tickets"
                        (sql-select 'ticket
                                    #:columns columns
                                    #:where (list '(deleted-at is null)
                                                  (cons 'project-id pid))
                                    #:order-by order-by)))))

  (define (search-tickets project-id q order-by)
    ;; title OR description again, so the statement is a template whose three
    ;; values are bound; only the (controlled) order-by is rendered in.
    (let ((pid (project-id-of project-id))
          (pattern (format #f "%~a%" q)))
      (if (not pid)
          '()
          (execute-read "search-tickets"
                        (sql-parameterize (format #f
                                           "SELECT ~a FROM ticket WHERE deleted_at IS NULL AND project_id = ? AND (title LIKE ? OR description LIKE ?)~a"
                                           (quoted-sql-columns columns)
                                           (order-by-sql order-by))
                                          (list pid pattern pattern))))))

  (define (get-ticket-by-project-human-id-and-human-id project-id human-id)
    (let ((pid (project-id-of project-id)))
      (if (not pid) #f
          (let ((rows (execute-read
                       "get-ticket-by-project-human-id-and-human-id"
                       (sql-select 'ticket
                                   #:columns columns
                                   #:where (list '(deleted-at is null)
                                                 (cons 'project-id pid)
                                                 (cons 'human-id human-id))
                                   #:limit 1))))
            (if (nil? rows) #f
                (car rows))))))

  (define (get-ticket-of-latest-human-id project-id)
    (let ((pid (project-id-of project-id)))
      (if (not pid) #f
          (let ((rows (execute-read "get-ticket-of-latest-human-id"
                                    (sql-select 'ticket
                                                #:columns columns
                                                #:where (list '(deleted-at is
                                                                null)
                                                              (cons 'project-id
                                                                    pid))
                                                #:order-by '((human-id desc))
                                                #:limit 1))))
            (if (nil? rows) #f
                (car rows))))))

  (define (update-ticket-by-project-human-id-and-human-id project-id human-id
                                                          attrs)
    (let ((pid (project-id-of project-id)))
      (if (not pid) #f
          (execute-write "update-ticket-by-project-human-id-and-human-id"
                         (sql-update 'ticket attrs
                                     #:where (list (cons 'project-id pid)
                                                   (cons 'human-id human-id)))))))

  (define (insert-ticket attrs)
    (execute-write "insert-ticket"
                   (sql-insert 'ticket attrs)))

  (mk-ticket-repository #:get-tickets-proc get-tickets
                        #:search-tickets-proc search-tickets
                        #:get-ticket-by-project-human-id-and-human-id-proc
                        get-ticket-by-project-human-id-and-human-id
                        #:get-ticket-of-latest-human-id-proc
                        get-ticket-of-latest-human-id
                        #:update-ticket-by-project-human-id-and-human-id-proc
                        update-ticket-by-project-human-id-and-human-id
                        #:insert-ticket-proc insert-ticket))
