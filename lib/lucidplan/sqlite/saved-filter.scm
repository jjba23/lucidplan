;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (lucidplan sqlite saved-filter)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain saved-filter)
  #:export (make-sqlite-saved-filter-repository))

(define (make-sqlite-saved-filter-repository rc)
  (define columns
    '(id project-id
         name
         query
         created-at
         deleted-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-saved-filters project-id)
    (execute-read "get-saved-filters"
                  (sql-select 'saved_filter
                              #:columns columns
                              #:where (list '(deleted-at is null)
                                            (cons 'project-id project-id))
                              #:order-by '((name asc)))))

  (define (get-saved-filter-by-id id)
    (let ((rows (execute-read "get-saved-filter-by-id"
                              (sql-select 'saved_filter
                                          #:columns columns
                                          #:where (list '(deleted-at is null)
                                                        (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-saved-filter attrs)
    (execute-write "insert-saved-filter"
                   (sql-insert 'saved_filter attrs)))

  (define (soft-delete-saved-filter id)
    (execute-write "soft-delete-saved-filter"
                   (sql-update 'saved_filter
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (mk-saved-filter-repository #:get-saved-filters-proc get-saved-filters
                              #:get-saved-filter-by-id-proc
                              get-saved-filter-by-id
                              #:insert-saved-filter-proc insert-saved-filter
                              #:soft-delete-saved-filter-proc
                              soft-delete-saved-filter))
