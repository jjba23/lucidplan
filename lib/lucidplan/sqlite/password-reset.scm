;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

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

(define-module (lucidplan sqlite password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain password-reset)
  #:export (make-sqlite-password-reset-repository))

;; --- SQLite implementation ---
(define (make-sqlite-password-reset-repository rc)
  (define password-reset-columns
    '(token ticket-id user-id))

  (define password-reset-upsert-columns
    '(token ticket-id user-id))

  (define (get-password-reset-by-token token)
    (let* ((query (format #f
                   "SELECT ~a FROM ticket_assignee WHERE project_id = ~a"
                   (symbols->sql-columns-list password-reset-columns)
                   (value->sql-literal token)))
           (_ (log-info "get-password-reset-by-token query:
	~a
" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'ticket-id)
                                     (assq-ref r
                                               'user-id))) rows))
           (_ (log-info "get-password-reset-by-token mapped-rows: ~a
"
                        (length mapped-rows))))
      mapped-rows))


  (mk-password-reset-repository #:get-password-reset-by-token-proc
                                 get-password-reset-by-token))
