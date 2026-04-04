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

(define-module (lucidplan domain)
  #:use-module (ice-9 string-fun))

(define-public (only-active-tickets-filter ticket)
  (or (equal? (assq-ref ticket
                        'ticket-status) "todo")
      (equal? (assq-ref ticket
                        'ticket-status) "work-in-progress")
      (equal? (assq-ref ticket
                        'ticket-status) "in-review")))

(define-public (only-done-tickets-filter ticket)
  (equal? (assq-ref ticket
                    'ticket-status) "done"))

(define-public (only-rejected-tickets-filter ticket)
  (equal? (assq-ref ticket
                    'ticket-status) "rejected"))

(define-public (entity-sort-from-query user-sort)
  (cond
    ((equal? "created-at-desc" user-sort)
     '(created_at desc))
    ((equal? "created-at-asc" user-sort)
     '(created_at asc))
    ((equal? "last-edited-at-desc" user-sort)
     '(updated_at desc))
    ((equal? "last-edited-at-asc" user-sort)
     '(updated_at asc))
    ((equal? "title-desc" user-sort)
     '(title desc))
    ((equal? "title-asc" user-sort)
     '(title asc))
    ((equal? "priority-desc" user-sort)
     '(priority desc))
    ((equal? "priority-asc" user-sort)
     '(priority asc))
    (else '(updated_at desc))))
