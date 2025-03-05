;;; lucidplan - free and open project management for everyone

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

(define-module (lucidplan abstraction)
  #:use-module (ice-9 string-fun))

(define-public (only-active-tickets-filter ticket)
  (or (equal? (assoc-ref ticket "ticket_status") "todo")
      (equal? (assoc-ref ticket "ticket_status") "work-in-progress")
      (equal? (assoc-ref ticket "ticket_status") "in-review")))

(define-public (only-done-tickets-filter ticket)
  (equal? (assoc-ref ticket "ticket_status") "done"))

(define-public (only-rejected-tickets-filter ticket)
  (equal? (assoc-ref ticket "ticket_status") "rejected"))

