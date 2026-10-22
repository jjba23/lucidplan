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

;;; The audit trail is append-only.  Every write the application makes to a
;;; ticket -- a field change, an assignment, a comment, an attachment, a move,
;;; a restore, a delete -- leaves a row here saying who did it, when, and what
;;; the value was before and after.  Nothing in the application updates or
;;; removes one: a history that can be rewritten is not an audit trail.

(define-module (lucidplan domain ticket-event)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-ticket-events insert-ticket-event))

;; -- Record definition ---

(define-record-with-kw (<ticket-event-repository>
                        %make-ticket-event-repository ticket-event-repository?)
                       mk-ticket-event-repository
                       (get-ticket-events-proc repo-get-ticket-events)
                       (insert-ticket-event-proc repo-insert-ticket-event))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-ticket-events repo-get-ticket-events
 "Retrieves the events recorded for TICKET-ID from REPO, newest first.")

(define-repo-method insert-ticket-event repo-insert-ticket-event
 "Appends an event using ATTRS to the trail in the given REPO.")
