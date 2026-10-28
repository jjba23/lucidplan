;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
