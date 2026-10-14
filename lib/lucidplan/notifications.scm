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

;;; ===== What LucidPlan tells its people, and who it tells =====

;;; A change somebody made is worth a word to the people it concerns, and the
;;; two things such a word needs are kept together here: the message -- which
;;; template pingwing is asked for, and what that template is filled with --
;;; and the audience.  The mail itself is handed to `(lucidplan mail)', which
;;; knows the service, the sender and the retries; nothing here posts
;;; directly, so a message cannot accidentally bypass the retrying.
;;;
;;; The one notification that exists yet is the new ticket.  A ticket is a
;;; thing other people were waiting on, so its arrival is the event that
;;; matters: whoever takes part in the project should hear that the board
;;; moved, without having to be looking at it.
;;;
;;; Who "takes part" is not a membership list -- LucidPlan keeps none -- but
;;; the people the project has already put to work: everyone who has been
;;; assigned a ticket in it.  The ticket's own assignee and the person who
;;; opened it are added on top, because the assignee may never have held a
;;; ticket in this project before, and the opener is a participant in the
;;; thing they just wrote even if nobody has ever assigned them anything.
;;; The address is what makes a person reachable, so a user with no address
;;; is passed over rather than counted, and an address is written to once
;;; however many times it comes up.

(define-module (lucidplan notifications)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (kracht prelude)
  #:use-module (lucidplan config)
  #:use-module (lucidplan domain)
  #:use-module (lucidplan domain user)
  #:use-module (lucidplan domain ticket-assignee)
  #:use-module (lucidplan mail)
  #:export (project-people notify-new-ticket!))

;; What a person's letter says when nothing better is known: a recipient with
;; no name of its own is still a recipient.
(define (person-of user)
  "USER as a @code{(name . address)} pair, or @code{#f} when it cannot be
written to.

A user row carries the name a reader is shown and the address a message goes
to; a row without an address is not an error -- an account may be made before
its address is known -- it is simply somebody this instance cannot reach."
  (let ((address (and user
                      (assq-ref user
                                'email)))
        (name (and user
                   (assq-ref user
                             'username))))
    (if (and (string? address)
             (not (string-null? address)))
        (cons (if (string? name) name "") address) #f)))

(define* (project-people #:key user-repo assignee-repo project-human-id
                         (also '()))
  "The people to write to about a change in the project PROJECT-HUMAN-ID.

The answer is a list of @code{(name . address)} pairs: everyone the project
has assigned a ticket to, plus the user ids in ALSO -- the ticket's own
assignee and the person who opened it, which the caller has in hand and this
function cannot guess.  A user that no longer exists, or that has no address,
is left out; an address appears once."
  (let* ((assigned (map (lambda (row)
                          (assq-ref row
                                    'user-id))
                        (get-ticket-assignee-by-project-human-id assignee-repo
                         project-human-id)))
         (ids (delete-duplicates (append also assigned))))
    (filter-map (lambda (id)
                  (person-of (get-user-by-id user-repo id))) ids)))

(define* (notify-new-ticket! #:key user-repo
                             assignee-repo
                             project
                             statuses
                             ticket
                             author-id
                             assignee-id)
  "Tell the people of PROJECT that TICKET has joined it.

@var{project} is the project row the ticket was written under and
@var{ticket} is the row that was written, as the repository answers them,
while @var{statuses} is the ladder the project's tickets move through -- the
status is named to the reader the way the board names it, so a project that
calls its first column something of its own says that in the mail too.  The
title and the description reach the message as the reader wrote them: they
are stored escaped for the pages, and a message is not a page.

Answer what @code{(lucidplan mail)}'s @code{send-email!} answers:
@code{#t} when the service took the message, @code{#f} when there was nobody
to write to or every attempt failed.  Reporting a failure is the caller's
business, never a reason to refuse the write that asked for the message."
  (let* ((project-human-id (assq-ref project
                                     'human-id))
         (human-id (assq-ref ticket
                             'human-id))
         (title (html-unescape (or (assq-ref ticket
                                             'title) "")))
         (description (html-unescape (or (assq-ref ticket
                                                   'description) "")))
         (status (ticket-status-label statuses
                                      (assq-ref ticket
                                                'ticket-status)))
         (author (and author-id
                      (get-user-by-id user-repo author-id)))
         (assignee (and assignee-id
                        (get-user-by-id user-repo assignee-id)))
         (recipients (project-people #:user-repo user-repo
                                     #:assignee-repo assignee-repo
                                     #:project-human-id project-human-id
                                     #:also (list author-id assignee-id))))
    (send-email! #:template 'lucidplan-new-ticket
                 #:subject (format #f "New ticket in ~a"
                                   (html-unescape (or (assq-ref project
                                                                'title)
                                                      project-human-id)))
                 #:recipients recipients
                 #:vars `((system-name unquote
                                       (config-ref 'site-name))
                          (project-name unquote
                                        (html-unescape (or (assq-ref project
                                                                     'title)
                                                           "")))
                          (project-human-id unquote project-human-id)
                          (ticket-human-id unquote human-id)
                          (ticket-title unquote title)
                          (ticket-type unquote
                                       (or (assq-ref ticket
                                                     'ticket-type) ""))
                          (ticket-status unquote status)
                          (priority unquote
                                    (or (assq-ref ticket
                                                  'priority) ""))
                          (author unquote
                                  (if author
                                      (assq-ref author
                                                'username) ""))
                          (assignee unquote
                                    (if assignee
                                        (assq-ref assignee
                                                  'username) ""))
                          (description unquote description)
                          (ticket-url unquote
                                      (absolute-url (format #f
                                                     "/tickets/~a/id/~a"
                                                     project-human-id human-id)))))))
