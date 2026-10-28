;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The repositories behind planning poker: the room a team meets in, the
;;; items it puts a number on, the seats around it, and the cards played.
;;;
;;; The four are described together, in one module, for a reason worth stating
;;; once.  Everywhere else in this application a repository stands for a thing
;;; that is asked about on its own -- a ticket is read, a sprint is read, a
;;; comment is read -- and each has a module of its own because each is
;;; reached on its own.  None of these four is: a seat is only ever read to
;;; find who is sitting at a room, a card only to find what a round came to,
;;; and an item only to find what the room is asking now.  They are four
;;; tables of one aggregate, and splitting them across four files would say
;;; they could be used apart when they cannot.
;;;
;;; Nothing here points at `user'.  A participant is a name and a secret --
;;; see `(lucidplan domain)' for the seat a browser holds -- so the section
;;; works for a meeting in which most of the people present have no account
;;; on this instance at all.

(define-module (lucidplan domain poker)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-rooms get-room-by-id insert-room soft-delete-room
                         get-items-by-room get-current-item insert-item
                         reveal-item-by-id hide-item-by-id
                         record-item-ticket-by-id
                         get-participants-by-room
                         get-participant-by-token insert-participant
                         update-participant-by-id get-votes-by-item upsert-vote))

;;; ===== the room =====

(define-record-with-kw (<poker-room-repository> %make-poker-room-repository
                                                poker-room-repository?)
                       mk-poker-room-repository
                       (get-rooms-proc repo-get-rooms)
                       (get-room-by-id-proc repo-get-room-by-id)
                       (insert-room-proc repo-insert-room)
                       (soft-delete-room-proc repo-soft-delete-room))

(define-repo-method get-rooms repo-get-rooms
 "Retrieves the tables that are still open, newest first.

A closed table is not hidden because it is secret but because it is over: the
list is a list of rooms somebody may walk into, and one whose work is done is
not one of them.")

(define-repo-method get-room-by-id repo-get-room-by-id
                    "Retrieves the room ROOM-ID from REPO.")

(define-repo-method insert-room repo-insert-room
                    "Inserts a new room using ATTRS into the given REPO.")

(define-repo-method soft-delete-room repo-soft-delete-room
 "Marks the room ROOM-ID as closed in REPO.

Nothing else is touched.  The items asked in it, the seats around it and the
cards played are all left where they are, so a closed room is still readable:
closing a table ends the meeting, it does not erase what it decided.")

;;; ===== the items a room asks about =====

(define-record-with-kw (<poker-item-repository> %make-poker-item-repository
                                                poker-item-repository?)
                       mk-poker-item-repository
                       (get-items-by-room-proc repo-get-items-by-room)
                       (get-current-item-proc repo-get-current-item)
                       (insert-item-proc repo-insert-item)
                       (reveal-item-by-id-proc repo-reveal-item-by-id)
                       (hide-item-by-id-proc repo-hide-item-by-id)
                       (record-item-ticket-by-id-proc
                        repo-record-item-ticket-by-id))

(define-repo-method get-items-by-room repo-get-items-by-room
 "Retrieves every item ROOM-ID has asked about, in the round order.

The order is the round's, so the oldest item is the one a room started with
and the newest is the one it is on; a page reads them backwards to show what
the table has already settled.")

(define-repo-method get-current-item repo-get-current-item
 "Retrieves the item ROOM-ID is asking about now.

The current item is the one with the highest round, which is a state and not
an error to be missing: a room is created with its first item in the same
breath, but a room whose rows were removed by hand should answer nothing
rather than a wrong item.")

(define-repo-method insert-item repo-insert-item
                    "Inserts a new item using ATTRS into the given REPO.")

(define-repo-method record-item-ticket-by-id repo-record-item-ticket-by-id
 "Remembers that the item ITEM-ID was agreed and written onto a ticket.

PROJECT and NUMBER name the ticket the way its own addresses name it, so the
table can say where the number went and link there.  Only the round that was
written onto a ticket answers with one, which is why this is a column set once
rather than a flag on the ticket: what a room said and what a backlog now
carries are two facts, and a reader looking at a settled round asks which one
it became.")

(define-repo-method reveal-item-by-id repo-reveal-item-by-id
 "Turns the cards over on the item ITEM-ID, as of now.

The moment is kept rather than a flag, so a room's record says when it was
agreed and not merely that it was.")

(define-repo-method hide-item-by-id repo-hide-item-by-id
 "Turns the cards back over on the item ITEM-ID.

Revealing and hiding are the same column moved to a value or to nothing, and
they are two methods rather than one taking a value because clearing a
column is not a thing the query builders express: a `#f' handed to them is
written as the number zero, which is a moment in 1970 rather than the absence
of one.  Saying NULL in the statement is the honest spelling, and it is what
`unassign-sprint' in `(lucidplan sqlite ticket)' does for the same reason.")

;;; ===== the seats around it =====

(define-record-with-kw (<poker-participant-repository>
                        %make-poker-participant-repository
                        poker-participant-repository?)
                       mk-poker-participant-repository
                       (get-participants-by-room-proc
                        repo-get-participants-by-room)
                       (get-participant-by-token-proc repo-get-participant-by-token)
                       (insert-participant-proc repo-insert-participant)
                       (update-participant-by-id-proc repo-update-participant-by-id))

(define-repo-method get-participants-by-room repo-get-participants-by-room
 "Retrieves the seats taken at ROOM-ID, in the order they were taken.

The order people arrived is the order a room is read in, and it keeps a seat
from jumping around a page between two polls.")

(define-repo-method get-participant-by-token repo-get-participant-by-token
 "Retrieves the seat a browser holding TOKEN is sitting in.

This is how a request becomes a participant, and it is asked of the token
alone -- a token is unique across the whole table -- so the room a request
names is checked against the seat afterwards rather than trusted from the
URL: holding a seat at one table is not holding one at another.")

(define-repo-method insert-participant repo-insert-participant
                    "Inserts a new seat using ATTRS into the given REPO.")

(define-repo-method update-participant-by-id repo-update-participant-by-id
 "Updates the seat PARTICIPANT-ID with ATTRS in the given REPO.

The one thing written here after a seat is taken is when it was last active,
so a room's record says when each person last did something at it.")

;;; ===== the cards =====

(define-record-with-kw (<poker-vote-repository> %make-poker-vote-repository
                                               poker-vote-repository?)
                       mk-poker-vote-repository
                       (get-votes-by-item-proc repo-get-votes-by-item)
                       (upsert-vote-proc repo-upsert-vote))

(define-repo-method get-votes-by-item repo-get-votes-by-item
                    "Retrieves the cards played on ITEM-ID.")

(define-repo-method upsert-vote repo-upsert-vote
 "Writes one seat's card on one item, replacing whatever it held.

A vote is a seat's *current* card and not a stack of cards, so this is an
upsert rather than an insert: a participant who changes their mind from five
to eight has changed their vote, and a row that answered with both would
leave the room to guess which one they meant.  It is also what lets a table
be voted on again after its cards are turned over -- the second round
overwrites the first rather than piling up behind it.")
