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

;;; SQLite behind the four planning-poker repositories.
;;;
;;; The four are made together for the same reason their records are
;;; described together (see `(lucidplan domain poker)'): no page or endpoint
;;; ever asks about a seat without asking about the room it sits in, so a
;;; module per table would be four files that only ever open the same
;;; connection.
;;;
;;; One rule they all keep: a room is answered whether or not it has been
;;; closed.  Closing a table is how a meeting ends, and the record of what it
;;; decided is the reason to still be able to read it -- so `get-room-by-id'
;;; answers a closed room and `get-rooms' does not list one.  What may no
;;; longer be written to a closed room is decided by the endpoints, which is
;;; where every other "is this still allowed" question in this application is
;;; decided too.

(define-module (lucidplan sqlite poker)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (lucidplan sqlite util)
  #:use-module (lucidplan domain poker)
  #:export (make-sqlite-poker-room-repository
            make-sqlite-poker-item-repository
            make-sqlite-poker-participant-repository
            make-sqlite-poker-vote-repository))

;; ===== the room =====

(define (make-sqlite-poker-room-repository rc)
  (define columns
    ;; The last two name the ticket the table was opened for, where it was
    ;; opened from a ticket's own page.  They are read everywhere the room is
    ;; -- a table is named by what it is about -- and written once, when the
    ;; room is made.
    '(id title deck ticket-project ticket-number created-at updated-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-rooms)
    (execute-read "get-poker-rooms"
                  (sql-select 'poker_room
                              #:columns columns
                              #:where '((deleted-at is null))
                              #:order-by '((created-at desc)))))

  (define (get-room-by-id id)
    (let ((rows (execute-read "get-poker-room-by-id"
                              (sql-select 'poker_room
                                          #:columns (append columns
                                                            '(deleted-at))
                                          #:where (list (cons 'id id))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-room attrs)
    (execute-write "insert-poker-room"
                   (sql-insert 'poker_room attrs)))

  (define (soft-delete-room id)
    (execute-write "soft-delete-poker-room"
                   (sql-update 'poker_room
                               `((deleted-at unquote
                                             (current-time)))
                               #:where (list (cons 'id id)))))

  (mk-poker-room-repository #:get-rooms-proc get-rooms
                            #:get-room-by-id-proc get-room-by-id
                            #:insert-room-proc insert-room
                            #:soft-delete-room-proc soft-delete-room))

;; ===== the items =====

(define (make-sqlite-poker-item-repository rc)
  (define columns
    '(id room-id round title revealed-at created-at ticket-project
         ticket-number))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-items-by-room room-id)
    (execute-read "get-poker-items-by-room"
                  (sql-select 'poker_item
                              #:columns columns
                              #:where (list (cons 'room-id room-id))
                              #:order-by '((round asc)))))

  (define (get-current-item room-id)
    (let ((rows (execute-read "get-poker-current-item"
                              (sql-select 'poker_item
                                          #:columns columns
                                          #:where (list (cons 'room-id room-id))
                                          #:order-by '((round desc))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-item attrs)
    (execute-write "insert-poker-item"
                   (sql-insert 'poker_item attrs)))

  (define (reveal-item-by-id id)
    (execute-write "reveal-poker-item-by-id"
                   (sql-update 'poker_item
                               `((revealed-at unquote
                                              (current-time)))
                               #:where (list (cons 'id id)))))

  ;; Written with an explicit NULL rather than through the builder: a `#f'
  ;; handed to `sql-update' is rendered as the number zero (see
  ;; `value->sql-literal' in kracht's SQL module), which would leave the item
  ;; looking revealed at the epoch instead of looking hidden.
  (define (hide-item-by-id id)
    (execute-write "hide-poker-item-by-id"
                   (sql-parameterize "UPDATE poker_item SET revealed_at = NULL WHERE id = ?"
                                     (list id))))

  ;; The ticket a round was written onto, both halves of it: the project and
  ;; the number, because that pair is what a link to a ticket is made of and
  ;; neither half means anything alone.
  (define (record-item-ticket-by-id id project number)
    (execute-write "record-poker-item-ticket-by-id"
                   (sql-update 'poker_item
                               `((ticket-project unquote project)
                                 (ticket-number unquote number))
                               #:where (list (cons 'id id)))))

  (mk-poker-item-repository #:get-items-by-room-proc get-items-by-room
                            #:get-current-item-proc get-current-item
                            #:insert-item-proc insert-item
                            #:reveal-item-by-id-proc reveal-item-by-id
                            #:hide-item-by-id-proc hide-item-by-id
                            #:record-item-ticket-by-id-proc
                            record-item-ticket-by-id))

;; ===== the seats =====

(define (make-sqlite-poker-participant-repository rc)
  (define columns
    '(id room-id name token joined-at last-seen-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  ;; The order is the order people arrived.  The id is the tie-break rather
  ;; than a second reading of the clock: two people joining in the same second
  ;; -- which is what happens when a link is pasted into a chat and opened by
  ;; everybody at once -- would otherwise be ordered by nothing at all, and a
  ;; seat that jumps around a page between two polls is a seat a reader has to
  ;; find again.
  (define (get-participants-by-room room-id)
    (execute-read "get-poker-participants-by-room"
                  (sql-select 'poker_participant
                              #:columns columns
                              #:where (list (cons 'room-id room-id))
                              #:order-by '((joined-at asc) (id asc)))))

  (define (get-participant-by-token token)
    (let ((rows (execute-read "get-poker-participant-by-token"
                              (sql-select 'poker_participant
                                          #:columns columns
                                          #:where (list (cons 'token token))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (insert-participant attrs)
    (execute-write "insert-poker-participant"
                   (sql-insert 'poker_participant attrs)))

  (define (update-participant-by-id id attrs)
    (execute-write "update-poker-participant-by-id"
                   (sql-update 'poker_participant attrs
                               #:where (list (cons 'id id)))))

  (mk-poker-participant-repository
   #:get-participants-by-room-proc get-participants-by-room
   #:get-participant-by-token-proc get-participant-by-token
   #:insert-participant-proc insert-participant
   #:update-participant-by-id-proc update-participant-by-id))

;; ===== the cards =====

(define (make-sqlite-poker-vote-repository rc)
  (define columns
    '(id item-id participant-id value created-at updated-at))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-votes-by-item item-id)
    (execute-read "get-poker-votes-by-item"
                  (sql-select 'poker_vote
                              #:columns columns
                              #:where (list (cons 'item-id item-id)))))

  ;; The conflict is a seat's card on a round, so the unique index over the
  ;; pair is what an upsert is told to collide on: playing a second card
  ;; rewrites the first rather than adding to it.  The id is not touched on
  ;; conflict -- it names the card that was played first, and keeping it means
  ;; a vote read back after a change still has the same row identity.
  (define (upsert-vote attrs)
    (execute-write "upsert-poker-vote"
                   (sql-upsert 'poker_vote attrs
                               #:dialect 'sqlite
                               #:conflict-target '(item-id participant-id)
                               #:update '(value updated-at))))

  (mk-poker-vote-repository #:get-votes-by-item-proc get-votes-by-item
                            #:upsert-vote-proc upsert-vote))
