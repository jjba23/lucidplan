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

;;; Planning poker: the tables a team estimates at.
;;;
;;; Three rules are worth stating once, because every endpoint below keeps
;;; them and none of them is obvious from a single handler.
;;;
;;; The first is that nothing here is behind an account.  Every other section
;;; of this application is guarded by a permission, and this one is guarded by
;;; a *seat*: a name somebody typed and a secret their browser holds.  That is
;;; deliberate rather than lax.  An estimation meeting is where the people who
;;; are not in this instance turn up -- a product owner, somebody from another
;;; team, a guest for the afternoon -- and a table that asked each of them to
;;; register before they could say "five" is a table the meeting would work
;;; around.  `lucidplan:ticket-create' is still what turns an agreed number
;;; into a ticket's estimate, and what opens a table for a ticket in the first
;;; place; those two do need an account, and playing a card does not.
;;;
;;; The second is that the room in the URL is never trusted on its own.  A
;;; request is resolved to a seat by the cookie it carries, and the seat is
;;; then checked to be sitting at the room being named; so a link to one table
;;; is not a seat at another, and a token stolen from one page cannot be
;;; replayed against a different room's rows.
;;;
;;; The third is that the cards are face down until the room says otherwise.
;;; Whether a vote may be shown is decided by `poker-vote-face' in
;;; `(lucidplan domain)', from the item's own `revealed-at', and it is that
;;; one answer which the page draws and the polling endpoint answers with --
;;; so there is no second place where a face could leak, and a browser that
;;; asks early is told nothing rather than told a smaller something.

(define-artanis-controller poker)

(import (app views poker)
        (web uri)
        (ice-9 string-fun)
        (srfi srfi-1)
        (kracht prelude)
        (uuid generate)
        (lucidplan auth)
        (lucidplan domain)
        (lucidplan domain poker)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain ticket-event)
        (lucidplan sqlite poker)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite ticket-event))

(define (new-id)
  (string-replace-substring (generate-string-uuid) "\n" ""))

(define (url-of text)
  (string->uri text))

;; The form a POST carried, as an alist.  A request that arrives with no body
;; is answered as an empty form rather than read as one: the reader of a body
;; takes a bytevector, so a POST that sent nothing would otherwise raise
;; inside the framework and take the whole server down with it, which is a
;; steep price for a button pressed twice or a client that closed early.
(define (posted-form rc)
  (let ((raw (rc-body rc)))
    (if raw
        (alist-from-url-encoded-body raw)
        '())))

(define (tables-url)
  (format #f "~a/poker" (current-myhost)))

(define (table-url room-id)
  (format #f "~a/poker/~a" (current-myhost) room-id))

;; How many tables the list shows.  A planning table is an afternoon's work
;; and the list of them only grows, so the page answers with the ones somebody
;; could still sit at rather than with every room an instance has ever held --
;; a page that scrolled for a year of meetings would be a page nobody reads
;; the top of.  The row is named for what it answers, and the number is the
;; operator's to change.
(define tables-shown
  20)

;; How much of a table's own record is shown: the items it has already
;; settled, newest first.  The current item is drawn above them and is not
;; part of this, so the page answers "what have we agreed" without becoming a
;; transcript of the meeting.
(define settled-shown
  5)

;;; ===== the seat a browser holds =====
;;;
;;; One cookie for the whole section, carrying a map of room to secret (see
;;; `poker-seat-remember' in `(lucidplan domain)'), so sitting at a second
;;; table does not cost a reader their seat at the first.  The name is a
;;; constant, which is what lets it be declared to the framework: Artanis
;;; refuses to write a cookie it was not told about, so the string here and
;;; the `#:cookies' rule on the routes that set it are one fact said twice.
(define poker-cookie-name
  "x-lucidplan-poker")

(define poker-cookie-symbol
  'x-lucidplan-poker)

(define (seat-cookie rc)
  (:cookies-value rc poker-cookie-name))

(define (remember-seat! rc room-id token)
  (:cookies-set! rc poker-cookie-symbol poker-cookie-name
                 (poker-seat-remember (seat-cookie rc) room-id token))
  (:cookies-setattr! rc poker-cookie-symbol
                     ;; A meeting is an afternoon, but a table is often left
                     ;; standing for the next one -- a planning room for a
                     ;; sprint is opened again the week after -- so the seat
                     ;; outlives the day it was taken.
                     #:expires (* 3600 24 30)
                     #:path "/"
                     #:secure #t
                     #:http-only #t))

;; The seat a request is sitting in at ROOM-ID, or #f.
;;
;; The cookie names a secret and the secret names a seat; the room is then
;; checked against that seat rather than believed from the URL.  This is the
;; whole of the section's authorisation, and it is one procedure so that every
;; endpoint below asks the same question the same way.
(define (seat-at rc participant-repo room-id)
  (let* ((token (poker-seat-token (seat-cookie rc) room-id))
         (seat (and token
                    (get-participant-by-token participant-repo token))))
    (and seat
         (equal? (format #f "~a" (assq-ref seat
                                           'room-id))
                 (format #f "~a" room-id))
         seat)))

(define (room-open? room)
  (not (assq-ref room
                 'deleted-at)))

(define (revealed? item)
  (and item
       (assq-ref item
                 'revealed-at)
       #t))

(define (votes-of vote-repo item)
  "Answer the cards played on ITEM as an alist of seat id to card."
  (if (not item)
      '()
      (map (lambda (vote)
             (cons (format #f "~a" (assq-ref vote
                                             'participant-id))
                   (assq-ref vote
                             'value)))
           (get-votes-by-item vote-repo (assq-ref item
                                                   'id)))))

;; ===== the tables there are =====

(get "/poker"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (room-repo (make-sqlite-poker-room-repository rc)))
         (response-emit (:mime rc
                               (html-poker-tables (:i18n rc)
                                                  #:projects (get-projects
                                                              project-repo)
                                                  #:rows (table-rows rc
                                                                     room-repo)
                                                  #:decks (map (lambda (deck)
                                                                 (cons deck
                                                                       (poker-deck-label
                                                                        deck)))
                                                               poker-decks)
                                                  #:is-logged-in (:cookies-value
                                                                  rc
                                                                  "x-bewaking-lucidplan")))))))

;; A row of the list: the room, what it is asking now, and how many people are
;; sitting at it.  It is built here rather than in the page because each of
;; the three is a question asked of a different table, and a page that queried
;; while drawing would be drawing from somewhere else than the markup says.
(define (table-rows rc room-repo)
  (let ((item-repo (make-sqlite-poker-item-repository rc))
        (participant-repo (make-sqlite-poker-participant-repository rc)))
    (map (lambda (room)
           (let* ((room-id (assq-ref room
                                     'id))
                  (item (get-current-item item-repo room-id))
                  (seats (get-participants-by-room participant-repo room-id)))
             (list (cons 'room room)
                   (cons 'round (and item (assq-ref item
                                                    'round)))
                   (cons 'asking (and item (assq-ref item
                                                     'title)))
                   (cons 'seats (length seats)))))
         (let ((rooms (get-rooms room-repo)))
           (take rooms
                 (min (length rooms)
                      tables-shown))))))

;; Opening a table is one write of three rows that only mean something
;; together -- the room, the first item it asks about, and the seat of the
;; person who opened it -- so they are written in one handler and nothing is
;; written unless all three can be.  The creator is a participant like
;; everybody else: there is no owner of a room, only the seat that happened to
;; come first.
(post "/poker"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (item-repo (make-sqlite-poker-item-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (body (posted-form rc))
               (deck (parse-poker-deck (assoc-ref body "deck")))
               (title (parse-table-title (assoc-ref body "title")))
               (asking (parse-table-title (assoc-ref body "asking")))
               (name (parse-participant-name (assoc-ref body "name"))))
          ;; A table nobody can name, or one that asks nothing, is not a
          ;; half-built room: nothing is written and the reader is handed back
          ;; the list to try again, which is better than a room whose first
          ;; round is blank.
          (if (or (not deck) (not title) (not asking) (not name))
              (redirect-to rc (url-of (tables-url))
                           #:status 302)
              (let ((room-id (new-id))
                    (token (new-id)))
                (insert-room room-repo
                             `((id unquote room-id)
                               (title unquote title)
                               (deck unquote deck)
                               (created-at unquote
                                           (current-time))))
                (insert-item item-repo
                             `((id unquote
                                   (new-id))
                               (room-id unquote
                                        room-id)
                               (round unquote 1)
                               (title unquote
                                      asking)
                               (created-at unquote
                                           (current-time))))
                (insert-participant participant-repo
                                    `((id unquote
                                          (new-id))
                                      (room-id unquote
                                               room-id)
                                      (name unquote
                                            name)
                                      (token unquote
                                             token)
                                      (joined-at unquote
                                                 (current-time))
                                      (last-seen-at unquote
                                                    (current-time))))
                (remember-seat! rc room-id token)
                (redirect-to rc (url-of (table-url room-id))
                             #:status 302))))))

;; A title -- of a meeting or of the thing being estimated -- is read the way
;; a name is: the space around it is not part of it, and an empty one is no
;; title at all.  It is escaped here because this application renders its
;; text unescaped (see `sxml->xml' in `(hygguile sxml)'), exactly as a sprint
;; title or a milestone title is.
(define (parse-table-title value)
  (and (string? value)
       (let ((text (string-trim-both value)))
         (and (not (string-null? text))
              (html-escape text)))))

;;; ===== a table opened for one ticket =====
;;;
;;; A team usually estimates a ticket just after reading it, and the address
;;; above is where a table is otherwise opened: it asks for the meeting's name
;;; and the first thing to estimate, because nothing there knows what the
;;; meeting is about.  Opened from a ticket's own page, both are already
;;; known, and so is the one fact that the rest of this section would
;;; otherwise have to be told on every push.  This is the same write as the
;;; one above with those facts filled in, and with no seat taken: whoever
;;; opens the table is asked to sit down by the table itself, exactly as
;;; everybody else is.

(post "/tickets/:project/id/:human-id/planning-table"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (open-for-ticket)
          (let* ((room-repo (make-sqlite-poker-room-repository rc))
                 (item-repo (make-sqlite-poker-item-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project (params rc "project"))
                 (number (params rc "human-id"))
                 (ticket (get-ticket-by-project-human-id-and-human-id
                          ticket-repo project number))
                 (deck (parse-poker-deck (assoc-ref (posted-form rc) "deck"))))
            ;; A table is opened for a ticket that is there, on a ladder this
            ;; application deals.  A reader who pressed the button on a ticket
            ;; that has since been deleted, or a form naming a deck nothing
            ;; knows, is handed back the ticket's page with nothing written,
            ;; rather than left at a room about nothing.
            (if (or (not ticket) (not deck))
                (redirect-to rc
                             (url-of (format #f "~a/tickets/~a/id/~a"
                                             (current-myhost) project number))
                             #:status 302)
                (let ((room-id (new-id)))
                  (insert-room room-repo
                               `((id unquote room-id)
                                 ;; The title is the ticket's own, exactly as
                                 ;; the ticket is shown by: it is already the
                                 ;; text a page renders, and escaping it a
                                 ;; second time would show a reader the
                                 ;; escape.
                                 (title unquote
                                        (assq-ref ticket 'title))
                                 (deck unquote deck)
                                 (ticket-project unquote project)
                                 (ticket-number unquote number)
                                 (created-at unquote
                                             (current-time))))
                  ;; The first round asks the ticket, which is the same
                  ;; sentence the meeting is named after: a table opened for
                  ;; a ticket starts on the ticket.
                  (insert-item item-repo
                               `((id unquote
                                     (new-id))
                                 (room-id unquote
                                          room-id)
                                 (round unquote 1)
                                 (title unquote
                                        (assq-ref ticket 'title))
                                 (created-at unquote
                                             (current-time))))
                  (redirect-to rc (url-of (table-url room-id))
                               #:status 302)))))
        ;; Opening a table for a ticket is the same kind of act as changing
        ;; that ticket's estimate -- it is deciding where the estimate will
        ;; come from -- so it asks for the permission the estimate itself is
        ;; changed with, and sends a reader who does not hold it to the login
        ;; like the push does.
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow open-for-ticket
                           #:unhappy-flow back-to-login)))

;; ===== one table =====

(get "/poker/:room"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (room-repo (make-sqlite-poker-room-repository rc))
              (item-repo (make-sqlite-poker-item-repository rc))
              (participant-repo (make-sqlite-poker-participant-repository rc))
              (vote-repo (make-sqlite-poker-vote-repository rc))
              (ticket-repo (make-sqlite-ticket-repository rc))
              (room-id (params rc "room"))
              (room (get-room-by-id room-repo room-id))
              ;; The ticket the table was opened for, where it was opened for
              ;; one, and whether it is still there to be written onto: a
              ;; table whose ticket has since been deleted says so rather than
              ;; offering a button that would be refused.
              (room-ticket (and room (poker-room-ticket room)))
              (ticket-exists? (and room-ticket
                                   (get-ticket-by-project-human-id-and-human-id
                                    ticket-repo
                                    (assq-ref room-ticket
                                              'project)
                                    (format #f "~a"
                                            (assq-ref room-ticket
                                                      'number)))
                                   #t)))
         ;; A room id that names nothing is a wrong turn, so the reader is sent
         ;; back to the list they came from rather than shown a page with
         ;; nothing in it.
         (if (not room)
             (redirect-to rc (url-of (tables-url))
                          #:status 302)
             (let ((item (get-current-item item-repo room-id))
                   (settled (get-items-by-room item-repo room-id)))
               (response-emit (:mime rc
                                     (html-poker-room (:i18n rc)
                                                      #:projects (get-projects
                                                                  project-repo)
                                                      #:room room
                                                      #:item item
                                                      #:seats (get-participants-by-room
                                                               participant-repo
                                                               room-id)
                                                      #:votes (votes-of vote-repo
                                                                        item)
                                                      #:settled (settled-rows
                                                                 vote-repo room
                                                                 item settled)
                                                      #:seat (seat-at rc
                                                                      participant-repo
                                                                      room-id)
                                                      ;; Whether the page offers the
                                                      ;; push onto a ticket is
                                                      ;; asked of the session the
                                                      ;; same way the endpoint
                                                      ;; behind it asks, so a
                                                      ;; control is drawn exactly
                                                      ;; when pressing it would
                                                      ;; be honoured.
                                                      #:can-record? (session-has-permission?
                                                                     #:rc rc
                                                                     #:token-name
                                                                     'x-bewaking-lucidplan
                                                                     #:permission
                                                                     'lucidplan:ticket-create)
                                                      #:ticket-exists? ticket-exists?
                                                      #:notices (record-notices
                                                                 rc (:i18n rc))
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-lucidplan")))))))))

;; What a table has already put a number on, newest first, with the card it
;; agreed on.  The current item is left out -- it is drawn above them -- and
;; so is anything the room dealt but never turned over: an item whose cards
;; are still face down is not part of what the table has settled.
(define (settled-rows vote-repo room item items)
  (let* ((settled (filter (lambda (one)
                            (not (equal? (assq-ref one
                                                  'id)
                                         (and item (assq-ref item
                                                             'id)))))
                          items))
         (order (reverse settled))
         (deck (assq-ref room
                         'deck)))
    (map (lambda (one)
           (let* ((votes (map (lambda (vote)
                                (assq-ref vote
                                          'value))
                              (get-votes-by-item vote-repo (assq-ref one
                                                                      'id))))
                  (shown? (revealed? one)))
             (list (cons 'round (assq-ref one
                                          'round))
                   (cons 'title (assq-ref one
                                          'title))
                   (cons 'revealed shown?)
                   (cons 'votes (length votes))
                   ;; Where a settled round's number went, if it went
                   ;; anywhere: the same two columns the round keeps, carried
                   ;; here so the page can say it without asking again.
                   (cons 'ticket-project (assq-ref one
                                                   'ticket-project))
                   (cons 'ticket-number (assq-ref one
                                                  'ticket-number))
                   ;; The agreed card is the one played most often, and is
                   ;; answered only once the cards were turned over; an even
                   ;; split answers with both cards rather than picking one,
                   ;; because a tie is not an agreement.
                   (cons 'agreed (if shown?
                                     (map (lambda (value)
                                            (poker-card-label deck value))
                                          (poker-most-chosen deck votes))
                                     '())))))
         (take order
               (min (length order)
                    settled-shown)))))

;;; ===== what a room agreed on, written onto a ticket =====
;;;
;;; A table answers with the card the room agreed on, and that answer is worth
;;; nothing to a backlog until somebody writes it down.  This is the one thing
;;; in the section that touches a ticket, and it is a different kind of act
;;; from playing a card: it needs an account, it needs the permission a
;;; ticket's own estimate is changed with, and it leaves a row in that
;;; ticket's own audit trail.  So it is guarded where the rest of the section
;;; is open, and it writes through the same column, the same check and the
;;; same trail the ticket's own page writes through.
;;;
;;; Which ticket it writes onto is the room's answer where the room has one:
;;; a table opened from a ticket's page (see `POST
;;; /tickets/:project/id/:human-id/planning-table' above) is about that ticket
;;; and nothing else, so its rounds are pushed there without being asked, and
;;; the two fields a reader fills in on a table opened for nothing are not
;;; read at all.

;; A change is only a change against the rest of the ticket, so the trail is
;; asked the way the ticket page asks it: the whole of the ticket's tracked
;; values, with the one column this table is writing replaced.
(define (ticket-values-with ticket column value)
  (map (lambda (field)
         (let ((col (car field)))
           (cons col (if (eq? col column)
                         value
                         (assq-ref ticket col)))))
       ticket-tracked-fields))

;; The row a change leaves behind in the ticket's own activity.  The ticket
;; page keeps a numbered version of the content as well, and this is not one:
;; a deadline moved from the sprints section is not one either, and the trail
;; is what says a ticket changed and why.
(define* (record-ticket-change! rc change
                                #:key ticket-id project-id actor-id)
  (insert-ticket-event (make-sqlite-ticket-event-repository rc)
                       `((id unquote (new-id))
                         (ticket-id unquote ticket-id)
                         (project-id unquote project-id)
                         (action unquote "changed")
                         (field unquote (cadr change))
                         (old-value unquote (caddr change))
                         (new-value unquote (cadddr change))
                         (created-at unquote (current-time))
                         ,@(if actor-id
                               `((actor-id unquote actor-id))
                               '()))))

;; A ticket's number is what its own addresses carry, so what is typed here is
;; read the same way: digits, trimmed, and nothing else is a number.
(define (parse-ticket-number value)
  (and (string? value)
       (let ((trimmed (string-trim-both value)))
         (and (not (string-null? trimmed))
              (every char-numeric? (string->list trimmed))
              trimmed))))

;; Where a table is sent back to when a push did not go through, with the
;; reason said after it: a form that silently does nothing is worse than one
;; that refuses out loud, and the reader is at a table rather than in a backlog
;; page's debug log.
(define (table-url-after room-id why)
  (format #f "~a?record_failed=~a" (table-url room-id) why))

(define (record-notices rc G_)
  "Answer the sentences a push onto a ticket that did not go through leaves."
  (let ((why (get-from-qstr rc "record_failed")))
    (if (not why)
        '()
        (list (cond
                ((equal? why "no-agreement")
                 (G_
                  "nothing was written: the room agreed on no single card, so there is no number to put on a ticket"))
                ((equal? why "still-face-down")
                 (G_
                  "nothing was written: the cards are still face down, and a number nobody has seen is not an answer"))
                ((equal? why "no-such-ticket")
                 (G_
                  "nothing was written: that project has no ticket with that number"))
                ((equal? why "closed")
                 (G_ "nothing was written: this table is closed"))
                (else (G_ "nothing was written")))))))

;; The card the table agreed on goes onto the ticket's estimate -- the one its
;; deck is a ladder for -- and nothing else about the ticket is touched.  The
;; round is then marked with where its number went, so the table can say so
;; rather than leaving the reader to wonder whether the press did anything.
(post "/poker/:room/record"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (define (do-record)
          (let* ((room-repo (make-sqlite-poker-room-repository rc))
                 (item-repo (make-sqlite-poker-item-repository rc))
                 (vote-repo (make-sqlite-poker-vote-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (body (posted-form rc))
                 (room-id (params rc "room"))
                 (room (get-room-by-id room-repo room-id))
                 ;; A table opened for a ticket is about that ticket for every
                 ;; round it deals, so the room answers which ticket a round's
                 ;; number belongs to, and the form's own two fields are read
                 ;; only at a table that was opened for nothing.  A ticket
                 ;; number is then asked for once -- when the table is opened
                 ;; -- rather than typed again at every push, and no round can
                 ;; be written onto a ticket its table was not opened to
                 ;; estimate.
                 (room-ticket (and room (poker-room-ticket room)))
                 (project (if room-ticket
                              (assq-ref room-ticket
                                        'project)
                              (assoc-ref body "project")))
                 (number (if room-ticket
                             (format #f "~a"
                                     (assq-ref room-ticket
                                               'number))
                             (parse-ticket-number (assoc-ref body
                                                             "number"))))
                 (item (and room (get-current-item item-repo room-id)))
                 (deck (and room (assq-ref room
                                           'deck)))
                 (agreed (and item
                              (revealed? item)
                              (poker-agreed-card deck
                                                 (map cdr (votes-of vote-repo
                                                                    item)))))
                 (field (and agreed (poker-estimate-field deck)))
                 (change (and field
                              (ticket-field-change field agreed)))
                 (ticket (and project number
                              (get-ticket-by-project-human-id-and-human-id
                               ticket-repo project number))))
            (cond
              ((not room)
               (redirect-to rc (url-of (tables-url)) #:status 302))
              ((not (room-open? room))
               (redirect-to rc (url-of (table-url-after room-id "closed"))
                            #:status 302))
              ((not (and item (revealed? item)))
               (redirect-to rc
                            (url-of (table-url-after room-id "still-face-down"))
                            #:status 302))
              ((not change)
               (redirect-to rc
                            (url-of (table-url-after room-id "no-agreement"))
                            #:status 302))
              ((not ticket)
               (redirect-to rc
                            (url-of (table-url-after room-id "no-such-ticket"))
                            #:status 302))
              (else
               (let* ((ticket-id (assq-ref ticket
                                           'id))
                      ;; A change is a one-entry association list -- the column
                      ;; and its new value -- so the column is behind `caar'.
                      ;; Asked for as `car' this would be the pair itself,
                      ;; which compares equal to no column of the ticket and
                      ;; would leave both estimates cleared rather than one
                      ;; written.
                      (column (caar change))
                      ;; The estimate the table is not writing stays what it
                      ;; was: a table estimates one ladder, and a push is not a
                      ;; reason to clear the other estimate somebody made.
                      (points (if (eq? column
                                       'estimate-points)
                                  (parse-estimate-points agreed)
                                  (assq-ref ticket
                                            'estimate-points)))
                      (size (if (eq? column
                                     'estimate-size)
                                (parse-estimate-size agreed)
                                (assq-ref ticket
                                          'estimate-size)))
                      (actor-id (session-owner-id #:rc rc
                                                  #:token-name
                                                  'x-bewaking-lucidplan))
                      (changes (ticket-changes
                                ticket
                                (ticket-values-with ticket column
                                                    (if (eq? column
                                                             'estimate-points)
                                                        points size)))))
                 (update-ticket-estimates-by-id ticket-repo ticket-id
                                                points size)
                 (for-each (lambda (one)
                             (record-ticket-change! rc one
                                                    #:ticket-id ticket-id
                                                    #:project-id
                                                    (assq-ref ticket
                                                              'project-id)
                                                    #:actor-id actor-id))
                           changes)
                 (record-item-ticket-by-id item-repo
                                           (assq-ref item
                                                     'id)
                                           project number)
                 (redirect-to rc (url-of (table-url room-id))
                              #:status 302))))))
        ;; Writing a table's answer down is a backlog write, so the guard is
        ;; the permission a ticket's own estimate is changed with rather than a
        ;; seat at the table: whoever may edit the backlog may take a room's
        ;; answer into it, whether or not they played a card.
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-record
                           #:unhappy-flow back-to-login)))

;; Everything a polling browser is allowed to know, and nothing else.  The two
;; numbers it needs are the round and whether the cards are over, because a
;; browser that sees either change has to re-read the page -- the room has
;; moved on and the page it is holding is about a round that is over.  What is
;; left is the seats: whether each has played, never which card.  A card is
;; not in this answer at all, so there is no field for a browser to read
;; early, no matter how it asks.
(get "/poker/:room/state"
     #:mime 'json
     (lambda (rc)
       (let* ((room-repo (make-sqlite-poker-room-repository rc))
              (item-repo (make-sqlite-poker-item-repository rc))
              (participant-repo (make-sqlite-poker-participant-repository rc))
              (vote-repo (make-sqlite-poker-vote-repository rc))
              (room-id (params rc "room"))
              (room (get-room-by-id room-repo room-id)))
         (if (not room)
             (response-emit (:mime rc
                                   '((error . "no such table")))
                            #:status 404)
             (let* ((item (get-current-item item-repo room-id))
                    (played (map car (votes-of vote-repo item))))
               (response-emit (:mime rc
                                     `((round . ,(or (and item
                                                          (assq-ref item
                                                                    'round)) 0))
                                       (revealed . ,(revealed? item))
                                       (seats . ,(list->vector
                                                  (map (lambda (seat)
                                                         `((id . ,(format
                                                                   #f "~a"
                                                                   (assq-ref
                                                                    seat
                                                                    'id)))
                                                           (played . ,(if (member
                                                                           (format
                                                                            #f
                                                                            "~a"
                                                                            (assq-ref
                                                                             seat
                                                                             'id))
                                                                           played)
                                                                          #t #f))))
                                                       (get-participants-by-room
                                                        participant-repo
                                                        room-id))))))))))))

;; Taking a seat.  A name is all that is asked for, and the secret handed back
;; in the cookie is what makes the seat the reader's own: nothing here knows
;; who they are, and nothing needs to.
(post "/poker/:room/join"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (room-id (params rc "room"))
               (room (get-room-by-id room-repo room-id))
               (body (posted-form rc))
               (name (parse-participant-name (assoc-ref body "name"))))
          ;; Somebody already seated who sends the form again keeps the seat
          ;; they have: joining twice is not two people, and a second row for
          ;; one reader would show the room a participant who never votes.
          (if (and room (room-open? room) name
                   (not (seat-at rc participant-repo room-id)))
              (let ((token (new-id)))
                (insert-participant participant-repo
                                    `((id unquote
                                          (new-id))
                                      (room-id unquote
                                               room-id)
                                      (name unquote
                                            (html-escape name))
                                      (token unquote
                                             token)
                                      (joined-at unquote
                                                 (current-time))
                                      (last-seen-at unquote
                                                    (current-time))))
                (remember-seat! rc room-id token))
              #f)
          (redirect-to rc (url-of (table-url room-id))
                       #:status 302))))

;; Playing a card, and changing it afterwards.  The card has to be one the
;; room's own deck deals, so a request that names a card nobody offers is
;; refused rather than stored, and a vote is a rewrite of the seat's own row
;; rather than a second card beside the first.
(post "/poker/:room/vote"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (item-repo (make-sqlite-poker-item-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (vote-repo (make-sqlite-poker-vote-repository rc))
               (room-id (params rc "room"))
               (room (get-room-by-id room-repo room-id))
               (seat (and room (seat-at rc participant-repo room-id)))
               (body (posted-form rc))
               (item (get-current-item item-repo room-id))
               (card (and room item (parse-poker-card (assq-ref room
                                                                'deck)
                                                      (assoc-ref body
                                                                 "value")))))
          (when (and seat item card (room-open? room) (not (revealed? item)))
            ;; The seat is touched as well as the card: a room's record says
            ;; when each person last did something at it, and a vote is the
            ;; thing they did.
            (upsert-vote vote-repo
                         `((id unquote
                               (new-id))
                           (item-id unquote
                                    (assq-ref item
                                              'id))
                           (participant-id unquote
                                           (assq-ref seat
                                                     'id))
                           (value unquote
                                  card)
                           (created-at unquote
                                       (current-time))
                           (updated-at unquote
                                       (current-time))))
            (update-participant-by-id participant-repo (assq-ref seat
                                                                 'id)
                                      `((last-seen-at unquote
                                                      (current-time)))))
          (redirect-to rc (url-of (table-url room-id))
                       #:status 302))))

;; Turning the cards over, and turning them back.  The two are the same change
;; -- one column, set to a moment or cleared -- so they are one endpoint, and
;; the state the form posts has to be one of the two the page offers.  Hiding
;; the cards again is what a room does when it disagrees: the cards go face
;; down, everybody votes again, and the second vote rewrites the first, so the
;; room can see who moved after the discussion.
(post "/poker/:room/cards"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (item-repo (make-sqlite-poker-item-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (room-id (params rc "room"))
               (room (get-room-by-id room-repo room-id))
               (seat (and room (seat-at rc participant-repo room-id)))
               (item (get-current-item item-repo room-id))
               (body (posted-form rc))
               (action (assoc-ref body "action")))
          (when (and seat item (room-open? room))
            (cond
              ((equal? action "reveal")
               (reveal-item-by-id item-repo (assq-ref item
                                                      'id)))
              ((equal? action "hide")
               (hide-item-by-id item-repo (assq-ref item
                                                    'id)))
              (else #f)))
          (redirect-to rc (url-of (table-url room-id))
                       #:status 302))))

;; Dealing the next thing to estimate.  A new item is always face down, so the
;; round that follows a reveal starts where the first one did.  It is refused
;; while the cards are still over the current item: a room that dealt on past
;; a round would leave cards nobody ever looked at, and the numbers people
;; just took the trouble to choose would be the ones thrown away.
(post "/poker/:room/next"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (item-repo (make-sqlite-poker-item-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (room-id (params rc "room"))
               (room (get-room-by-id room-repo room-id))
               (seat (and room (seat-at rc participant-repo room-id)))
               (item (get-current-item item-repo room-id))
               (body (posted-form rc))
               (title (parse-table-title (assoc-ref body "title"))))
          (when (and seat title (room-open? room)
                     (or (not item) (revealed? item)))
            (insert-item item-repo
                         `((id unquote
                               (new-id))
                           (room-id unquote
                                    room-id)
                           (round unquote
                                  (+ 1 (or (and item (assq-ref item
                                                               'round))
                                           0)))
                           (title unquote
                                  title)
                           (created-at unquote
                                       (current-time)))))
          (redirect-to rc (url-of (table-url room-id))
                       #:status 302))))

;; Closing the table.  The rows stay: the items asked, the seats taken and the
;; cards played are left exactly as they were, because a closed room is the
;; record of what the meeting decided, and the record is the reason to have
;; kept any of it.
(post "/poker/:room/close"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan x-lucidplan-poker lang)
      (lambda (rc)
        (let* ((room-repo (make-sqlite-poker-room-repository rc))
               (participant-repo (make-sqlite-poker-participant-repository rc))
               (room-id (params rc "room"))
               (room (get-room-by-id room-repo room-id))
               (seat (and room (seat-at rc participant-repo room-id))))
          (when (and seat (room-open? room))
            (soft-delete-room room-repo room-id))
          (redirect-to rc (url-of (tables-url))
                       #:status 302))))
