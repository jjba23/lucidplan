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

;;; The pages of the planning table: the list of tables somebody could walk
;;; into, and one table with its cards.
;;;
;;; Nothing here computes a number that is not drawn, and -- more to the point
;;; for this section -- nothing here decides whether a card may be shown.
;;; `poker-vote-face' in `(lucidplan domain)' answers that from the item's own
;;; `revealed-at', and both the seat that played a card and the record of what
;;; the room settled on are drawn through it, so there is one rule about the
;;; cards being face down rather than one per panel that happens to draw them.
;;;
;;; The page is not what keeps a vote secret, though: the endpoint that a
;;; polling browser asks does not carry a card at all until the room is
;;; revealed (see `app/controllers/poker.scm').  A page is a thing a reader
;;; has, and a secret that depends on the reader behaving is not a secret.
;;;
;;; Everything a page wants to change is a form that posts, so a table is
;;; opened, joined and voted at without a script having to run.  The one
;;; script the section loads makes the waiting live: it asks what has changed
;;; and marks the seats that have played, and it re-reads the page when the
;;; round moves on -- which is the one thing a page cannot redraw itself.

(define-module (app views poker)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (lucidplan html)
  #:use-module (lucidplan domain)
  #:use-module (hygguile hygguile)
  #:export (html-poker-tables html-poker-room))

;; =============================================================================
;;  Small pieces the two pages share
;; =============================================================================

(define %panel-face
  (mk-class (append '(flex flex-col gap-2 px-4 py-4 rounded-xl border-2
                           border-stone-300/75 dark:border-onyx-100/75
                           shadow-md) frosted-tint-classes)))

(define %muted-face
  (mk-class '(text-sm text-onyx-500 dark:text-stone-500)))

;; The name of a deck, said in the reader's language.
;;
;; The two labels are written as calls of the translator with the strings in
;; front of it rather than looked up from a table, which is what lets the
;; extractor find them: a label that only ever reaches the page as the
;; argument of a lookup is invisible to `xgettext', and the deck would then
;; stay in English inside an otherwise translated page.  This is the same
;; reason `translate-status-label' in `(lucidplan html)' is written this way.
(define (deck-label G_ deck)
  (cond
    ((equal? deck "points")
     (G_ "story points"))
    ((equal? deck "sizes")
     (G_ "t-shirt sizes"))
    (else deck)))

;; A card, as a button that submits the card it shows.  A form full of submit
;; buttons rather than a row of radios: the browser sends the name and value of
;; the button that was pressed, which is exactly the one fact the endpoint
;; needs, and it is one interactive element per card instead of two.
(define (card-face chosen?)
  (mk-class (append '(w-16 h-20 rounded-xl border-2 shadow-md font-sans
                           text-lg font-bold cursor-pointer
                           flex items-center justify-center
                           transition-colors duration-150
                           focus-visible:outline-2 focus-visible:outline-offset-2
                           outline-onyx-500)
                    (if chosen?
                        '(bg-asparagus-900 text-stone-100
                          border-asparagus-900
                          dark:bg-asparagus-100 dark:text-onyx-900
                          dark:border-asparagus-100)
                        '(bg-stone-300/75 text-onyx-700
                          border-stone-500/75 dark:bg-onyx-700/75
                          dark:text-stone-300
                          dark:border-onyx-100/75
                          hover:bg-stone-300 dark:hover:bg-onyx-700)))))

(define (render-card card chosen?)
  `(button (@ (type "submit")
              (name "value")
              (value ,(car card))
              ;; The card a reader is holding is what they are shown holding;
              ;; the payload is the value either way, so the state is carried
              ;; for a screen reader and not instead of the value.
              (aria-pressed ,(if chosen? "true" "false"))
              (class ,(card-face chosen?)))
           ,(cdr card)))

(define (poker-tables-url)
  "/poker")

(define (poker-room-url room-id)
  (format #f "/poker/~a" room-id))

;; =============================================================================
;;  The tables a team could sit at
;; =============================================================================

(define (render-table-row G_ row)
  (let* ((room (assq-ref row
                         'room))
         (room-id (format #f "~a"
                          (assq-ref room
                                    'id)))
         (seats (assq-ref row
                          'seats))
         (round (assq-ref row
                          'round))
         (asking (assq-ref row
                           'asking)))
    `(div (@ (class ,%panel-face))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            items-baseline
                                            justify-between))))
               (a (@ (href ,(poker-room-url room-id))
                     (class ,(mk-class '(text-md font-bold no-underline))))
                  ,(assq-ref room
                             'title))
               (small (@ (class ,%muted-face))
                      ,(deck-label G_ (assq-ref room
                                                'deck))))
          ,@(if asking
                (list `(p (@ (class ,(mk-class '(text-sm max-w-prose))))
                          ,asking))
                '())
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4
                                            items-baseline))))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a ~a" (G_ "round") (or round 0)))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "at the table") seats))))))

(define* (html-poker-tables G_
                            #:key
                            (projects '())
                            (rows '())
                            (decks '())
                            (is-logged-in #f))
  (html-template G_
                 #:title (G_ "planning poker")
                 #:canonical-path (poker-tables-url)
                 #:is-logged-in is-logged-in
                 #:proj projects
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4 flex flex-col
                                                           gap-3))))
                              (h2 (@ (class ,(mk-class '(text-lg font-bold))))
                                  ,(G_ "planning poker"))
                              (p (@ (class ,%muted-face))
                                 ,(G_
                                   "a table a team estimates at. Somebody opens one and names what is on it, everybody else takes a seat by typing their name, and each person plays a card; the cards stay face down until the table turns them over. No account is needed for any of it."))
                              ;; Opening a table is the one form on this
                              ;; page, and it is open to everybody: the room
                              ;; it makes is the thing that will need a name
                              ;; on it, and there is nothing else to qualify
                              ;; for.
                              (div (@ (class ,%panel-face))
                                   (h3 ,(G_ "open a table"))
                                   (form (@ (enctype
                                             "application/x-www-form-urlencoded")
                                            (method "POST")
                                            (action ,(poker-tables-url)))
                                         ,(input #:label (G_ "your name")
                                                 #:name "name"
                                                 #:autocomplete "nickname"
                                                 #:required #t)
                                         ,(input #:label
                                                 (G_ "what this meeting is called")
                                                 #:name "title"
                                                 #:required #t)
                                         ,(input #:label
                                                 (G_ "the first thing to estimate")
                                                 #:name "asking"
                                                 #:required #t)
                                         ,(select #:label
                                                  (G_ "how the table estimates")
                                                  #:name "deck"
                                                  #:options (map (lambda (one)
                                                                   (option #:value
                                                                           (car one)
                                                                           #:content
                                                                           (deck-label
                                                                            G_
                                                                            (car one))
                                                                           #:selected-value
                                                                           "points"))
                                                                 decks))
                                         (div (@ (class ,(mk-class '(my-1))))
                                              ,(submit-button (G_ "open it")))))
                              (h3 ,(G_ "tables you could sit at"))
                              (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                                   ,@(if (pair? rows)
                                         (map (lambda (row)
                                                (render-table-row G_ row))
                                              rows)
                                         (list `(p (@ (class ,%muted-face))
                                                   ,(G_
                                                     "no tables are open. Open one above and share its address with the team."))))))))

;; =============================================================================
;;  One table
;; =============================================================================

;; What a reader who has no seat at this table is offered: a name, and nothing
;; else.  There is no password because there is no account -- the cookie that
;; comes back is what makes the seat theirs, and the page says so rather than
;; leaving them to wonder what they signed up for.
(define (render-join-panel G_ room-id)
  `(div (@ (class ,%panel-face))
        (h3 ,(G_ "take a seat"))
        (p (@ (class ,%muted-face))
           ,(G_
             "type the name the rest of the table should see next to your card. It does not have to be your real one, and nothing else is asked for."))
        (form (@ (enctype "application/x-www-form-urlencoded")
                 (method "POST")
                 (action ,(format #f "~a/join" (poker-room-url room-id))))
              ,(input #:label (G_ "your name")
                      #:name "name"
                      #:autocomplete "nickname"
                      #:required #t)
              (div (@ (class ,(mk-class '(my-1))))
                   ,(submit-button (G_ "sit down"))))))

;; The seats, drawn as the cards they are holding.  A seat that has played is
;; shown with a face-down card while the room is unrevealed -- the page says
;; somebody has chosen, never what they chose -- and with the card's face once
;; it is.
;;
;; The wording a seat is marked with travels with the seat as data, because
;; the script that keeps the marks current cannot write a sentence of its own:
;; a sentence written into script is a sentence no translator can reach.
(define* (render-seat G_ #:key seat vote revealed? deck)
  (let* ((seat-id (format #f "~a"
                          (assq-ref seat
                                    'id)))
         (played? (and vote #t))
         (face (and vote
                    (poker-vote-face deck revealed? (cdr vote)))))
    `(div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                        items-center justify-between
                                        border-2 border-stone-300/50
                                        dark:border-onyx-100/50
                                        rounded-lg px-3 py-2 my-1)))
             (data-poker-seat ,seat-id)
             (data-poker-played ,(G_ "played"))
             (data-poker-waiting ,(G_ "waiting")))
          (span (@ (class ,(mk-class '(text-sm font-medium))))
                ,(assq-ref seat
                           'name))
          ,@(cond
              (face (list `(span (@ (class ,(mk-class '(text-lg font-bold
                                                              px-3 py-1
                                                              rounded-lg
                                                              border-2
                                                              bg-asparagus-900
                                                              text-stone-100
                                                              border-asparagus-900
                                                              dark:bg-asparagus-100
                                                              dark:text-onyx-900
                                                              dark:border-asparagus-100))))
                                ,face)))
              (revealed? (list `(span (@ (class ,%muted-face))
                                      ,(G_ "did not play"))))
              (else (list `(span (@ (class ,(mk-class '(text-xs px-2 py-1
                                                              rounded-lg
                                                              border-2
                                                              border-stone-300/75
                                                              dark:border-onyx-100/75)))
                                     (data-poker-mark ""))
                                 ,(if played?
                                      (G_ "played")
                                      (G_ "waiting")))))))))

;; What a round came to, once the cards are over: the cards that were played
;; and how many people played each, which of them the room agrees on, the mean
;; of the points, and how wide the disagreement was.  All four are the domain's
;; answers drawn, and the two that only mean something for one of the two decks
;; are simply absent for the other.
(define (render-result-panel G_ deck votes)
  (let* ((counts (poker-counts deck votes))
         (most (poker-most-chosen deck votes))
         (average (poker-average deck votes))
         (spread (poker-spread deck votes))
         (top (apply max 0 (map cdr counts))))
    `(div (@ (class ,%panel-face))
          (h3 ,(G_ "what the room said"))
          ,@(if (null? counts)
                (list `(p (@ (class ,%muted-face))
                          ,(G_ "nobody played a card")))
                (map (lambda (row)
                       (let* ((value (car row))
                              (count (cdr row))
                              (percent (if (> top 0)
                                           (round (* 100 (/ count top)))
                                           0)))
                         `(div (@ (class ,(mk-class '(flex flex-col gap-1
                                                             my-1))))
                               (div (@ (class ,(mk-class '(flex flex-row
                                                                  flex-wrap
                                                                  gap-3
                                                                  items-baseline))))
                                    (strong (@ (class ,(mk-class '(text-lg))))
                                            ,(poker-card-label deck value))
                                    (small (@ (class ,%muted-face))
                                           ,(format #f "~a ×" count)))
                               (div (@ (class ,(mk-class '(w-full h-2
                                                                  rounded-full
                                                                  border-2
                                                                  border-stone-300/75
                                                                  dark:border-onyx-100/75))))
                                    (div (@ (class ,(mk-class '(h-2 rounded-full
                                                                       bg-asparagus-500)))
                                            (style ,(format #f "width: ~a%"
                                                            percent))))))))
                     counts))
          ,@(if (null? most)
                '()
                (list `(p (@ (class ,(mk-class '(text-sm font-medium))))
                          ,(if (= (length most) 1)
                               (format #f "~a ~a"
                                       (G_ "the room agrees on")
                                       (poker-card-label deck (car most)))
                               (format #f "~a ~a"
                                       (G_ "the room is split between")
                                       (string-join (map (lambda (value)
                                                           (poker-card-label
                                                            deck value))
                                                         most)
                                                    ", "))))))
          ,@(if spread
                (list `(p (@ (class ,%muted-face))
                          ,(format #f "~a ~a → ~a"
                                   (G_ "the votes ran from")
                                   (poker-card-label deck (assq-ref spread
                                                                    'low))
                                   (poker-card-label deck (assq-ref spread
                                                                    'high)))))
                (list `(p (@ (class ,%muted-face))
                          ,(G_ "everybody played the same card"))))
          ,@(if average
                (list `(p (@ (class ,%muted-face))
                          ,(format #f "~a ~,1f" (G_ "average") average)))
                '()))))

;; The cards the table has already settled, newest first.  An item the room
;; dealt but never turned over is shown as unsettled rather than with the cards
;; it was holding: a page about a meeting has no business publishing a round
;; the table never looked at.
(define (render-settled-panel G_ rows)
  (if (null? rows)
      '()
      (list `(div (@ (class ,%panel-face))
                  (h3 ,(G_ "already estimated"))
                  (div (@ (class ,(mk-class '(flex flex-col gap-2))))
                       ,@(map (lambda (row)
                                `(div (@ (class ,(mk-class '(flex flex-row
                                                                   flex-wrap
                                                                   gap-2
                                                                   items-baseline
                                                                   justify-between))))
                                      (span (@ (class ,(mk-class '(text-sm))))
                                            ,(format #f "~a. ~a"
                                                     (assq-ref row
                                                               'round)
                                                     (assq-ref row
                                                               'title)))
                                      (small (@ (class ,%muted-face))
                                             ,(cond
                                                ((not (assq-ref row
                                                                'revealed))
                                                 (G_ "not turned over"))
                                                ((null? (assq-ref row
                                                                  'agreed))
                                                 (G_ "no cards"))
                                                (else (string-join (assq-ref
                                                                    row
                                                                    'agreed)
                                                                   ", "))))))
                              rows))))))

(define* (html-poker-room G_
                          #:key
                          (projects '())
                          room
                          item
                          (seats '())
                          (votes '())
                          (settled '())
                          (seat #f)
                          (is-logged-in #f))
  (let* ((room-id (format #f "~a"
                          (assq-ref room
                                    'id)))
         (deck (assq-ref room
                         'deck))
         (round (or (and item (assq-ref item
                                        'round)) 0))
         (shown? (and item
                      (assq-ref item
                                'revealed-at)
                      #t))
         (open? (not (assq-ref room
                               'deleted-at)))
         (mine (let ((found (and seat
                                 (assoc (format #f "~a"
                                                (assq-ref seat
                                                          'id))
                                        votes))))
                 (and found (cdr found))))
         (played-count (length votes))
         (seat-of (lambda (one)
                    (assoc (format #f "~a"
                                   (assq-ref one
                                             'id))
                           votes)))
         (seat-panel `(div (@ (class ,%panel-face))
                           ;; The count is the heading of the marks below it,
                           ;; so it is drawn as a part of the sentence that the
                           ;; polling script can replace on its own: a panel
                           ;; saying one of two had played, above two names
                           ;; saying they had, would be arguing with itself.
                           (h3 ,(G_ "at the table")
                               " "
                               (span (@ (data-poker-count ""))
                                     ,(format #f "(~a/~a)"
                                              played-count
                                              (length seats))))
                           ,@(if (pair? seats)
                                 (map (lambda (one)
                                        (render-seat G_ #:seat one
                                                     #:vote (seat-of one)
                                                     #:revealed? shown?
                                                     #:deck deck))
                                      seats)
                                 (list `(p (@ (class ,%muted-face))
                                           ,(G_ "somebody has to go first"))))))
         (card-panel (if (and seat open? (not shown?))
                         `(div (@ (class ,%panel-face))
                               (h3 ,(G_ "play a card"))
                               (p (@ (class ,%muted-face))
                                  ,(G_
                                    "the cards are face down, so nobody sees what anybody chose until the table is ready. Changing your mind is allowed: play another card and it replaces the first."))
                               (form (@ (enctype
                                         "application/x-www-form-urlencoded")
                                        (method "POST")
                                        (action ,(format #f "~a/vote"
                                                         (poker-room-url
                                                          room-id))))
                                     (div (@ (class ,(mk-class '(flex flex-row
                                                                        flex-wrap
                                                                        gap-2))))
                                          ,@(map (lambda (card)
                                                   (render-card card
                                                                (equal? (cdr card)
                                                                        mine)))
                                                 (poker-cards deck)))))
                          '()))
         (turn-panel (if (and seat open?)
                         `(div (@ (class ,%panel-face))
                               (h3 ,(G_ "the table"))
                               (form (@ (enctype
                                         "application/x-www-form-urlencoded")
                                        (method "POST")
                                        (action ,(format #f "~a/cards"
                                                         (poker-room-url
                                                          room-id))))
                                     (input (@ (hidden "")
                                               (name "action")
                                               (value ,(if shown?
                                                           "hide" "reveal"))))
                                     (div (@ (class ,(mk-class '(my-1))))
                                          ,(if shown?
                                               (submit-button (G_ "vote again"))
                                               (submit-button (G_
                                                               "turn the cards over"))))))
                         '()))
         ;; Dealing the next item is offered only once the cards are over,
         ;; which is the same condition the endpoint behind it refuses on: a
         ;; room that dealt on past a round would throw away numbers people
         ;; had just chosen.
         (next-panel (if (and seat open? shown?)
                         `(div (@ (class ,%panel-face))
                               (h3 ,(G_ "the next thing"))
                               (form (@ (enctype
                                         "application/x-www-form-urlencoded")
                                        (method "POST")
                                        (action ,(format #f "~a/next"
                                                         (poker-room-url
                                                          room-id))))
                                     ,(input #:label (G_ "what to estimate next")
                                             #:name "title"
                                             #:required #t)
                                     (div (@ (class ,(mk-class '(my-1))))
                                          ,(submit-button (G_ "deal it")))))
                         '()))
         (result-panel (if shown?
                           (list (render-result-panel G_ deck
                                                      (map cdr votes)))
                           '()))
         (close-panel (if (and seat open?)
                          `(div (@ (class ,%panel-face))
                                (h3 ,(G_ "closing the table"))
                                (p (@ (class ,%muted-face))
                                   ,(G_
                                     "closing a table takes it off the list of tables somebody could walk into. What it estimated and what it decided stay readable, so the record of the meeting is not lost with it."))
                                (form (@ (enctype
                                          "application/x-www-form-urlencoded")
                                         (method "POST")
                                         (action ,(format #f "~a/close"
                                                          (poker-room-url
                                                           room-id))))
                                      (div (@ (class ,(mk-class '(my-1))))
                                           ,(danger-submit-button
                                             (G_ "close the table")))))
                          '())))
    (html-template
     G_
     #:title (assq-ref room
                       'title)
     #:canonical-path (poker-room-url room-id)
     #:is-logged-in is-logged-in
     #:proj projects
     #:body `(div (@ (class ,(mk-class '(px-4 py-4 flex flex-col gap-3)))
                    ;; What the polling script reads the room's state from.
                    ;; The round and the reveal are as this page was drawn, so
                    ;; the script can tell a page that is still current from
                    ;; one about a round that is over.
                    (data-poker-room ,room-id)
                    (data-poker-round ,round)
                    (data-poker-revealed ,(if shown? "true" "false"))
                    (data-poker-state-url ,(format #f "~a/state"
                                                   (poker-room-url room-id))))
                  (a (@ (href ,(poker-tables-url))
                        (class ,(mk-class '(text-md px-2 no-underline))))
                     ,(G_ "← all tables"))
                  (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                    items-baseline))))
                       (h2 (@ (class ,(mk-class '(text-lg font-bold))))
                           ,(assq-ref room
                                      'title))
                       (span (@ (class ,(mk-class '(text-xs px-2 py-1
                                                          rounded-lg border-2
                                                          text-onyx-500
                                                          dark:text-stone-500
                                                          border-stone-300/75
                                                          dark:border-onyx-100/75))))
                             ,(deck-label G_ deck))
                       ,@(if open?
                             '()
                             (list `(span (@ (class ,(mk-class '(text-xs px-2
                                                                        py-1
                                                                        rounded-lg
                                                                        border-2
                                                                        text-onyx-500
                                                                        dark:text-stone-500
                                                                        border-stone-300/75
                                                                        dark:border-onyx-100/75))))
                                          ,(G_ "closed")))))
                  ,@(if item
                        (list `(div (@ (class ,%panel-face))
                                    (small (@ (class ,%muted-face))
                                           ,(format #f "~a ~a"
                                                    (G_ "round") round))
                                    (p (@ (class ,(mk-class '(text-md
                                                              font-medium
                                                              max-w-prose))))
                                       ,(assq-ref item
                                                  'title))))
                        (list `(p (@ (class ,%muted-face))
                                  ,(G_ "this table is asking nothing"))))
                  ,@(cond
                      ((not open?)
                       '())
                      ((not seat)
                       (list (render-join-panel G_ room-id)))
                      (else '()))
                  ,seat-panel
                  ,card-panel
                  ,turn-panel
                  ,@result-panel
                  ,next-panel
                  ,@(render-settled-panel G_ settled)
                  ,close-panel
                  ,@(if seat
                        (list (poker-script))
                        '())))))

;; The one script the section loads: it makes the waiting live rather than
;; making the table work.  It is only on the page of somebody who has a seat,
;; because the only thing it does is mark the seats that have played.
(define (poker-script)
  `(script (@ (type "text/javascript")
              (defer ""))
           ,(assoc-ref asset-library "pub/js/poker.js")))
