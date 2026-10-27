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

;;; The rules a planning table is made of: the two decks it can deal, the
;;; cards of each, what a round of cards adds up to, and the seat a browser
;;; holds without an account.
;;;
;;; The first two things checked here are the two that decide whether the
;;; section works at all.  One is that a card is a card of the room's own
;;; deck: the decks are the ladders a ticket is already estimated on, and a
;;; value that is not on one of them has to be refused rather than stored, or
;;; a number agreed at a table would be a number no ticket could be given.
;;;
;;; The other is the face-down rule.  A planning table only works while nobody
;;; can read the room before the cards are turned over, so `poker-vote-face' is
;;; checked here for the one thing it exists to guarantee: before the reveal it
;;; answers nothing, whatever it is asked, and afterwards it answers the card.
;;;
;;; The rest is arithmetic and reading -- what a round adds up to, whether the
;;; room agrees or is split, and what a name and a cookie hold -- and none of
;;; it touches a database or a request: the votes are strings, the rooms are
;;; strings, and the cookie is a string.
;;;
;;; The module is named `(veritas unit planning-poker-spec)' because that is
;;; the name the veritas runner derives from where the file sits
;;; (test/veritas/unit), which is how `maak test' loads it.

(define-module (veritas unit planning-poker-spec)
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (lucidplan domain)
  #:export (spec))

;;; ====== which deck a table is estimating with ======

(define deck-suite
  (suite "the deck a table deals"
         (test "a deck the table offers is taken as it is stored"
               (assert-equal #:name "story points"
                             #:expect "points"
                             #:got (parse-poker-deck "points"))
               (assert-equal #:name "t-shirt sizes"
                             #:expect "sizes"
                             #:got (parse-poker-deck "sizes")))
         (test "a deck typed in capitals means the same"
               (assert-equal #:name "capitals are the same deck"
                             #:expect "points"
                             #:got (parse-poker-deck "POINTS")))
         (test "a deck nobody deals is refused"
               ;; The room would have no card to show and no vote it could
               ;; check, so a ladder nothing knows is refused rather than
               ;; guessed at.
               (assert-equal #:name "the fibonacci deck is not one of ours"
                             #:expect #f
                             #:got (parse-poker-deck "fibonacci"))
               (assert-equal #:name "an empty form names no deck"
                             #:expect #f
                             #:got (parse-poker-deck ""))
               (assert-equal #:name "something that is not text names no deck"
                             #:expect #f
                             #:got (parse-poker-deck 'points)))
         (test "the decks are the two ladders a ticket is estimated on"
               (assert-equal #:name "the two the section ships"
                             #:expect '("points" "sizes")
                             #:got poker-decks)
               (assert-equal #:name "and their sizes are the ticket ladders"
                             #:expect ticket-sizes
                             #:got (map car (poker-cards "sizes")))
               (assert-equal #:name "and their points are too"
                             #:expect (map number->string ticket-points)
                             #:got (map car (poker-cards "points"))))
         (test "a deck is named in the words a reader is shown"
               (assert-equal #:name "the numbers a team burns down"
                             #:expect "story points"
                             #:got (poker-deck-label "points"))
               (assert-equal #:name "what a first pass is written in"
                             #:expect "t-shirt sizes"
                             #:got (poker-deck-label "sizes")))))

;;; ====== the cards themselves ======

(define card-suite
  (suite "a card of one of the decks"
         (test "a card of the room's deck is taken as the deck spells it"
               (assert-equal #:name "a story point"
                             #:expect "5"
                             #:got (parse-poker-card "points" "5"))
               (assert-equal #:name "a t-shirt size is lowercased"
                             #:expect "m"
                             #:got (parse-poker-card "sizes" "M"))
               (assert-equal #:name "space around a card is not part of it"
                             #:expect "8"
                             #:got (parse-poker-card "points" " 8 ")))
         (test "a card nobody deals is refused"
               ;; A number off the ladder is the case that matters: a team
               ;; that agrees on 4 has agreed on something no ticket can hold.
               (assert-equal #:name "a number off the ladder"
                             #:expect #f
                             #:got (parse-poker-card "points" "4"))
               (assert-equal #:name "a size off the ladder"
                             #:expect #f
                             #:got (parse-poker-card "sizes" "xxxl"))
               (assert-equal #:name "a card of the other deck"
                             #:expect #f
                             #:got (parse-poker-card "points" "m"))
               (assert-equal #:name "an empty form plays nothing"
                             #:expect #f
                             #:got (parse-poker-card "points" ""))
               (assert-equal #:name "nothing at all plays nothing"
                             #:expect #f
                             #:got (parse-poker-card "points" #f)))
         (test "the cards are dealt in the ladder's own order"
               ;; The order is what a tally is read in, so it is low to high
               ;; and not the order people happened to pick.
               (assert-equal #:name "a hand of points opens on nothing"
                             #:expect '("0" "1" "2" "3" "5" "8" "13" "21" "34")
                             #:got (map car (poker-cards "points")))
               (assert-equal #:name "a hand of sizes opens on the smallest"
                             #:expect '("xs" "s" "m" "l" "xl" "xxl")
                             #:got (map car (poker-cards "sizes")))
               (assert-equal #:name "a deck nobody deals deals nothing"
                             #:expect '()
                             #:got (poker-cards "fibonacci")))
         (test "a card is shown with the face the ladder gives it"
               (assert-equal #:name "a size is shown in capitals"
                             #:expect "XXL"
                             #:got (poker-card-label "sizes" "xxl"))
               (assert-equal #:name "a number is already what it says"
                             #:expect "13"
                             #:got (poker-card-label "points" "13")))
         (test "the sizes sort by the ladder and not by their letters"
               ;; `xs' and `xxl' are words: only the ladder says which of them
               ;; is the low end of a spread.
               (assert-equal #:name "the smallest is the first rung"
                             #:expect 0
                             #:got (poker-card-rank "sizes" "xs"))
               (assert-equal #:name "the largest is the last"
                             #:expect 5
                             #:got (poker-card-rank "sizes" "xxl"))
               (assert-equal #:name "a card nobody deals has no rung"
                             #:expect #f
                             #:got (poker-card-rank "sizes" "xxxl")))))

;;; ====== the cards stay face down ======

(define face-down-suite
  (suite "what a browser may know about a card before the reveal"
         (test "a card is not answered while the cards are face down"
               ;; The one rule the section stands on: whatever is asked, a
               ;; round that has not been turned over answers with nothing.
               (assert-equal #:name "a point, not yet revealed"
                             #:expect #f
                             #:got (poker-vote-face "points" #f "5"))
               (assert-equal #:name "a size, not yet revealed"
                             #:expect #f
                             #:got (poker-vote-face "sizes" #f "m")))
         (test "a card is answered once the room is revealed"
               (assert-equal #:name "a point, revealed"
                             #:expect "5"
                             #:got (poker-vote-face "points" #t "5"))
               (assert-equal #:name "a size, revealed, in capitals"
                             #:expect "M"
                             #:got (poker-vote-face "sizes" #t "m")))
         (test "a seat that played nothing has nothing to show either way"
               (assert-equal #:name "no card, revealed"
                             #:expect #f
                             #:got (poker-vote-face "points" #t #f))
               (assert-equal #:name "no card, unrevealed"
                             #:expect #f
                             #:got (poker-vote-face "points" #f #f)))))

;;; ====== what a round came to ======

(define tally-suite
  (suite "what a round of cards adds up to"
         (test "a card nobody played is not a row of the tally"
               (assert-equal #:name "two cards were played, in ladder order"
                             #:expect '(("2" . 1) ("5" . 2))
                             #:got (poker-counts "points" '("5" "5" "2"))))
         (test "a tally of sizes is counted too"
               (assert-equal #:name "two sizes, in ladder order"
                             #:expect '(("s" . 1) ("l" . 1))
                             #:got (poker-counts "sizes" '("l" "s"))))
         (test "a round nobody played has no tally"
               (assert-equal #:name "no cards, no rows"
                             #:expect '()
                             #:got (poker-counts "points" '())))
         (test "the room agrees on the card most of it played"
               (assert-equal #:name "the card played most often"
                             #:expect '("5")
                             #:got (poker-most-chosen "points" '("5" "5" "2"))))
         (test "an even split is a split, not a decision"
               ;; Picking one of two equally played cards would be inventing
               ;; an agreement nobody made, so both are answered.
               (assert-equal #:name "both cards of a tie, in ladder order"
                             #:expect '("2" "5")
                             #:got (poker-most-chosen "points" '("5" "2"))))
         (test "a round nobody played agrees on nothing"
               (assert-equal #:name "no cards, no agreement"
                             #:expect '()
                             #:got (poker-most-chosen "points" '())))
         (test "the mean of a round of points is a number"
               (assert-equal #:name "two cards average where they sit"
                             #:expect 6.5
                             #:got (poker-average "points" '("5" "8")))
               (assert-equal #:name "one card is its own mean"
                             #:expect 3.0
                             #:got (poker-average "points" '("3"))))
         (test "a round with no mean has none"
               ;; The sizes are words: `L' is not half way between `M' and
               ;; `XL', so there is no mean to answer.
               (assert-equal #:name "sizes have no mean"
                             #:expect #f
                             #:got (poker-average "sizes" '("m" "l")))
               (assert-equal #:name "no cards have no mean"
                             #:expect #f
                             #:got (poker-average "points" '())))
         (test "a spread is the two ends of the disagreement"
               (assert-equal #:name "the lowest and the highest of the points"
                             #:expect '((low . "2") (high . "13"))
                             #:got (poker-spread "points" '("13" "2" "5")))
               (assert-equal #:name "the ends of the sizes are sizes"
                             #:expect '((low . "s") (high . "xxl"))
                             #:got (poker-spread "sizes" '("xxl" "s"))))
         (test "a room that agrees has no spread"
               (assert-equal #:name "one card is no spread"
                             #:expect #f
                             #:got (poker-spread "points" '("5")))
               (assert-equal #:name "the same card twice is no spread"
                             #:expect #f
                             #:got (poker-spread "points" '("5" "5")))
               (assert-equal #:name "no cards is no spread"
                             #:expect #f
                             #:got (poker-spread "points" '())))))

;;; ====== the name somebody signs in with ======

(define name-suite
  (suite "the name a participant is known by"
         (test "the space around a name is not part of it"
               (assert-equal #:name "a padded name is trimmed"
                             #:expect "joe"
                             #:got (parse-participant-name "  joe  ")))
         (test "a run of spaces inside a name is one space"
               ;; Names are typed on phones and pasted from chat, where the
               ;; spacing is whatever it is; the name is the words.
               (assert-equal #:name "the words are kept, the gap is not"
                             #:expect "joe the dev"
                             #:got (parse-participant-name "joe   the   dev")))
         (test "a name of nothing is no name"
               (assert-equal #:name "an empty form"
                             #:expect #f
                             #:got (parse-participant-name ""))
               (assert-equal #:name "a form of spaces"
                             #:expect #f
                             #:got (parse-participant-name "   "))
               (assert-equal #:name "nothing at all"
                             #:expect #f
                             #:got (parse-participant-name #f))
               (assert-equal #:name "something that is not text"
                             #:expect #f
                             #:got (parse-participant-name '(joe))))
         (test "a name is capped because it is a label on a card"
               (assert-equal #:name "a name at the cap is a name"
                             #:expect (make-string poker-name-max-length
                                                  #\a)
                             #:got (parse-participant-name (make-string
                                                           poker-name-max-length
                                                           #\a)))
               (assert-equal #:name "one character past it is not"
                             #:expect #f
                             #:got (parse-participant-name (make-string
                                                           (+ 1
                                                              poker-name-max-length)
                                                           #\a))))))

;;; ====== the seat a browser holds ======

(define seat-suite
  (suite "the seat a browser holds without an account"
         (test "a seat is a room and a secret"
               (assert-equal #:name "one seat reads back"
                             #:expect '(("room-a" . "secret-1"))
                             #:got (poker-seats "room-a~secret-1"))
               (assert-equal #:name "two seats read back in written order"
                             #:expect '(("room-a" . "secret-1")
                                        ("room-b" . "secret-2"))
                             #:got (poker-seats "room-a~secret-1|room-b~secret-2")))
         (test "a value that is not a seat is not a seat"
               ;; A browser sends this header on every request, so a cookie
               ;; from anywhere else, or one somebody edited, is simply a
               ;; reader who is not seated anywhere.
               (assert-equal #:name "no cookie at all"
                             #:expect '()
                             #:got (poker-seats #f))
               (assert-equal #:name "an empty cookie"
                             #:expect '()
                             #:got (poker-seats ""))
               (assert-equal #:name "an entry without a secret"
                             #:expect '()
                             #:got (poker-seats "room-a"))
               (assert-equal #:name "an entry with an empty room"
                             #:expect '()
                             #:got (poker-seats "~secret-1"))
               (assert-equal #:name "a malformed entry is skipped, not obeyed"
                             #:expect '(("room-b" . "secret-2"))
                             #:got (poker-seats "garbage|room-b~secret-2")))
         (test "a seat is found by the room it was taken at"
               (assert-equal #:name "the seat at one room"
                             #:expect "secret-2"
                             #:got (poker-seat-token "room-a~secret-1|room-b~secret-2"
                                                    "room-b"))
               (assert-equal #:name "no seat at a room nobody sat at"
                             #:expect #f
                             #:got (poker-seat-token "room-a~secret-1"
                                                    "room-z")))
         (test "sitting at a second table does not cost the first seat"
               (assert-equal #:name "the newer seat is written first"
                             #:expect "room-b~secret-2|room-a~secret-1"
                             #:got (poker-seat-remember "room-a~secret-1"
                                                       "room-b" "secret-2"))
               (assert-equal #:name "a seat taken again moves to the front"
                             #:expect "room-a~secret-9|room-b~secret-2"
                             #:got (poker-seat-remember
                                    "room-a~secret-1|room-b~secret-2"
                                    "room-a" "secret-9")))
         (test "the number of seats a cookie holds is capped"
               ;; A cookie travels on every request, so the map cannot grow
               ;; without limit; the tables last sat at are the ones kept.
               (let* ((full (fold (lambda (n acc)
                                    (poker-seat-remember acc
                                                         (format #f "room-~a" n)
                                                         (format #f "secret-~a" n)))
                                  ""
                                  (iota poker-seats-kept)))
                      (grown (poker-seat-remember full "room-new" "secret-new")))
                 (assert-equal #:name "the cap is what the section says"
                               #:expect poker-seats-kept
                               #:got (length (poker-seats grown)))
                 (assert-equal #:name "the newest seat is the one that is there"
                               #:expect "secret-new"
                               #:got (poker-seat-token grown "room-new"))))))

(define (spec)
  (veritas-run deck-suite card-suite face-down-suite tally-suite name-suite
               seat-suite))
