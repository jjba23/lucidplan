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

;;; The two estimates a ticket carries -- story points and t-shirt sizes -- and
;;; the record a change to a ticket leaves behind.
;;;
;;; Both estimates are optional, and an unestimated ticket is a ticket nobody
;;; has looked at yet rather than a ticket worth zero, so what is checked here
;;; is that nothing -- an empty form, a blank one, a value the ladder does not
;;; name -- is answered as no estimate rather than as zero.
;;;
;;; The second half is the history: what `ticket-changes' says actually moved
;;; between a ticket and the values it is being given.  A form submitted
;;; unchanged is not an edit, and a change that is recorded is recorded with
;;; the text a reader will be shown, so the two halves share the notion of the
;;; empty value below.
;;;
;;; Nothing here touches a database or a request: the ladders are values and
;;; the rules are procedures, so a value typed by hand, a value the menu
;;; offered and a value from a script are all just inputs.
;;;
;;; The module is named `(veritas unit ticket-estimate-spec)' because that is
;;; the name the veritas runner derives from where the file sits
;;; (test/veritas/unit), which is how `maak test' loads it.

(define-module (veritas unit ticket-estimate-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan domain)
  #:export (spec))

;;; ====== the story points a ticket is estimated at ======

(define points-suite
  (suite "a ticket's story-point estimate"
         (test "a number on the team's ladder is taken as that number"
               (assert-equal #:name "a value the menu offered is accepted"
                             #:expect 5
                             #:got (parse-estimate-points "5"))
               (assert-equal #:name "a number arrives as a number as well"
                             #:expect 13
                             #:got (parse-estimate-points 13))
               (assert-equal #:name "space around it is not part of it"
                             #:expect 3
                             #:got (parse-estimate-points " 3 "))
               (assert-equal #:name "zero is an estimate somebody made"
                             #:expect 0
                             #:got (parse-estimate-points "0")))
         (test "an estimate nobody made is no estimate, not zero"
               (assert-equal #:name "an empty form is no estimate"
                             #:expect #f
                             #:got (parse-estimate-points ""))
               (assert-equal #:name "a blank form is no estimate"
                             #:expect #f
                             #:got (parse-estimate-points "   "))
               (assert-equal #:name "no value at all is no estimate"
                             #:expect #f
                             #:got (parse-estimate-points #f)))
         (test "a number the team does not estimate with is refused"
               (assert-equal #:name "a value off the ladder is refused"
                             #:expect #f
                             #:got (parse-estimate-points "4"))
               (assert-equal #:name "a negative estimate is refused"
                             #:expect #f
                             #:got (parse-estimate-points "-1"))
               (assert-equal #:name "words are refused"
                             #:expect #f
                             #:got (parse-estimate-points "lots"))
               (assert-equal #:name "something that is not text is refused"
                             #:expect #f
                             #:got (parse-estimate-points '(1 2))))
         (test "the ladder is the one the menus and the endpoints share"
               (assert-equal #:name "the points are the shipped sequence"
                             #:expect '(0 1
                                        2
                                        3
                                        5
                                        8
                                        13
                                        21
                                        34)
                             #:got ticket-points))))

;;; ====== the t-shirt size a ticket is estimated at ======

(define size-suite
  (suite "a ticket's t-shirt size"
         (test "a size on the ladder is taken as it is stored"
               (assert-equal #:name "a size the menu offered is accepted"
                             #:expect "l"
                             #:got (parse-estimate-size "l"))
               (assert-equal #:name "a size typed in capitals means the same"
                             #:expect "xl"
                             #:got (parse-estimate-size "XL"))
               (assert-equal #:name "space around it is not part of it"
                             #:expect "m"
                             #:got (parse-estimate-size " m ")))
         (test "no size is no estimate"
               (assert-equal #:name "an empty form is no size"
                             #:expect #f
                             #:got (parse-estimate-size ""))
               (assert-equal #:name "a value that is not text is no size"
                             #:expect #f
                             #:got (parse-estimate-size 4)))
         (test "a size nobody offers is refused"
               (assert-equal #:name "a size off the ladder is refused"
                             #:expect #f
                             #:got (parse-estimate-size "xxxl"))
               (assert-equal #:name "the ladder is the shipped sequence"
                             #:expect '("xs" "s" "m" "l" "xl" "xxl")
                             #:got ticket-sizes))
         (test "each size is shown to a reader in capitals"
               (assert-equal #:name "the smallest is shown in capitals"
                             #:expect "XS"
                             #:got (ticket-size-label "xs"))
               (assert-equal #:name "the middle of the ladder"
                             #:expect "M"
                             #:got (ticket-size-label "m"))
               (assert-equal #:name "the largest is shown in capitals"
                             #:expect "XXL"
                             #:got (ticket-size-label "xxl"))
               (assert-equal #:name "a size nobody knows is shown as itself"
                             #:expect "huge"
                             #:got (ticket-size-label "huge")))))

;;; ====== an inline change to an estimate ======

(define inline-estimate-suite
  (suite "an estimate changed from the ticket's own page"
         (test "a story point the menu offered is accepted"
               (assert-equal #:name "the change is the column and its value"
                             #:expect '((estimate-points . "8"))
                             #:got (ticket-field-change "estimate-points" "8")))
         (test "clearing an estimate is a change the menu offers"
               (assert-equal #:name "the empty row changes the ticket"
                             #:expect '((estimate-points . ""))
                             #:got (ticket-field-change "estimate-points" "")))
         (test "a story point nobody offered is refused"
               (assert-equal #:name "a value off the ladder changes nothing"
                             #:expect #f
                             #:got (ticket-field-change "estimate-points" "4"))
               (assert-equal #:name "words change nothing"
                             #:expect #f
                             #:got (ticket-field-change "estimate-points"
                                                        "a lot")))
         (test "a size the menu offered is accepted"
               (assert-equal #:name "the change is the column and its value"
                             #:expect '((estimate-size . "m"))
                             #:got (ticket-field-change "estimate-size" "m"))
               (assert-equal #:name "clearing a size is offered too"
                             #:expect '((estimate-size . ""))
                             #:got (ticket-field-change "estimate-size" "")))
         (test "a size nobody offered is refused"
               (assert-equal #:name "a size off the ladder changes nothing"
                             #:expect #f
                             #:got (ticket-field-change "estimate-size" "xxxl")))))

;;; ====== what a change to a ticket is recorded as ======

;; A ticket as the repository answers it.  Only the tracked columns are here,
;; because they are the only ones a change is measured against.
(define stored-ticket
  '((title . "a ticket") (description . "what it is about")
    (ticket-status . "todo")
    (ticket-type . "task")
    (priority . 2)
    (estimate-points . 3)
    (estimate-size . "m")))

;; What the same ticket looks like submitted back without a change: every
;; value the form carries, matching the row above.
(define submitted-unchanged
  '((title . "a ticket") (description . "what it is about")
    (ticket-status . "todo")
    (ticket-type . "task")
    (priority . 2)
    (estimate-points . 3)
    (estimate-size . "m")))

(define (with-value alist column value)
  (cons (cons column value)
        (filter (lambda (pair)
                  (not (eq? (car pair) column))) alist)))

(define changes-suite
  (suite "the changes a write to a ticket is recorded with"
         (test "a ticket submitted back unchanged is not an edit"
               (assert-equal #:name "nothing moved, so nothing is recorded"
                             #:expect '()
                             #:got (ticket-changes stored-ticket
                                                   submitted-unchanged)))
         (test "a field that moved is recorded with its two values"
               (assert-equal #:name "the title, the label and both values"
                             #:expect '((title "title" "a ticket"
                                               "another ticket"))
                             #:got (ticket-changes stored-ticket
                                                   (with-value
                                                    submitted-unchanged
                                                    'title "another ticket"))))
         (test "several fields that moved are recorded one row each"
               (assert-equal #:name "both estimates moved"
                             #:expect 2
                             #:got (length (ticket-changes stored-ticket
                                                           (with-value (with-value
                                                                        submitted-unchanged
                                                                        'estimate-points
                                                                        8)
                                                                       'estimate-size
                                                                       "l")))))
         (test "an estimate that was cleared is a change"
               (assert-equal #:name "the estimate it had is recorded"
                             #:expect '("3" "")
                             #:got (cddr (car (ticket-changes stored-ticket
                                                              (with-value
                                                               submitted-unchanged
                                                               'estimate-points
                                                               #f))))))
         (test "an estimate that was never made is not a change"
               ;; A ticket nobody estimated, submitted back with the estimate
               ;; still empty: the absent value and the empty form are the
               ;; same state, so nothing is recorded.
               (assert-equal #:name "no estimate answers no change"
                             #:expect '()
                             #:got (ticket-changes (with-value stored-ticket
                                                               'estimate-points
                                                               #f)
                                                   (with-value
                                                    submitted-unchanged
                                                    'estimate-points ""))))
         (test "the two ways of saying nothing are the same state"
               (assert-equal #:name "a missing value and an empty text agree"
                             #:expect (tracked-value #f)
                             #:got (tracked-value ""))
               (assert-equal #:name "and both are recorded as the empty text"
                             #:expect ""
                             #:got (tracked-value #f))
               (assert-equal #:name "a number is recorded as its text"
                             #:expect "21"
                             #:got (tracked-value 21)))
         (test "a column the tracker does not watch is not a change"
               ;; `updated-at' moves on every write and says nothing about
               ;; what the ticket means, so it is deliberately not tracked.
               (assert-equal #:name "the stamp is not a tracked field"
                             #:expect #f
                             #:got (assq 'updated-at ticket-tracked-fields)))
         (test "every tracked field has the name a reader knows it by"
               (assert-equal #:name "the seven tracked fields are named"
                             #:expect '((title "title")
                                        (description "description")
                                        (ticket-status "status")
                                        (ticket-type "type")
                                        (priority "priority")
                                        (estimate-points "story points")
                                        (estimate-size "size"))
                             #:got ticket-tracked-fields))))

(define (spec)
  (veritas-run points-suite size-suite inline-estimate-suite changes-suite))
