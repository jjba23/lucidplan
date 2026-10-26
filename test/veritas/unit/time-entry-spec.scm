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

;;; The time a ticket has cost, written the way people write it.
;;;
;;; A duration is typed into a form by somebody who has just done the work, so
;;; what is accepted is what they would say: an hour and a half is `1.5h',
;;; `90m', `1h30m', `1h 30m' or `1:30', and every one of them has to become the
;;; same ninety minutes.  What is checked here is that they do -- and that
;;; nothing else does.  A form that says nothing a duration can be read from
;;; writes nothing rather than an entry of zero, because an entry of zero sits
;;; in the log saying nothing happened while counting toward nothing.
;;;
;;; The second half is how a stored number of minutes is said back: `1h 30m'
;;; for a reader, `1.5' for a report, and neither of them a fraction.
;;;
;;; Nothing here touches a database or a request: a duration is a string and
;;; `parse-duration' is a procedure, so a value typed into a form, one sent by
;;; a script and one written in another test are all just strings.
;;;
;;; The module is named `(veritas unit time-entry-spec)' because that is the
;;; name the veritas runner derives from where the file sits
;;; (test/veritas/unit), which is how `maak test' loads it.

(define-module (veritas unit time-entry-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan domain)
  #:export (spec))

;;; ====== reading a duration ======

(define parsing-suite
  (suite "a duration, read the ways it is written"
         (test "an hour and a half is ninety minutes however it is typed"
               (assert-equal #:name "in hours, with a fraction"
                             #:expect 90
                             #:got (parse-duration "1.5h"))
               (assert-equal #:name "in minutes alone"
                             #:expect 90
                             #:got (parse-duration "90m"))
               (assert-equal #:name "as hours and minutes, run together"
                             #:expect 90
                             #:got (parse-duration "1h30m"))
               (assert-equal #:name "as hours and minutes, with a space"
                             #:expect 90
                             #:got (parse-duration "1h 30m"))
               (assert-equal #:name "as a clock reading"
                             #:expect 90
                             #:got (parse-duration "1:30"))
               (assert-equal #:name "as a bare number of minutes"
                             #:expect 90
                             #:got (parse-duration "90")))
         (test "a duration is read in the words it is spoken in"
               (assert-equal #:name "two hours, spelled out"
                             #:expect 120
                             #:got (parse-duration "2 hours"))
               (assert-equal #:name "three quarters of an hour"
                             #:expect 45
                             #:got (parse-duration "45 min"))
               (assert-equal #:name "an abbreviation of an hour"
                             #:expect 60
                             #:got (parse-duration "1hr"))
               (assert-equal #:name "an abbreviation of minutes"
                             #:expect 15
                             #:got (parse-duration "15mins"))
               (assert-equal #:name "a whole day of work is a day"
                             #:expect 1440
                             #:got (parse-duration "24h")))
         (test "the case of what was typed does not matter"
               (assert-equal #:name "capitals are the same duration"
                             #:expect 90
                             #:got (parse-duration "1H30M")))
         (test "space around the whole thing is not part of it"
               (assert-equal #:name "a padded form still means its duration"
                             #:expect 30
                             #:got (parse-duration "  30m  ")))))

(define refusal-suite
  (suite "what is not a duration"
         (test "no time logged is no entry"
               (assert-equal #:name "an empty form is refused"
                             #:expect #f
                             #:got (parse-duration ""))
               (assert-equal #:name "a form of nothing but space is refused"
                             #:expect #f
                             #:got (parse-duration "   "))
               (assert-equal #:name "no value at all is refused"
                             #:expect #f
                             #:got (parse-duration #f))
               (assert-equal #:name "zero minutes is not time spent"
                             #:expect #f
                             #:got (parse-duration "0"))
               (assert-equal #:name "zero hours is not time spent either"
                             #:expect #f
                             #:got (parse-duration "0h")))
         (test "a day's worth is the ceiling"
               (assert-equal #:name "more than a day is a typo"
                             #:expect #f
                             #:got (parse-duration "25h"))
               (assert-equal #:name "a bare number past the ceiling is a typo"
                             #:expect #f
                             #:got (parse-duration "9000")))
         (test "a clock reading is read as a clock, not as two numbers"
               (assert-equal #:name "minutes past the hour are minutes"
                             #:expect #f
                             #:got (parse-duration "1:99"))
               (assert-equal #:name "a single minute digit is not a clock"
                             #:expect #f
                             #:got (parse-duration "1:5")))
         (test "what names no duration at all is refused"
               (assert-equal #:name "words are refused"
                             #:expect #f
                             #:got (parse-duration "a while"))
               (assert-equal #:name "a negative duration is refused"
                             #:expect #f
                             #:got (parse-duration "-90m"))
               (assert-equal #:name "a unit nobody knows is refused"
                             #:expect #f
                             #:got (parse-duration "3d"))
               (assert-equal #:name "a stray character is not skipped"
                             #:expect #f
                             #:got (parse-duration "1h!"))
               (assert-equal #:name "something that is not text is refused"
                             #:expect #f
                             #:got (parse-duration 90)))))

;;; ====== saying a duration back ======

(define formatting-suite
  (suite "a number of minutes, said to a reader"
         (test "a whole number of hours is said in hours"
               (assert-equal #:name "two hours"
                             #:expect "2h"
                             #:got (format-duration 120)))
         (test "less than an hour is said in minutes"
               (assert-equal #:name "three quarters of an hour"
                             #:expect "45m"
                             #:got (format-duration 45)))
         (test "hours and minutes are said together"
               (assert-equal #:name "an hour and a half"
                             #:expect "1h 30m"
                             #:got (format-duration 90)))
         (test "nothing is said as nothing rather than as empty space"
               (assert-equal #:name "zero minutes still reads as a duration"
                             #:expect "0m"
                             #:got (format-duration 0)))
         (test "a number that is not a duration is said as nothing"
               (assert-equal #:name "no value answers the empty text"
                             #:expect ""
                             #:got (format-duration #f))
               (assert-equal #:name "a negative duration answers it too"
                             #:expect ""
                             #:got (format-duration -30)))))

(define hours-suite
  (suite "a number of minutes, added up in hours"
         (test "a whole number of hours is a whole decimal"
               (assert-equal #:name "an hour is 1"
                             #:expect "1"
                             #:got (format-hours 60))
               (assert-equal #:name "two hours is 2"
                             #:expect "2"
                             #:got (format-hours 120)))
         (test "part of an hour is a fraction of it"
               (assert-equal #:name "an hour and a half is 1.5"
                             #:expect "1.5"
                             #:got (format-hours 90))
               (assert-equal #:name "three quarters is 0.75"
                             #:expect "0.75"
                             #:got (format-hours 45)))
         (test "a number that is not a duration is zero hours"
               (assert-equal #:name "no value answers zero"
                             #:expect "0"
                             #:got (format-hours #f)))))

;;; ====== adding up what a ticket cost ======

(define entries
  '(((id . "e1") (duration-minutes . 90) (spent-on . "2026-10-27"))
    ((id . "e2") (duration-minutes . 30) (spent-on . "2026-10-26"))
    ((id . "e3") (duration-minutes . 45) (spent-on . "2026-10-25"))))

(define total-suite
  (suite "what a ticket's time entries add up to"
         (test "every entry counts toward the total"
               (assert-equal #:name "two and a half hours over three days"
                             #:expect 165
                             #:got (time-entries-minutes entries)))
         (test "a ticket nobody logged time on adds up to nothing"
               (assert-equal #:name "no entries is no minutes"
                             #:expect 0
                             #:got (time-entries-minutes '())))
         (test "a row that is not a duration does not break the sum"
               ;; The schema does not allow a row without a duration, but a
               ;; total is a report and a report should not fail because one
               ;; row is malformed: it counts as nothing and the rest stand.
               (assert-equal #:name "a malformed row counts as nothing"
                             #:expect 120
                             #:got (time-entries-minutes
                                    (list (car entries)
                                          '((id . "bad"))
                                          (cadr entries)))))
         (test "a total reads as a duration and as hours"
               (assert-equal #:name "one hundred and sixty-five minutes"
                             #:expect "2h 45m"
                             #:got (format-duration (time-entries-minutes
                                                     entries)))
               (assert-equal #:name "and as the hours a report adds up in"
                             #:expect "2.75"
                             #:got (format-hours (time-entries-minutes
                                                  entries))))))

(define (spec)
  (veritas-run parsing-suite refusal-suite formatting-suite hours-suite
               total-suite))
