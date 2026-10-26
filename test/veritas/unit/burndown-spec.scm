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

;;; The days a plan is made of, and the line a sprint is burnt down along.
;;;
;;; Two things are checked here, and they are the two things that decide
;;; whether a chart says anything true.
;;;
;;; The first is that a day is a day.  A deadline, a sprint's start and the day
;;; time was spent are all submitted by a form, so what arrives is whatever was
;;; sent: the 31st of February has to be refused rather than rolled into March,
;;; and a value that is not a day at all has to be refused rather than read as
;;; one.  The arithmetic on days is exact -- two days subtract to a whole
;;; number of days, whatever the month lengths and leap years between them.
;;;
;;; The second is the burndown, which is replayed from the audit trail rather
;;; than kept in a table of its own.  What is checked is that the replay tells
;;; the story the events describe: work finished on a day is burnt on that day,
;;; work taken out of a sprint stops counting from the day it left, and a
;;; ticket that was reopened is not still counted as done.
;;;
;;; Nothing here touches a database or a request: the tickets are association
;;; lists, the events are association lists, and a day is a string.
;;;
;;; The module is named `(veritas unit burndown-spec)' because that is the name
;;; the veritas runner derives from where the file sits (test/veritas/unit),
;;; which is how `maak test' loads it.

(define-module (veritas unit burndown-spec)
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (lucidplan domain)
  #:export (spec))

;;; ====== a day, read from a form ======

(define date-suite
  (suite "the day a submitted date names"
         (test "a day written the way a date field writes it is read"
               (assert-equal #:name "an ordinary day"
                             #:expect "2026-10-27"
                             #:got (parse-iso-date "2026-10-27"))
               (assert-equal #:name "space around it is not part of it"
                             #:expect "2026-10-27"
                             #:got (parse-iso-date "  2026-10-27 ")))
         (test "a day that does not exist is refused"
               ;; The calendar library would happily roll these into the month
               ;; after, which would silently move a deadline nobody chose.
               (assert-equal #:name "the 31st of February is not a day"
                             #:expect #f
                             #:got (parse-iso-date "2026-02-31"))
               (assert-equal #:name "the 31st of April is not a day"
                             #:expect #f
                             #:got (parse-iso-date "2026-04-31"))
               (assert-equal #:name "the 29th of February 2026 is not a day"
                             #:expect #f
                             #:got (parse-iso-date "2026-02-29"))
               (assert-equal #:name "the 29th of February 2028 is a leap day"
                             #:expect "2028-02-29"
                             #:got (parse-iso-date "2028-02-29")))
         (test "what is not a date is refused"
               (assert-equal #:name "an empty form is no date"
                             #:expect #f
                             #:got (parse-iso-date ""))
               (assert-equal #:name "no value at all is no date"
                             #:expect #f
                             #:got (parse-iso-date #f))
               (assert-equal #:name "a date without its day is refused"
                             #:expect #f
                             #:got (parse-iso-date "2026-10"))
               (assert-equal #:name
                             "a date written the other way round is refused"
                             #:expect #f
                             #:got (parse-iso-date "27/10/2026"))
               (assert-equal #:name "a month that does not exist is refused"
                             #:expect #f
                             #:got (parse-iso-date "2026-13-01")))))

(define arithmetic-suite
  (suite "counting the days between two of them"
         (test "a week is seven days"
               (assert-equal #:name "seven days"
                             #:expect 7
                             #:got (iso-date-diff "2026-10-27" "2026-11-03")))
         (test "the answer is negative when the second day is earlier"
               (assert-equal #:name "a week the other way"
                             #:expect -7
                             #:got (iso-date-diff "2026-11-03" "2026-10-27")))
         (test "month lengths and leap years do not disturb the count"
               (assert-equal #:name "across the end of a short month"
                             #:expect 7
                             #:got (iso-date-diff "2026-02-25" "2026-03-04"))
               (assert-equal #:name "across a leap day"
                             #:expect 2
                             #:got (iso-date-diff "2028-02-28" "2028-03-01"))
               (assert-equal #:name "across a new year"
                             #:expect 2
                             #:got (iso-date-diff "2026-12-31" "2027-01-02")))
         (test "the same day is no days apart"
               (assert-equal #:name "a day and itself"
                             #:expect 0
                             #:got (iso-date-diff "2026-10-27" "2026-10-27")))
         (test "a value that is not a day is no difference"
               (assert-equal #:name "two things that are not days"
                             #:expect #f
                             #:got (iso-date-diff "not a day" "2026-10-27")))
         (test "a day and its number are each other's"
               (assert-equal #:name "the number of a day names it back"
                             #:expect "2026-10-27"
                             #:got (day->iso-date (iso-date->day
                                                   "2026-10-27"))))))

;;; ====== how a deadline stands ======

(define deadline-suite
  (suite "how a deadline stands, read against today"
         (test "a day that has passed is overdue"
               (assert-equal #:name "yesterday is overdue"
                             #:expect 'overdue
                             #:got (ticket-due-state "2026-10-26"
                                                     "2026-10-27")))
         (test "today is today"
               (assert-equal #:name "the day itself"
                             #:expect 'due-today
                             #:got (ticket-due-state "2026-10-27"
                                                     "2026-10-27")))
         (test "a day within the warning is due soon"
               (assert-equal #:name "a week out is close enough to worry about"
                             #:expect 'due-soon
                             #:got (ticket-due-state "2026-11-03"
                                                     "2026-10-27"))
               (assert-equal #:name "tomorrow is close enough too"
                             #:expect 'due-soon
                             #:got (ticket-due-state "2026-10-28"
                                                     "2026-10-27")))
         (test "a day further out is simply scheduled"
               (assert-equal #:name "the day after the warning"
                             #:expect 'scheduled
                             #:got (ticket-due-state "2026-11-04"
                                                     "2026-10-27")))
         (test "a ticket with no deadline has no state"
               (assert-equal #:name "no deadline answers nothing"
                             #:expect #f
                             #:got (ticket-due-state #f "2026-10-27"))
               (assert-equal #:name
                             "a deadline that is not a day answers nothing"
                             #:expect #f
                             #:got (ticket-due-state "soon" "2026-10-27")))
         (test "the warning is a week, and it is the same week everywhere"
               (assert-equal #:name "the threshold the pages share"
                             #:expect 7
                             #:got deadline-soon-days))))

;;; ====== a sprint's ladder ======

(define sprint-status-suite
  (suite "the states a sprint can be in"
         (test "the three states are the shipped ladder"
               (assert-equal #:name "planned, active, closed"
                             #:expect '("planned" "active" "closed")
                             #:got sprint-statuses))
         (test "a state on the ladder is taken as it is stored"
               (assert-equal #:name "an ordinary state"
                             #:expect "active"
                             #:got (parse-sprint-status "active"))
               (assert-equal #:name "a state typed in capitals means the same"
                             #:expect "closed"
                             #:got (parse-sprint-status "CLOSED")))
         (test "a state nobody offers is refused"
               (assert-equal #:name "a state off the ladder is refused"
                             #:expect #f
                             #:got (parse-sprint-status "abandoned"))
               (assert-equal #:name "something that is not text is refused"
                             #:expect #f
                             #:got (parse-sprint-status 'active)))
         (test "only one state is the one a team is in"
               (assert-equal #:name "active is running"
                             #:expect #t
                             #:got (sprint-running? "active"))
               (assert-equal #:name "planned is not"
                             #:expect #f
                             #:got (sprint-running? "planned")))
         (test "a stretch of days has to run forwards"
               (assert-equal #:name "an end before its beginning is refused"
                             #:expect #f
                             #:got (sprint-spans-valid? "2026-11-03"
                                                        "2026-10-27"))
               (assert-equal #:name "an undated sprint is allowed"
                             #:expect #t
                             #:got (sprint-spans-valid? #f #f))
               (assert-equal #:name "an open-ended sprint is allowed"
                             #:expect #t
                             #:got (sprint-spans-valid? "2026-10-27" #f)))))

;;; ====== the burndown ======

;; A moment, as the schema stores one: seconds since the epoch.  The events
;; below carry timestamps because that is what the audit trail holds, and the
;; burndown has to put each one on the day it happened.
(define (seconds-of iso)
  (inexact->exact (time-second (date->time-utc (apply make-date
                                                      (append '(0 0 0 0)
                                                              (reverse (map
                                                                        string->number
                                                                        (string-split
                                                                         iso
                                                                         #\-)))
                                                              '(0)))))))

(define (a-ticket id points status)
  `((id . ,id) (estimate-points . ,points) (ticket-status . ,status)))

(define (a-sprint-event ticket-id day action sprint-id)
  `((ticket-id . ,ticket-id)
    (action . ,action)
    (field . "sprint")
    (old-value . #f)
    (new-value . ,sprint-id)
    (created-at . ,(seconds-of day))))

(define (a-status-event ticket-id day old new)
  `((ticket-id . ,ticket-id)
    (action . "changed")
    (field . "status")
    (old-value . ,old)
    (new-value . ,new)
    (created-at . ,(seconds-of day))))

;; A five-day sprint, read on its third day, with two tickets in it: five
;; points finished on the second day and eight points still open.  Thirteen
;; points committed, five burnt by the second day, eight left -- which is what
;; the chart has to say on each of the days it draws.
(define week-starts-on "2026-10-01")
(define week-ends-on "2026-10-05")
(define week-today "2026-10-03")

(define week-tickets
  (list (a-ticket "t1" 5 "done")
        (a-ticket "t2" 8 "work-in-progress")))

(define week-events
  (list (a-sprint-event "t1" "2026-10-01" "sprint-added" "s1")
        (a-sprint-event "t2" "2026-10-01" "sprint-added" "s1")
        (a-status-event "t1" "2026-10-02" "work-in-progress" "done")))

(define* (series-of tickets events #:key (starts-on week-starts-on)
                    (ends-on week-ends-on) (today week-today))
  (burndown-series #:sprint-id "s1"
                   #:starts-on starts-on
                   #:ends-on ends-on
                   #:tickets tickets
                   #:events events
                   #:today today))

(define (on-day rows day)
  (find (lambda (row)
          (equal? (assq-ref row
                            'day) day)) rows))

(define (days-of rows)
  (map (lambda (row)
         (assq-ref row
                   'day)) rows))

(define burndown-suite
  (suite "the line a sprint is burnt down along"
         (test "a day is drawn up to today and no further"
               ;; A chart has no business drawing the future as though it had
               ;; happened, so the series stops at the day it is read on.
               (assert-equal #:name "three days of a five-day sprint"
                             #:expect '("2026-10-01" "2026-10-02" "2026-10-03")
                             #:got (days-of (series-of week-tickets
                                                       week-events))))
         (test "a sprint with no dates is not drawn"
               (assert-equal #:name "no days to draw"
                             #:expect '()
                             #:got (burndown-days #f #f "2026-10-03")))
         (test "a sprint whose end is before its beginning is not drawn"
               (assert-equal #:name "a backwards sprint draws nothing"
                             #:expect '()
                             #:got (burndown-days "2026-11-03" "2026-10-27"
                                                 "2026-10-27")))
         (test "a closed sprint is drawn to its last day"
               ;; Nothing is cut off a sprint that is over: its chart is the
               ;; record of the days it ran, and today has nothing to say about
               ;; it.
               (assert-equal #:name "all five days, read afterwards"
                             #:expect 5
                             #:got (length (series-of week-tickets week-events
                                                      #:today "2026-12-31"))))
         (test "work finished on a day is burnt on that day, not before"
               (let ((rows (series-of week-tickets week-events)))
                 (assert-equal #:name "nothing is done on the first day"
                               #:expect 13
                               #:got (assq-ref (on-day rows "2026-10-01")
                                               'remaining))
                 (assert-equal #:name "five points are gone on the second"
                               #:expect 8
                               #:got (assq-ref (on-day rows "2026-10-02")
                                               'remaining))
                 (assert-equal #:name "and they stay gone on the third"
                               #:expect 8
                               #:got (assq-ref (on-day rows "2026-10-03")
                                               'remaining))))
         (test "the point a day started with is the scope and what was done"
               (let ((row (on-day (series-of week-tickets week-events)
                                  "2026-10-02")))
                 (assert-equal #:name "thirteen points were committed"
                               #:expect 13
                               #:got (assq-ref row
                                               'scope))
                 (assert-equal #:name "five of them were done"
                               #:expect 5
                               #:got (assq-ref row
                                               'completed))))
         (test "the committed line falls evenly to nothing on the last day"
               ;; Thirteen points over four days between the first and the
               ;; last: 13, 9.75, 6.5, 3.25, 0 -- so the gap between the two
               ;; lines is what a reader actually reads.
               (let ((rows (series-of week-tickets week-events)))
                 (assert-equal #:name "the first day is the whole commitment"
                               #:expect 13.0
                               #:got (assq-ref (on-day rows "2026-10-01")
                                               'ideal))
                 (assert-equal #:name "the second day is three quarters of it"
                               #:expect 9.75
                               #:got (assq-ref (on-day rows "2026-10-02")
                                               'ideal))
                 (assert-equal #:name "the third day is half of it"
                               #:expect 6.5
                               #:got (assq-ref (on-day rows "2026-10-03")
                                               'ideal))))
         (test "a ticket taken out of the sprint stops counting"
               ;; The eight open points leave the sprint on the second day, so
               ;; the scope shrinks with them rather than the chart pretending
               ;; they were finished.
               (let* ((events (append week-events
                                      (list (a-sprint-event "t2" "2026-10-02"
                                                            "sprint-removed"
                                                            "s1"))))
                      (rows (series-of week-tickets events)))
                 (assert-equal #:name
                               "the scope still holds it on the first day"
                               #:expect 13
                               #:got (assq-ref (on-day rows "2026-10-01")
                                               'scope))
                 (assert-equal #:name "it is gone from the second day on"
                               #:expect 5
                               #:got (assq-ref (on-day rows "2026-10-02")
                                               'scope))
                 (assert-equal #:name "and nothing but the done ticket is left"
                               #:expect 0
                               #:got (assq-ref (on-day rows "2026-10-02")
                                               'remaining))))
         (test "a ticket that was reopened is not still counted as done"
               (let* ((events (append week-events
                                      (list (a-status-event "t1" "2026-10-03"
                                                            "done"
                                                            "work-in-progress"))))
                      (rows (series-of week-tickets events)))
                 (assert-equal #:name "five points are burnt on the second day"
                               #:expect 8
                               #:got (assq-ref (on-day rows "2026-10-02")
                                               'remaining))
                 (assert-equal #:name "and they come back when the ticket does"
                               #:expect 13
                               #:got (assq-ref (on-day rows "2026-10-03")
                                               'remaining))))
         (test "an unestimated ticket adds nothing to the sum"
               ;; An unestimated ticket is a ticket nobody has sized, not a
               ;; ticket worth zero: it cannot be added to the same sum as the
               ;; tickets that were estimated.
               (let ((rows (series-of (list (a-ticket "t1" 5 "work-in-progress")
                                            (a-ticket "t2" #f "work-in-progress"))
                                      week-events)))
                 (assert-equal #:name "the unsized ticket is not counted"
                               #:expect 5
                               #:got (assq-ref (on-day rows "2026-10-01")
                                               'scope))))
         (test "the summary is the last day said once"
               (let* ((rows (series-of week-tickets week-events))
                      (summary (burndown-summary rows)))
                 (assert-equal #:name "thirteen committed"
                               #:expect 13
                               #:got (assq-ref summary
                                               'scope))
                 (assert-equal #:name "five done"
                               #:expect 5
                               #:got (assq-ref summary
                                               'completed))
                 (assert-equal #:name "eight left"
                               #:expect 8
                               #:got (assq-ref summary
                                               'remaining))))
         (test "a sprint with nothing in it is a flat line at nothing"
               (let ((rows (series-of '() '())))
                 (assert-equal #:name "every day is zero"
                               #:expect '(0 0 0)
                               #:got (map (lambda (row)
                                            (assq-ref row
                                                      'remaining)) rows))))))

(define (spec)
  (veritas-run date-suite arithmetic-suite deadline-suite sprint-status-suite
               burndown-suite))
