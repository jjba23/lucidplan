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

(define-module (lucidplan domain)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (lucidplan config))

;;; ===== The statuses a ticket moves through =====
;;;
;;; What a status is called, in what order the statuses come, which bucket
;;; each belongs to and what marks it on the page are one list, and it is
;;; configuration: an instance sets `ticket-statuses' in its own `env.scm'
;;; (see `(lucidplan config)'), and a project may override the list for
;;; itself (see `effective-ticket-statuses' and the project edit page).
;;;
;;; Each entry is `(value label icon glyph bucket)': the value the schema
;;; stores, the label a reader is shown, the name of a Phosphor icon, the
;;; character a native menu can hold, and the bucket the status belongs to.
;;; A bucket is `"open"', `"done"' or `"rejected"':
;;;
;;;   open      a status a ticket is still being worked on in -- a column of
;;;             the board, and a row of the backlog's live list;
;;;   done      a status a ticket is finished and accepted in;
;;;   rejected  a status a ticket is finished and turned down in.
;;;
;;; The closed buckets are what the backlog's completion bar and its closed
;;; sections are built from, and what its Done and Rejected checkboxes
;;; reveal.  A status the list does not name is treated as open, so a ticket
;;; whose status has been taken off the list stays visible and can be moved
;;; back onto it rather than vanishing.

(define *status-buckets*
  '("open" "done" "rejected"))

(define-public (default-ticket-statuses)
  "Answer the statuses this instance ships with, as configuration defines
them: the list a project falls back to when it has none of its own."
  (config-ref 'ticket-statuses))

;; One field of one status entry, or FALLBACK when the entry is missing or
;; shorter than the field -- which is what a hand-written override may be.
(define (status-field spec n fallback)
  (let ((value (and (list? spec)
                    (> (length spec) n)
                    (list-ref spec n))))
    (if (string? value) value fallback)))

(define-public (ticket-status-spec specs value)
  "Answer the entry of SPECS that describes VALUE, or @code{#f}."
  (find (lambda (spec)
          (and (pair? spec)
               (equal? (car spec) value))) specs))

(define-public (ticket-status-values specs)
  "Answer the stored values SPECS names, in their order."
  (map car specs))

(define-public (ticket-status-bucket specs value)
  "Answer the bucket the status VALUE belongs to, as a string.

A value SPECS does not name is answered as `\"open\"': a ticket carrying a
status that has been taken off the list is still a ticket somebody is
working on, and keeping it on the board is what lets it be moved back onto
the list."
  (let ((bucket (status-field (ticket-status-spec specs value) 4 "open")))
    (if (member bucket *status-buckets*) bucket "open")))

(define-public (ticket-status-label specs value)
  "Answer the label a reader is shown for VALUE, or VALUE itself."
  (status-field (ticket-status-spec specs value) 1 value))

(define-public (ticket-status-icon specs value)
  "Answer the Phosphor icon name VALUE is marked with, or a plain dot."
  (status-field (ticket-status-spec specs value) 2 "circle"))

(define-public (ticket-status-glyph specs value)
  "Answer the character VALUE is marked with in a native menu, or nothing."
  (status-field (ticket-status-spec specs value) 3 ""))

(define-public (ticket-status-in-bucket? specs value bucket)
  "Is VALUE, as SPECS describes it, in BUCKET?"
  (equal? bucket
          (ticket-status-bucket specs value)))

(define-public (ticket-status-values-in-bucket specs bucket)
  "The values of SPECS that belong to BUCKET, in their order."
  (filter (lambda (value)
            (ticket-status-in-bucket? specs value bucket))
          (ticket-status-values specs)))

;;; The editor's format, read back.
;;;
;;; A project that defines its own statuses keeps them as the text the
;;; operator wrote, so the editor round-trips exactly what was typed; this
;;; is what turns that text back into the list everything else uses.
(define-public (parse-ticket-statuses text)
  "Parse TEXT -- one status to a line, its fields separated by `|' -- into a
list of status entries, or @code{#f} when TEXT names nothing.

The format is the one the project edit page offers:

@example
todo | To-do | anchor | ⚓ | open
done | Done  | check  | ✅ | done
@end example

A line with no value, and a line beginning with `#', is skipped -- the editor
is a place to leave a note as well as a status.  A missing label, icon, glyph
or bucket takes the same default the accessors use (the value itself,
`circle', nothing, `open'), and text that parses to no entries at all answers
@code{#f}, which is how a project with no list of its own falls back to the
instance's."
  (if (or (not (string? text))
          (string-null? (string-trim-both text))) #f
      (let ((entries (filter-map (lambda (line)
                                   (let* ((fields (map string-trim-both
                                                       (string-split (string-trim-both
                                                                      line)
                                                                     #\|)))
                                          (value (car fields))
                                          (field (lambda (n fallback)
                                                   (let ((v (and (> (length
                                                                     fields) n)
                                                                 (list-ref
                                                                  fields n))))
                                                     (if (and (string? v)
                                                              (not (string-null?
                                                                    v))) v
                                                         fallback)))))
                                     (and (not (string-null? value))
                                          (not (string-prefix? "#" value))
                                          (let ((bucket (field 4 "open")))
                                            (list value
                                                  (field 1 value)
                                                  (field 2 "circle")
                                                  (field 3 "")
                                                  (if (member bucket
                                                       *status-buckets*)
                                                      bucket "open"))))))
                                 (string-split text #\newline))))
        (and (pair? entries) entries))))

(define-public (effective-ticket-statuses project)
  "Answer the statuses a PROJECT's tickets move through.

A project that carries a list of its own -- the `ticket_statuses' column the
project edit page writes -- uses it; any other project uses the instance's
own list.  PROJECT is a project row as the repository answers it, and may be
@code{#f}, which is simply a project with no override."
  (or (parse-ticket-statuses (and project
                                  (assq-ref project
                                            'ticket-statuses)))
      (default-ticket-statuses)))

(define-public (known-user-id user-ids submitted)
  "Answer the user id a submitted assignee names, or @code{#f}.

A dropdown offers the users that exist, but a form is not a promise: the
value that arrives is whatever was sent, so it is checked here against the
ids that were offered rather than trusted.  A value naming one of
@var{user-ids} is returned -- the id itself, so a caller can act on the
answer -- and anything else, an unknown id or a value that is not text at
all, answers @code{#f}, which is what leaves a ticket as it was.

An empty string is not an unknown user: it is the row that puts a ticket
back to nobody, and it answers @code{#f} here because the absence of an
assignee is the absence of a row rather than a row naming no one."
  (and (string? submitted)
       (not (string-null? submitted))
       (member submitted user-ids) submitted))

;; The other ladder a ticket is triaged with -- its type -- which is not
;; configuration: nothing on a ticket is grouped or counted by its type, so
;; the list is simply the values the schema holds, and two things need to
;; agree about it: the menus that offer them, and the check that refuses a
;; value nothing offered.
(define-public ticket-types
  '("task" "bug" "story" "spike" "time-box"))

;;; ===== The two estimates a ticket carries =====
;;;
;;; A ticket is estimated in story points -- the numbers a team burns down --
;;; and in t-shirt sizes, which is what a first pass through a backlog is
;;; usually written in.  Both are optional: an unestimated ticket is a ticket
;;; nobody has looked at yet, and is stored as NULL rather than as zero.
;;;
;;; The two ladders below are what the menus offer and what a submitted value
;;; is checked against, exactly as the status and type ladders are.  Changing
;;; what an instance estimates with therefore means editing these lists, and
;;; the pages, the forms and the endpoints all follow.
(define-public ticket-points
  '(0 1
    2
    3
    5
    8
    13
    21
    34))

(define-public ticket-sizes
  '("xs" "s" "m" "l" "xl" "xxl"))

(define-public (ticket-size-label size)
  "Answer the label a reader is shown for the t-shirt size SIZE."
  (cond
    ((equal? size "xs")
     "XS")
    ((equal? size "s")
     "S")
    ((equal? size "m")
     "M")
    ((equal? size "l")
     "L")
    ((equal? size "xl")
     "XL")
    ((equal? size "xxl")
     "XXL")
    (else size)))

(define-public (parse-estimate-points value)
  "Answer the story-point estimate VALUE names, or @code{#f} for no estimate.

A form is not a promise, so the value that arrives is checked against the
ladder the menu offered: a number on it is answered as that number, an empty
value is answered as @code{#f} -- an unestimated ticket -- and anything else
is @code{#f} too, which is what leaves a ticket's estimate as it was."
  (let ((number (cond
                  ((number? value)
                   value)
                  ((string? value)
                   (let ((trimmed (string-trim-both value)))
                     (and (not (string-null? trimmed))
                          (string->number trimmed))))
                  (else #f))))
    (and number
         (member number ticket-points) number)))

(define-public (parse-estimate-size value)
  "Answer the t-shirt size VALUE names, or @code{#f} for no estimate.

The comparison is case-insensitive, so a hand-typed form may send @code{L}
and mean @code{l}; a value the ladder does not name answers @code{#f}."
  (and (string? value)
       (let ((size (string-downcase (string-trim-both value))))
         (and (member size ticket-sizes) size))))

;;; ===== What a ticket's history is made of =====
;;;
;;; A tracked field is a column and the name a reader knows it by.  The audit
;;; trail records a change as one row per field, and a version is a snapshot
;;; of these same columns, so both use this one list and neither can quietly
;;; stop watching a column the other still versions.
(define-public ticket-tracked-fields
  '((title "title")
    (description "description")
    (ticket-status "status")
    (ticket-type "type")
    (priority "priority")
    (estimate-points "story points")
    (estimate-size "size")))

(define-public (tracked-value value)
  "Answer VALUE as the text a change is recorded with.

A missing value, and the empty string a form sends for one, are both the
empty text: a ticket that never had an estimate and one whose estimate was
just cleared are the same state, and the trail should say so once rather
than twice."
  (cond
    ((not value)
     "")
    ((string? value)
     value)
    ((null? value)
     "")
    (else (format #f "~a" value))))

(define-public (ticket-changes old new)
  "Answer the tracked fields that differ between OLD and NEW.

OLD is a ticket (or a version snapshot) as the repository answers it; NEW is
an association list of the values the ticket is being given, keyed by the
same column names.  The answer is a list of @code{(column label before
after)} entries, one per field that actually changed -- a form submitted
without a change is not an edit, and writes no history."
  (filter-map (lambda (field)
                (let* ((column (car field))
                       (label (cadr field))
                       (before (tracked-value (assq-ref (or old
                                                            '()) column)))
                       (after (tracked-value (assq-ref new column))))
                  (and (not (equal? before after))
                       (list column label before after))))
              ticket-tracked-fields))

(define (ticket-ladder field specs)
  "Answer the column a submitted inline change names and the values it may
hold, as the pair @code{(column . values)}, or @code{#f} for a field a ticket
has no ladder for.

The status ladder is the one @var{specs} describes -- the project's own list
or the instance's -- so an inline change is checked against exactly the
statuses the menu that offered it was built from."
  (cond
    ((equal? field "status")
     (cons 'ticket-status
           (ticket-status-values specs)))
    ((equal? field "type")
     (cons 'ticket-type ticket-types))
    ((equal? field "estimate-points")
     (cons 'estimate-points
           (cons ""
                 (map (lambda (points)
                        (number->string points)) ticket-points))))
    ((equal? field "estimate-size")
     (cons 'estimate-size
           (cons "" ticket-sizes)))
    (else #f)))

(define-public (ticket-field-change field value . statuses-arg)
  "Answer the column change a submitted inline edit names, or @code{#f}.

The status and the type of a ticket are changed from the ticket's own page by
menus that post what they show, and a form is not a promise: the value that
arrives is whatever was sent.  @var{field} must therefore name one of the two
ladders a ticket is triaged with, and @var{value} must be one of the rows that
ladder offers -- the status ladder being STATUSES-ARG, the statuses the
project in question actually moves through, or the instance's own when it is
left out.  Anything else -- an unknown field, a status invented by hand, a
value that is not text at all -- answers @code{#f}, which is what leaves a
ticket as it was.

The answer is the change itself, an association list of one column and its new
value, so a caller can hand it straight to the repository rather than deciding
a second time which column moves."
  (let* ((statuses (if (pair? statuses-arg)
                       (car statuses-arg)
                       (default-ticket-statuses)))
         (ladder (ticket-ladder field statuses)))
    (and ladder
         (string? value)
         (member value
                 (cdr ladder))
         (list (cons (car ladder) value)))))

(define-public (entity-sort-from-query user-sort)
  "Translate a sort keyword taken from the query string into the ORDER BY spec
kracht's `sql-select' accepts: a list of (column [direction]) pairs.  An
unknown (or missing) keyword falls back to the most recently edited first."
  (cond
    ((equal? "created-at-desc" user-sort)
     '((created-at desc)))
    ((equal? "created-at-asc" user-sort)
     '((created-at asc)))
    ((equal? "last-edited-at-desc" user-sort)
     '((updated-at desc)))
    ((equal? "last-edited-at-asc" user-sort)
     '((updated-at asc)))
    ((equal? "title-desc" user-sort)
     '((title desc)))
    ((equal? "title-asc" user-sort)
     '((title asc)))
    ((equal? "priority-desc" user-sort)
     '((priority desc)))
    ((equal? "priority-asc" user-sort)
     '((priority asc)))
    (else '((updated-at desc)))))

;;; ===== The days a plan is made of =====
;;;
;;; Three things below are dates rather than moments -- the day a sprint
;;; starts, the day a ticket is due, the day somebody did the work -- and
;;; they are read and written as the ISO text `2026-10-27', the form the
;;; schema stores and an `<input type="date">' speaks.  What is here is what
;;; turns submitted text into that form, and what does arithmetic on it: a
;;; form is not a promise, so a date that arrives is checked by writing it
;;; back out and seeing whether it says the same thing, which is what
;;; refuses the 31st of February rather than quietly rolling it into March.
;;;
;;; A timestamp -- when a row was written -- is the other kind of value: it
;;; is the number of seconds since the epoch, the way every `created_at' in
;;; this schema holds it.  `seconds->iso-date' is the bridge, because the
;;; burndown below has to put the moment a ticket was finished onto the day
;;; it was finished.

(define-public (current-time-seconds)
  "Answer the moment it is, as the number of seconds since the epoch.

`current-time' is what the rest of the application writes into a row, so
this is the same value said as one number, which is what arithmetic can be
done on: everything that reads a timestamp back -- a sprint's history, a
ticket's status changes -- has to be able to compare and subtract them."
  (let ((now (current-time)))
    (if (number? now) now
        (time-second now))))

(define-public (seconds->iso-date seconds)
  "Answer the ISO day SECONDS names, or @code{#f} when it names no moment.

The conversion is made in the server's own timezone, so a change recorded
late in the evening is the day the reader remembers making it, and so it
agrees with @code{today-iso} on what day it is now."
  (and (number? seconds)
       (date->string (time-utc->date (make-time 'time-utc
                                                0
                                                (inexact->exact (round
                                                                 seconds))))
                     "~Y-~m-~d")))

(define (leap-year? year)
  "Is YEAR a leap year, by the rule the Gregorian calendar actually uses?"
  (and (zero? (remainder year 4))
       (or (not (zero? (remainder year 100)))
           (zero? (remainder year 400)))))

(define (days-in-month year month)
  "Answer how many days MONTH of YEAR has."
  (cond
    ((= month 2) (if (leap-year? year) 29 28))
    ((member month '(4 6 9 11)) 30)
    (else 31)))

(define (iso-date-parts value)
  "Answer the year, month and day VALUE names as a list, or @code{#f}.

VALUE has to be exactly `YYYY-MM-DD', zero-padded, and has to name a day
that exists.  The check is made against the length of the month rather than
by writing the day back out: the calendar library keeps a 31st of February
as a 31st of February, so a round trip would agree with itself about a day
that never happened."
  (and (string? value)
       (let* ((text (string-trim-both value))
              (m (string-match "^([0-9]{4})-([0-9]{2})-([0-9]{2})$" text)))
         (and m
              (let ((year (string->number (match:substring m 1)))
                    (month (string->number (match:substring m 2)))
                    (day (string->number (match:substring m 3))))
                (and (>= month 1)
                     (<= month 12)
                     (>= day 1)
                     (<= day (days-in-month year month))
                     (list year month day)))))))

(define-public (parse-iso-date value)
  "Answer VALUE as the ISO day it names, or @code{#f} for no day.

This is what an endpoint does with a submitted date: a form is not a
promise, so the value that arrives is whatever was sent, and anything that
is not a day written as `YYYY-MM-DD' answers @code{#f} -- which is what
leaves the column it was meant for as it was."
  (and (iso-date-parts value)
       (string-trim-both value)))

(define-public (iso-date->day value)
  "Answer the Julian day number VALUE names, or @code{#f} for no day.

A Julian day is a whole number per calendar day, so two of them subtract
to the number of days between them with no month lengths or leap years to
think about.  The arithmetic stays exact -- the answer is a number, and one
day is one added to it."
  (let ((parts (iso-date-parts value)))
    (and parts
         (date->julian-day (apply make-date
                                  (append '(0 0 0 0)
                                          (reverse parts)
                                          '(0)))))))

(define-public (day->iso-date day)
  "Answer DAY, a Julian day number, as the ISO day it names."
  (and (number? day)
       (date->string (julian-day->date day 0) "~Y-~m-~d")))

(define-public (iso-date-diff from to)
  "Answer how many days separate FROM and TO, or @code{#f}.

The answer is negative when TO is the earlier of the two, so a caller can
ask which end of a plan a day falls on with one subtraction."
  (let ((a (iso-date->day from))
        (b (iso-date->day to)))
    (and a b
         (- b a))))

(define-public (today-iso)
  "Answer the day it is, as the ISO text a date column holds."
  (date->string (current-date) "~Y-~m-~d"))

(define-public (iso-date-compare a b)
  "Answer how the ISO days A and B order, as a number that sorts them.

The text of an ISO day sorts the way the day sorts, so this is a string
comparison with the numbers spelled out: a negative answer means A is the
earlier day, zero that they are the same day, and a positive one that A is
the later.  A value that is not a day is treated as the empty text, which
sorts before every day -- an absent date is not a day in the future."
  (let ((left (or (and (string? a)
                       a) ""))
        (right (or (and (string? b)
                        b) "")))
    (cond
      ((string<? left right) -1)
      ((string>? left right) 1)
      (else 0))))

;;; ===== The time somebody spent on a ticket =====
;;;
;;; A time entry is a duration and a comment, and the duration is written
;;; the way people say it rather than the way a form would prefer: an hour
;;; and a half is `1.5h', `90m', `1h30m', `1h 30m' or `1:30', and a reader
;;; who types any of them means the same ninety minutes.  All of them are
;;; read here, into the one thing the schema can hold exactly: a whole
;;; number of minutes.
;;;
;;; What is *not* a duration is as important as what is.  Nothing, an empty
;;; form and zero all answer @code{#f}: logging no time is not logging, and
;;; an entry of zero minutes would sit in the list saying nothing happened
;;; while counting toward nothing.  A day's worth is the ceiling -- no
;;; honest entry is longer than the day it happened in -- so a typo that
;;; turns 90 minutes into 90 hours is refused rather than stored.

(define-public time-entry-max-minutes
  ;; Twenty-four hours, in minutes.
  1440)

;; One `<amount><unit>' of a duration, and whatever follows it.  The units
;; are listed longest first so that `1hour' is read as an hour rather than
;; as an `h' followed by `our', and a bare number is a number of minutes.
(define duration-part-rx
  (make-regexp
   "^([0-9]+(\\.[0-9]+)?)(hours|hour|hrs|hr|h|minutes|minute|mins|min|m)?(.*)$"))

(define (duration-unit-in-hours? unit)
  "Is UNIT the name of an hour -- `h', `hr', `hrs', `hour', `hours'?"
  (and (string? unit)
       (not (string-null? unit))
       (char=? (string-ref unit 0)
               #\h)))

;; A duration written as a clock reading -- `1:30' -- which is read only as
;; hours and minutes, and only when the minutes are a real minute of the
;; hour: `1:99' is not one hundred and thirty-nine minutes, it is a typo.
(define (parse-clock-duration text)
  (let ((m (string-match "^([0-9]+):([0-9]{2})$" text)))
    (and m
         (let ((minutes (string->number (match:substring m 2))))
           (and (< minutes 60)
                (+ (* 60 (string->number (match:substring m 1)))
                   minutes))))))

;; A duration written as a run of amounts and units -- `1h30m', `90m',
;; `2 hours' with the spaces already taken out -- read one part at a time
;; until nothing is left.  A character that names no part answers #f rather
;; than being skipped, so `1h!' is refused rather than read as an hour.
(define (parse-unit-duration text)
  (let loop
    ((rest text)
     (minutes 0))
    (cond
      ((string-null? rest)
       (round minutes))
      (else (let ((m (regexp-exec duration-part-rx rest)))
              (and m
                   (let ((amount (string->number (match:substring m 1)))
                         (unit (match:substring m 3)))
                     (and amount
                          (loop (match:substring m 4)
                                (+ minutes
                                   (* (if (duration-unit-in-hours? unit)
                                          60
                                          1) amount)))))))))))

(define-public (parse-duration value)
  "Answer the number of minutes VALUE names, or @code{#f} for no duration.

VALUE is what somebody typed into the time form, and it is read the ways a
duration is written:

@example
1.5h   1h30m   1h 30m   90m   1:30   90
@end example

A whole number of minutes is the answer, and it has to be more than none
and no more than a day (@code{time-entry-max-minutes}): zero is not a
duration anybody logged, and more than a day is a typo rather than a day's
work.  Anything else -- words, a negative amount, a clock reading whose
minutes are not minutes -- answers @code{#f}, which is what leaves the
ticket's time log without an entry."
  (and (string? value)
       (let ((text (string-delete char-set:whitespace
                                  (string-downcase (string-trim-both value)))))
         (and (not (string-null? text))
              (let ((minutes (or (parse-clock-duration text)
                                 (parse-unit-duration text))))
                (and (number? minutes)
                     (> minutes 0)
                     (<= minutes time-entry-max-minutes)
                     (inexact->exact minutes)))))))

(define-public (format-duration minutes)
  "Answer MINUTES as the text a reader reads it as: `1h 30m', `45m', `2h'.

An absent or nonsensical number answers the empty text, so a page can put
the answer where a duration belongs without checking first."
  (if (or (not (number? minutes))
          (< minutes 0)) ""
      (let* ((whole (inexact->exact (round minutes)))
             (hours (quotient whole 60))
             (left (remainder whole 60)))
        (cond
          ((and (zero? hours)
                (zero? left)) "0m")
          ((zero? hours) (format #f "~am" left))
          ((zero? left) (format #f "~ah" hours))
          (else (format #f "~ah ~am" hours left))))))

(define-public (format-hours minutes)
  "Answer MINUTES as the decimal number of hours a report adds up in.

The answer is exact to a hundredth of an hour -- 3.6 seconds, which is as
fine as a time log is ever written -- and it is said without trailing
zeros: an hour is `1' and an hour and a half is `1.5', because a report
that reads `1.0' and `1.5' side by side looks like two different units."
  (if (or (not (number? minutes))
          (< minutes 0)) "0"
      (let* ((hundredths (inexact->exact (round (* 100 (/ minutes 60)))))
             (whole (quotient hundredths 100))
             (part (remainder hundredths 100)))
        (cond
          ((zero? part) (format #f "~a" whole))
          ((zero? (remainder part 10)) (format #f "~a.~a" whole
                                               (quotient part 10)))
          ((< part 10) (format #f "~a.0~a" whole part))
          (else (format #f "~a.~a" whole part))))))

(define-public (time-entries-minutes entries)
  "Answer how many minutes the time ENTRIES add up to.

A row whose duration is missing -- which the schema does not allow but a
hand-written list might carry -- is counted as nothing rather than breaking
the sum, because a total is a report and a report should not fail."
  (let loop
    ((rest entries)
     (total 0))
    (if (null? rest) total
        (let ((minutes (assq-ref (car rest)
                                 'duration-minutes)))
          (loop (cdr rest)
                (if (number? minutes) (+ total minutes) total))))))

;;; ===== How a deadline stands =====
;;;
;;; A due date is read by a person at a glance, so what the page needs is not
;;; the date but how it stands: past, today, close enough to worry about, or
;;; comfortably ahead.  The threshold is one number, kept here so the colour
;;; a deadline is drawn in and the sentence under it cannot disagree about
;;; what "soon" means.

(define-public deadline-soon-days
  ;; A week is what a team can still do something about.
  7)

(define-public (ticket-due-state due-on . today-arg)
  "Answer how the deadline DUE-ON stands, as a symbol, or @code{#f}.

The answer is `overdue' for a day that has passed, `due-today' for today,
`due-soon' for one within @code{deadline-soon-days}, and `scheduled' for
anything further out.  TODAY-ARG is the day to judge by, and defaults to
the day it is; a ticket with no deadline -- or one whose deadline is not a
day at all -- answers @code{#f}, which is what leaves the page without a
badge rather than with a misleading one."
  (let* ((today (if (pair? today-arg) (car today-arg) (today-iso)))
         (due (iso-date->day due-on))
         (now (iso-date->day today)))
    (and due now
         (let ((distance (- due now)))
           (cond
             ((< distance 0) 'overdue)
             ((= distance 0) 'due-today)
             ((<= distance deadline-soon-days) 'due-soon)
             (else 'scheduled))))))

(define-public (due-state-label state)
  "Answer the translation key a reader is shown for a deadline STATE."
  (case state
    ((overdue) "overdue")
    ((due-today) "due today")
    ((due-soon) "due soon")
    ((scheduled) "scheduled")
    (else "")))

;;; ===== Sprints =====
;;;
;;; A sprint is a stretch of work with a beginning, an end, and a state that
;;; says whether it is still being planned, under way, or finished.  The
;;; ladder is deliberately three values long: a fourth state would be a
;;; workflow a team has to agree on, and the point of a sprint is to be the
;;; simplest thing that can be committed to and closed.

(define-public sprint-statuses
  '("planned" "active" "closed"))

(define-public (parse-sprint-status value)
  "Answer the sprint status VALUE names, or @code{#f} for one it does not.

The comparison is case-insensitive and ignores space around the value, so
a hand-written form may send @code{Active} and mean @code{active}; a value
the ladder does not name answers @code{#f}, which is what leaves a sprint
as it was rather than moving it to a state nothing knows how to draw."
  (and (string? value)
       (let ((status (string-downcase (string-trim-both value))))
         (and (member status sprint-statuses) status))))

(define-public (sprint-running? status)
  "Is a sprint in STATUS the one a team is working in?"
  (equal? status "active"))

(define-public (sprint-closed? status)
  "Is a sprint in STATUS finished?"
  (equal? status "closed"))

(define-public (sprint-spans-valid? starts-on ends-on)
  "Do STARTS-ON and ENDS-ON describe a stretch of days a sprint can run over?

Both may be absent -- a sprint is often written down before anybody agrees
on the dates -- and an absent end is an open-ended sprint, which is
allowed.  What is refused is an end that is before its beginning, which is
not a sprint but a typo."
  (let ((start (iso-date->day starts-on))
        (end (iso-date->day ends-on)))
    (or (not start)
        (not end)
        (<= start end))))

;;; ===== Burning a sprint down =====
;;;
;;; A burndown answers one question, day by day: how much work is left.  It
;;; is read off what the application already records rather than kept in a
;;; table of its own -- a snapshot written every night would be a second
;;; copy of the truth, and the first day it disagreed with the tickets
;;; nobody would know which to believe.
;;;
;;; So the series is replayed from the audit trail.  A ticket counts toward
;;; a day when it was in the sprint on that day, and counts as burnt when
;;; its status on that day was one of the project's closed ones.  Both of
;;; those come from events: putting a ticket in a sprint and taking it out
;;; are recorded as `sprint-added' and `sprint-removed', and a status change
;;; is recorded with the statuses it moved between.
;;;
;;; Two things are taken as they are now rather than as they were then, and
;;; they are worth knowing when reading the chart: a ticket's estimate is
;;; the estimate it carries today, and a ticket nothing recorded an entry
;;; for is taken to have been in the sprint from the day the sprint began.
;;; The first is exact whenever an estimate never changed, which is most of
;;; the time; the second is right whenever a ticket was put in the sprint
;;; before the team started burning it down.

(define (same-id? a b)
  "Are A and B the same identifier, written as text?"
  (and a b
       (equal? (format #f "~a" a)
               (format #f "~a" b))))

(define (ticket-points-value ticket)
  "Answer the story points TICKET is estimated at, counting none as zero.

An unestimated ticket contributes nothing to a burndown -- not because it
is worth nothing, but because it cannot be added to the same sum as the
tickets that were estimated.  The chart is drawn over the work somebody has
sized."
  (let ((points (assq-ref ticket
                          'estimate-points)))
    (if (number? points) points 0)))

(define (event-seconds event)
  "Answer when EVENT happened, as seconds since the epoch, or @code{#f}."
  (let ((value (assq-ref event
                         'created-at)))
    (cond
      ((number? value) value)
      ((string? value) (string->number value))
      (else #f))))

(define (event-day event)
  "Answer the ISO day EVENT happened on, or @code{#f}."
  (seconds->iso-date (event-seconds event)))

;; The days a ticket spent in a sprint, as `(added . removed)', either end of
;; which may be #f: #f for the addition means the ticket was in the sprint
;; before the application started recording it, and #f for the removal means
;; it is in the sprint still.
(define (sprint-membership sprint-id events)
  (define (first-day-of action)
    (let ((days (filter-map (lambda (event)
                              (and (equal? (assq-ref event
                                                     'action) action)
                                   (same-id? (assq-ref event
                                                       'new-value) sprint-id)
                                   (event-day event)))
                            events)))
      (and (pair? days) (car (sort days
                                   string<?)))))
  (cons (first-day-of "sprint-added")
        (first-day-of "sprint-removed")))

(define (in-sprint-on-day? membership day)
  "Was the ticket MEMBERSHIP describes in the sprint on DAY?"
  (let ((added (car membership))
        (removed (cdr membership)))
    (and (or (not added)
             (<= (iso-date-compare added day) 0))
         (or (not removed)
             (< (iso-date-compare day removed) 0)))))

;; The status changes a ticket has been through, oldest first, as
;; `(seconds day new-status old-status)'.
(define (status-changes events)
  (sort (filter-map (lambda (event)
                      (and (equal? (assq-ref event
                                             'action) "changed")
                           (equal? (assq-ref event
                                             'field) "status")
                           (let ((seconds (event-seconds event))
                                 (day (event-day event)))
                             (and seconds day
                                  (list seconds day
                                        (assq-ref event
                                                  'new-value)
                                        (assq-ref event
                                                  'old-value))))))
                    events)
        (lambda (a b)
          (< (car a) (car b)))))

;; The status a ticket stood in at the end of DAY: the last change on or
;; before it.  Before the first change it was in the status that change
;; moved it away from; a ticket nothing has ever restatused is in the status
;; it carries now.
(define (ticket-status-on-day ticket changes day)
  (let loop
    ((rest changes)
     (status (if (pair? changes)
                 (car (cdddr (car changes)))
                 (assq-ref ticket
                           'ticket-status))))
    (cond
      ((null? rest) status)
      ;; A change made on the day being measured counts: the ticket was in
      ;; the status it moved to by the end of that day.  Only a change made
      ;; after it is still to come.
      ((> (iso-date-compare (cadr (car rest)) day) 0) status)
      (else (loop (cdr rest)
                  (car (cddr (car rest))))))))

(define (events-of-ticket ticket events)
  "Answer the events in EVENTS that happened to TICKET."
  (filter (lambda (event)
            (same-id? (assq-ref event
                                'ticket-id)
                      (assq-ref ticket
                                'id)))
          events))

(define (round-to-hundredths value)
  "Answer VALUE as an exact number, rounded to two decimal places."
  (/ (round (* (exact->inexact value) 100)) 100))

(define-public (burndown-days starts-on ends-on today)
  "Answer every ISO day a sprint is measured over, in order.

The days run from STARTS-ON to ENDS-ON, and stop at TODAY: a chart has no
business drawing the future as though it had happened.  A sprint whose
dates are missing or whose end is before its beginning is a sprint nothing
can be measured over, and answers the empty list rather than a broken
range."
  (let ((start (iso-date->day starts-on))
        (end (iso-date->day ends-on))
        (now (iso-date->day today)))
    (cond
      ((or (not start) (not end) (> start end)) '())
      (else (let ((last (if (and now (< now end)) now end)))
              (let loop
                ((day start)
                 (out '()))
                (if (> day last) (reverse out)
                    (loop (+ day 1)
                          (cons (day->iso-date day) out)))))))))

;; `define-public' expands to a plain `define', whose formals list cannot
;; take `#:key' -- so a procedure that is called by keyword is exported
;; explicitly, once, after it has been written.
(define* (burndown-series #:key
                          sprint-id
                          starts-on
                          ends-on
                          tickets
                          (events '())
                          (statuses (default-ticket-statuses))
                          (today #f))
  "Answer the burndown of a sprint, one row per day, in order.

Each row is an association list of the day it describes and the three
numbers a chart is drawn from:

@example
((day . \"2026-10-27\") (scope . 13) (completed . 5)
 (remaining . 8) (ideal . 13))
@end example

SCOPE is the points of the tickets that were in the sprint that day,
COMPLETED is how many of them were in a closed status at the end of it,
and REMAINING is the difference.  IDEAL is the straight line the team
committed to: the sprint's own points, falling evenly to nothing on the
last day, drawn from the scope the sprint has now so that the two lines
start in the same place.

TICKETS are the sprint's tickets as the repository answers them, EVENTS is
the audit trail of those tickets, and STATUSES is the ladder that says
which statuses are closed -- the project's own list, since a project that
renamed its statuses still has to be measured against them."
  (let* ((days (burndown-days starts-on ends-on (or today (today-iso))))
         (span (iso-date-diff starts-on ends-on))
         (committed (apply + 0
                         (map ticket-points-value tickets))))
    (map (lambda (day)
           (let ((scope 0)
                 (completed 0))
             (for-each (lambda (ticket)
                         (let* ((mine (events-of-ticket ticket events))
                                (points (ticket-points-value ticket)))
                           (when (in-sprint-on-day? (sprint-membership sprint-id
                                                                       mine)
                                                    day)
                             (set! scope (+ scope points))
                             (when (ticket-status-in-bucket? statuses
                                                             (ticket-status-on-day
                                                              ticket
                                                              (status-changes
                                                               mine) day)
                                                             "done")
                               (set! completed (+ completed points))))))
                       tickets)
             (let* ((index (or (iso-date-diff starts-on day) 0))
                    (ideal (if (or (not span)
                                   (zero? span)) 0
                               (* committed
                                  (max 0
                                       (- 1 (/ index span)))))))
               (list (cons 'day day)
                     (cons 'scope scope)
                     (cons 'completed completed)
                     (cons 'remaining (max 0
                                           (- scope completed)))
                     (cons 'ideal (round-to-hundredths ideal))))))
         days)))

;; See the note above `burndown-series'.
(export burndown-series)

(define-public (burndown-summary rows)
  "Answer the three numbers a burndown is read as a whole by.

ROWS is what @code{burndown-series} answered, and the answer is an
association list of the scope the sprint was given, the points burnt so
far, and the points still standing: the same three figures as the last day
of the chart, said once for a reader who does not want to read a chart."
  (let* ((last (and (pair? rows) (car (reverse rows))))
         (scope (or (and last (assq-ref last
                                        'scope)) 0))
         (completed (or (and last (assq-ref last
                                            'completed)) 0)))
    (list (cons 'scope scope)
          (cons 'completed completed)
          (cons 'remaining (max 0
                               (- scope completed))))))

;;; ===== Planning poker =====
;;;
;;; A table a team sits at to put a number on something.  Nothing in this
;;; section knows about accounts, requests or rows: it is the arithmetic and
;;; the reading a planning table is made of, so that the page, the endpoint
;;; that hands a polling browser its state, and the test that watches all of
;;; it agree about what a room is saying.
;;;
;;; There are two decks, and they are the two ladders a ticket is already
;;; estimated on -- `ticket-points' and `ticket-sizes' -- rather than a third
;;; list kept beside them.  A team that estimates in powers of two edits one
;;; line, and both the ticket's own estimate menu and the cards a table deals
;;; follow, which is what keeps a number agreed at a table from being a
;;; number no ticket can be given.
;;;
;;; A card is carried as the text the ladder stores -- `"5"', `"m"' -- not as
;;; a number, because the two decks are different kinds of thing and a value
;;; that had to be a number would leave every size unreadable to the code
;;; that wrote it.  Everything below therefore reads and answers text, and the
;;; one place a number is wanted -- the mean of a round of story points -- is
;;; the one place that asks the ladder for one.

(define-public poker-decks
  '("points" "sizes"))

(define-public (poker-deck-label deck)
  "Answer the words a reader is shown for DECK."
  (cond
    ((equal? deck "sizes")
     "t-shirt sizes")
    ((equal? deck "points")
     "story points")
    (else deck)))

(define-public (parse-poker-deck value)
  "Answer the deck VALUE names, or @code{#f} when no deck is named.

A form is not a promise, so a deck the table does not deal is refused rather
than guessed at: a room built on a ladder nothing knows would have no card to
show and no vote it could check."
  (and (string? value)
       (let ((deck (string-downcase (string-trim-both value))))
         (and (member deck poker-decks) deck))))

(define-public (poker-cards deck)
  "Answer the cards DECK deals, as @code{(value . label)} pairs.

The order is the ladder's own, which is the order a hand of cards is held in
and the order a tally is read in -- low to high, so a room looking at its own
votes reads them as a spread rather than as a list of who happened to pick
what when."
  (cond
    ((equal? deck "sizes")
     (map (lambda (size)
            (cons size (ticket-size-label size))) ticket-sizes))
    ((equal? deck "points")
     (map (lambda (points)
            (cons (number->string points)
                  (number->string points))) ticket-points))
    (else '())))

(define-public (poker-card-rank deck value)
  "Answer how far along DECK the card VALUE sits, or @code{#f}.

A rank is what makes the sizes orderable at all: `xs' and `xxl' are words,
and only the ladder says which of them is the low end of a spread."
  (let loop
    ((cards (poker-cards deck))
     (rank 0))
    (cond
      ((null? cards) #f)
      ((equal? (caar cards) value) rank)
      (else (loop (cdr cards) (+ rank 1))))))

(define-public (parse-poker-card deck value)
  "Answer VALUE as the card DECK deals it, or @code{#f} for a card nobody deals.

The answer is the deck's own spelling -- @code{\"5\"} for a number a team
burns down, @code{\"m\"} for a t-shirt size -- which is what a vote is
stored as and what the ladder was checked against, so a card that arrives
from a form and a card read back from the database are the same text."
  (cond
    ((equal? deck "sizes") (parse-estimate-size value))
    ((equal? deck "points") (let ((points (parse-estimate-points value)))
                              (and points (number->string points))))
    (else #f)))

(define-public (poker-card-label deck value)
  "Answer the face a reader is shown for the card VALUE on DECK.

A size is shown in capitals, the way it is shown everywhere else in the
application; a number is already what it says."
  (cond
    ((equal? deck "sizes") (ticket-size-label value))
    ((number? value) (number->string value))
    ((string? value) value)
    (else (format #f "~a" value))))

;; The face-down rule, said once.  A planning table only works if nobody can
;; read the room before the cards are turned over, so what a browser may know
;; about a vote is decided here rather than in whichever page happens to draw
;; it -- a page that carried the faces and drew them face down would be one
;; browser-tools window away from spoiling the vote, and a polling endpoint
;; that answered with them would hand the same room to anybody watching the
;; network.
(define-public (poker-vote-face deck revealed? value)
  "Answer the card a vote VALUE is drawn as, or @code{#f} while the cards are
face down.

VALUE is the card a participant played; the answer is its label once the room
has been revealed, and nothing at all before then."
  (and revealed? value (poker-card-label deck value)))

;;; --- what a round of cards adds up to ---

(define-public (poker-counts deck values)
  "Answer how many times each card of DECK was played in VALUES.

One row per card that was played at least once -- @code{(value . count)} --
in the deck's own order, so a tally is read in the order the cards are dealt
rather than in the order they happened to be picked.  A card nobody played is
not a row: a count of zero says nothing about a room."
  (filter (lambda (row)
            (> (cdr row) 0))
          (map (lambda (card)
                 (cons (car card)
                       (length (filter (lambda (value)
                                         (equal? value (car card)))
                                       values))))
               (poker-cards deck))))

(define-public (poker-most-chosen deck values)
  "Answer the cards VALUES played most often, in the deck's own order.

More than one card answers when the room is split evenly between them, which
is what a facilitator has to see: a tie is not a consensus, and a page that
picked one of two equal cards would be inventing an agreement nobody made.
A round nobody played answers nothing at all."
  (let* ((counts (poker-counts deck values))
         (top (apply max 0 (map cdr counts))))
    (if (= top 0)
        '()
        (map car (filter (lambda (row)
                           (= (cdr row) top)) counts)))))

(define-public (poker-average deck values)
  "Answer the mean of VALUES on DECK, or @code{#f} when there is no mean.

Only the story points have a mean.  The sizes are words, and their average is
a number nobody asked for: `L' is not half way between `M' and `XL'.  A round
with nothing played, and a round whose values are not cards of this deck, have
no mean either."
  (let ((numbers (filter number? (map parse-estimate-points values))))
    (if (or (not (equal? deck "points")) (null? numbers)) #f
        (exact->inexact (/ (apply + numbers) (length numbers))))))

(define-public (poker-spread deck values)
  "Answer the two ends of the room's disagreement, or @code{#f} for none.

A spread is the read-out a facilitator acts on: thirteen points against two is
a conversation, and a room that all played the same card has nothing to
discuss.  A single card, and a round nobody played, are no spread at all, and
a value the deck does not deal is left out rather than counted as an end."
  (let ((ranks (filter-map (lambda (value)
                             (poker-card-rank deck value))
                           values)))
    (if (< (length (delete-duplicates ranks)) 2) #f
        (let ((cards (poker-cards deck)))
          (list (cons 'low (car (list-ref cards (apply min ranks))))
                (cons 'high (car (list-ref cards (apply max ranks)))))))))

;;; --- the name a participant is known by ---

(define-public poker-name-max-length
  40)

(define-public (parse-participant-name value)
  "Answer the name a participant signed in with, or @code{#f} when there is none.

A name is the only thing asked of somebody joining a table -- there is no
account to make, and no password to remember -- so it is read forgivingly:
space around it is not part of it, and a run of spaces inside it is one space.
It is refused only when there is nothing left, and capped because a name is
the label on a card rather than a message."
  (and (string? value)
       (let ((name (string-join (string-tokenize value) " ")))
         (and (not (string-null? name))
              (<= (string-length name) poker-name-max-length)
              name))))

;;; --- the seat a browser holds ---
;;;
;;; A participant is a name and a secret, never an account.  The secret is
;;; handed out when somebody takes a seat and travels in one cookie for the
;;; whole section -- one name, rather than a cookie per room -- whose value is
;;; a small map of room to secret, so sitting at a second table does not cost
;;; a reader their seat at the first.  Both halves of an entry are UUIDs,
;;; neither of which can contain either separator, so the value reads back
;;; without any escaping of its own.

(define-public poker-token-cookie
  "x-lucidplan-poker")

(define poker-token-room-separator
  "~")

(define poker-token-seat-separator
  "|")

;; A cookie is sent on every request, so the map is capped: a browser that has
;; sat at a great many tables keeps the ones it was at most recently rather
;; than growing a header without limit.
(define-public poker-seats-kept
  8)

(define-public (poker-seats value)
  "Answer the seats a poker cookie VALUE holds, most recent first.

Anything that is not a well-formed entry -- a truncated value, a hand-edited
cookie, somebody else's cookie from another application -- is left out rather
than answered as a seat, so a malformed header is simply a reader who is not
seated anywhere."
  (if (not (string? value))
      '()
      (filter-map (lambda (entry)
                    (let ((parts (string-split entry #\~)))
                      (and (= (length parts) 2)
                           (not (string-null? (car parts)))
                           (not (string-null? (cadr parts)))
                           (cons (car parts) (cadr parts)))))
                  (string-split value #\|))))

(define-public (poker-seat-token value room)
  "Answer the seat VALUE holds at ROOM, or @code{#f} when it holds none."
  (let ((seat (assoc room (poker-seats value))))
    (and seat (cdr seat))))

(define-public (poker-seat-remember value room token)
  "Answer VALUE with the seat ROOM/TOKEN written in front of it.

The newest seat is written first and the map is capped at
@code{poker-seats-kept}, so the tables a reader was last at are the ones that
survive."
  (let* ((others (filter (lambda (seat)
                           (not (equal? (car seat) room)))
                         (poker-seats value)))
         (kept (take others
                     (min (length others)
                          (- poker-seats-kept 1)))))
    (string-join (map (lambda (seat)
                        (format #f "~a~a~a" (car seat)
                                poker-token-room-separator (cdr seat)))
                      (cons (cons room token) kept))
                 poker-token-seat-separator)))
