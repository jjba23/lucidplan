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
