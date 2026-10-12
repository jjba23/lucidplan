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
  #:use-module (ice-9 string-fun))

(define-public (only-tickets-filter ticket)
  (or (equal? (assq-ref ticket
                        'ticket-status) "todo")
      (equal? (assq-ref ticket
                        'ticket-status) "work-in-progress")
      (equal? (assq-ref ticket
                        'ticket-status) "in-review")))

(define-public (only-done-tickets-filter ticket)
  (equal? (assq-ref ticket
                    'ticket-status) "done"))

(define-public (only-rejected-tickets-filter ticket)
  (equal? (assq-ref ticket
                    'ticket-status) "rejected"))

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
       (member submitted user-ids)
       submitted))

;; The two ladders a ticket is triaged with, as the values the schema holds.
;; They are stated once here because two things need to agree about them: the
;; menus that offer them, and the check that refuses a value nothing offered.
(define-public ticket-statuses
  '("todo" "work-in-progress" "in-review" "done" "rejected"))

(define-public ticket-types
  '("task" "bug" "story" "spike" "time-box"))

(define (ticket-ladder field)
  "Answer the column a submitted inline change names and the values it may
hold, as the pair @code{(column . values)}, or @code{#f} for a field a ticket
has no ladder for."
  (cond
    ((equal? field "status")
     (cons 'ticket-status ticket-statuses))
    ((equal? field "type")
     (cons 'ticket-type ticket-types))
    (else #f)))

(define-public (ticket-field-change field value)
  "Answer the column change a submitted inline edit names, or @code{#f}.

The status and the type of a ticket are changed from the ticket's own page by
menus that post what they show, and a form is not a promise: the value that
arrives is whatever was sent.  @var{field} must therefore name one of the two
ladders a ticket is triaged with, and @var{value} must be one of the rows that
ladder offers; anything else -- an unknown field, a status invented by hand, a
value that is not text at all -- answers @code{#f}, which is what leaves a
ticket as it was.

The answer is the change itself, an association list of one column and its new
value, so a caller can hand it straight to the repository rather than deciding
a second time which column moves."
  (let ((ladder (ticket-ladder field)))
    (and ladder
         (string? value)
         (member value (cdr ladder))
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
