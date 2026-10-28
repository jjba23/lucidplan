;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The ladders a ticket is triaged with -- its status and its type -- and the
;;; rule that reads a submitted value against them.  A form is not a promise,
;;; so the menus a page offers and the endpoints behind them have to agree
;;; about what is on offer; what is checked here is that agreement, and the
;;; way a list a project wrote for itself is read back and falls back.
;;;
;;; Nothing here touches a database or a request: the ladders are values and
;;; the rules are procedures, so a status a project invented, a bucket nobody
;;; knows and an id that was never offered are all just inputs.
;;;
;;; The module is named `(veritas unit ticket-triage-spec)' because that is
;;; the name the veritas runner derives from where the file sits
;;; (test/veritas/unit), which is how `maak test' loads it.

(define-module (veritas unit ticket-triage-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan domain)
  #:export (spec))

;;; ====== a project's own statuses, as its edit page hands them over ======

;; The editor's format, one status to a line: the value the schema stores, the
;; label a reader is shown, the icon, the glyph a native menu can hold, and
;; the bucket it belongs to.  A note and a blank line are in here too, because
;; the editor is a place to leave one as well as a list of statuses.
(define written-statuses
  (parse-ticket-statuses "todo | To-do | anchor | ⚓ | open
done | Done | check | ✅ | done

# the ones we turned down in review
rejected | Rejected | x | | rejected"))

(define partial-statuses
  (parse-ticket-statuses "todo | To-do | anchor | ⚓ | finished"))

(define parse-suite
  (suite "statuses written by hand"
         (test "the values are read in the order they were written"
               (assert-equal #:name "the read-back values keep their order"
                             #:expect '("todo" "done" "rejected")
                             #:got (ticket-status-values written-statuses)))
         (test "each field of an entry is read"
               (assert-equal #:name "the label is read"
                             #:expect "To-do"
                             #:got (ticket-status-label written-statuses
                                                        "todo"))
               (assert-equal #:name "the icon is read"
                             #:expect "anchor"
                             #:got (ticket-status-icon written-statuses "todo"))
               (assert-equal #:name "the glyph is read"
                             #:expect "⚓"
                             #:got (ticket-status-glyph written-statuses
                                                        "todo")))
         (test "the bucket is read"
               (assert-equal #:name "a status in the done bucket says so"
                             #:expect "done"
                             #:got (ticket-status-bucket written-statuses
                                                         "done"))
               (assert-equal #:name "a status in the rejected bucket says so"
                             #:expect "rejected"
                             #:got (ticket-status-bucket written-statuses
                                                         "rejected")))
         (test
          "a note and a blank line are skipped rather than read as statuses"
          (assert-equal #:name "the comment is not a status"
                        #:expect #f
                        #:got (ticket-status-spec written-statuses
                               "# the ones we turned down in review"))
          (assert-equal #:name "no status was invented by the blank line"
                        #:expect 3
                        #:got (length written-statuses)))
         (test "text that names nothing is not a list of statuses"
               (assert-equal #:name "an empty block answers #f"
                             #:expect #f
                             #:got (parse-ticket-statuses ""))
               (assert-equal #:name "a block of notes alone answers #f"
                             #:expect #f
                             #:got (parse-ticket-statuses "# nothing here\n\n")))
         (test "a field left out takes the default the rest of the code reads"
          (assert-equal #:name "a bucket nothing knows is read as open"
                        #:expect "open"
                        #:got (ticket-status-bucket partial-statuses "todo")))))

;;; ====== a status the list does not name ======

(define unknown-status-suite
  (suite "a status that is not on the list"
         (test "is still a ticket somebody is working on"
               (assert-equal #:name "an unknown status is bucketed as open"
                             #:expect "open"
                             #:got (ticket-status-bucket written-statuses
                                                         "polishing")))
         (test "reads as itself when there is no label for it"
               (assert-equal #:name "the value stands in for the label"
                             #:expect "polishing"
                             #:got (ticket-status-label written-statuses
                                                        "polishing"))
               (assert-equal #:name "there is no spec for it"
                             #:expect #f
                             #:got (ticket-status-spec written-statuses
                                                       "polishing")))
         (test "is not in any bucket's list"
               (assert-equal #:name
                "the done bucket holds the statuses that are done"
                #:expect '("done")
                #:got (ticket-status-values-in-bucket written-statuses "done")))))

;;; ====== whose list a project's tickets move through ======

(define effective-suite
  (suite "the list a project's tickets move through"
         (test "a project with a list of its own uses it"
               (assert-equal #:name "the project's own list is the one read"
                             #:expect '("todo" "shipped")
                             #:got (ticket-status-values (effective-ticket-statuses `
                                                          ((human-id . "byggsteg")
                                                           (ticket-statuses . "todo | To-do | anchor | ⚓ | open\nshipped | Shipped | check | ✅ | done"))))))
         (test "a project with none uses the instance's"
               (assert-equal #:name
                "a project that says nothing falls back to the instance"
                #:expect (default-ticket-statuses)
                #:got (effective-ticket-statuses '((human-id . "byggsteg")))))
         (test "a project that could not be read is not an error"
               (assert-equal #:name
                             "no project at all falls back to the instance"
                             #:expect (default-ticket-statuses)
                             #:got (effective-ticket-statuses #f)))))

;;; ====== the assignee a form submitted ======

(define submitted-assignee-suite
  (suite "the assignee a form submitted"
         (test "an id that was offered is answered"
               (assert-equal #:name "a known id is taken as it was submitted"
                             #:expect "user-1"
                             #:got (known-user-id '("user-1" "user-2")
                                                  "user-1")))
         (test "an id that was not offered is refused"
               (assert-equal #:name "an id no menu offered is not an assignee"
                             #:expect #f
                             #:got (known-user-id '("user-1" "user-2")
                                                  "user-9"))
               (assert-equal #:name
                             "a value that is not text is not an assignee"
                             #:expect #f
                             #:got (known-user-id '("user-1") 42)))
         (test "no assignee is the absence of one, not an unknown user"
               (assert-equal #:name
                "an empty submission leaves the ticket as it was"
                #:expect #f
                #:got (known-user-id '("user-1") "")))))

;;; ====== an inline change to a ticket ======

(define inline-change-suite
  (suite "an inline change from the ticket's own page"
         (test "a status the project moves through is accepted"
               (assert-equal #:name
                             "the change is the column and its new value"
                             #:expect '((ticket-status . "done"))
                             #:got (ticket-field-change "status" "done"
                                                        written-statuses)))
         (test "a status the project does not move through is refused"
               (assert-equal #:name
                             "a status invented by hand changes nothing"
                             #:expect #f
                             #:got (ticket-field-change "status" "polishing"
                                                        written-statuses)))
         (test "a type is checked against the types a ticket has"
               (assert-equal #:name "a type on the ladder is accepted"
                             #:expect '((ticket-type . "bug"))
                             #:got (ticket-field-change "type" "bug"))
               (assert-equal #:name "a type nothing offers is refused"
                             #:expect #f
                             #:got (ticket-field-change "type" "chore"))
               (assert-equal #:name "the types are the ones the schema holds"
                             #:expect '("task" "bug" "story" "spike"
                                        "time-box")
                             #:got ticket-types))
         (test "a field a ticket has no ladder for is refused"
               (assert-equal #:name "the priority is not changed this way"
                             #:expect #f
                             #:got (ticket-field-change "priority" "2")))
         (test "with no project list the instance's own is the ladder"
               ;; `assert-true' takes the value it checks before its keywords.
               (assert-true (pair? (ticket-field-change "status"
                                                        (car (ticket-status-values
                                                              (default-ticket-statuses)))))
                #:name "the first status of the instance's list is accepted"))))

;;; ====== the order a list is asked for in ======

(define sort-suite
  (suite "the order a list is asked for in"
         (test "a keyword the pages offer becomes an ORDER BY"
               (assert-equal #:name "the title, A to Z"
                             #:expect '((title asc))
                             #:got (entity-sort-from-query "title-asc"))
               (assert-equal #:name "the priority, highest first"
                             #:expect '((priority desc))
                             #:got (entity-sort-from-query "priority-desc")))
         (test "a keyword nobody offers falls back to the last edited first"
               (assert-equal #:name "an unknown order is the default one"
                             #:expect '((updated-at desc))
                             #:got (entity-sort-from-query "whatever"))
               (assert-equal #:name "and so is no order at all"
                             #:expect '((updated-at desc))
                             #:got (entity-sort-from-query #f)))))

(define (spec)
  (veritas-run parse-suite
               unknown-status-suite
               effective-suite
               submitted-assignee-suite
               inline-change-suite
               sort-suite))
