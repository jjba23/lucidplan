;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; What a project's own list of statuses costs to read back.  Nothing here
;;; asserts: a benchmark measures, and that is why it lives under `benchmark/'
;;; rather than beside the tests -- the question it answers is how long, not
;;; whether.  It is run on its own type:
;;;
;;;   VERITAS_TEST_TYPE=benchmark guile -L . -L lib -L test -c '((@ (veritas runner) run-tests))'
;;;
;;; A project's list is read back on the pages and endpoints that work with
;;; its tickets, so the block below is a long one -- a project that names a
;;; dozen statuses of its own -- to say what the reading costs at the size a
;;; project could plausibly write.
;;;
;;; The module is named `(veritas benchmark ticket-statuses-benchmark)'
;;; because that is the name the veritas runner derives from where the file
;;; sits (test/veritas/benchmark).

(define-module (veritas benchmark ticket-statuses-benchmark)
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (lucidplan domain)
  #:export (spec))

(define written-statuses
  (string-join (map (lambda (n)
                      (format #f "status-~a | Status ~a | circle | ~a | open"
                              n n
                              (if (even? n) "open" "done")))
                    (iota 16)) "\n"))

;; A single reading is far too quick for the profiler to catch -- it samples a
;; running program, and a run that is over in microseconds has nothing sampled
;; from it at all -- so a run is a few thousand readings.  What is measured is
;; the reading itself, and the number of readings is only how long the
;; measurement is held open.
(define readings-per-run
  2000)

(define (read-it-back n)
  (unless (zero? n)
    (parse-ticket-statuses written-statuses)
    (read-it-back (- n 1))))

(define read-back-benchmark
  (benchmark #:name (format #f
                     "reading back a project's own list of sixteen statuses, ~a times"
                     readings-per-run)
             #:runs 3
             #:thunk (lambda ()
                       (read-it-back readings-per-run))))

(define lookup-benchmark
  (benchmark #:name (format #f
                     "naming the label of the last status on that list, ~a times"
                     readings-per-run)
             #:runs 3
             #:thunk (lambda ()
                       (read-it-back readings-per-run)
                       (ticket-status-label (parse-ticket-statuses
                                             written-statuses) "status-15"))))

(define (spec)
  (veritas-run read-back-benchmark lookup-benchmark))
