;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The persistence layer speaks the SQL that kracht (>= 0.3.0) builds.
;;
;; The repositories below never format a statement by hand: they assemble a
;; <sql-query> with the parameterized builders from `(kracht sql)' -- the same
;; ones re-exported here -- and hand it to `db-read' / `db-write'.  Those two
;; take care of the one wrinkle the driver imposes: the `dbi-query' that GNU
;; Artanis wraps accepts only a bare SQL string, so the values kracht collected
;; have to be inlined before they are executed.  They are inlined *escaped*, by
;; kracht's own `value->sql-literal', so a value can never close the literal it
;; sits in.  See `sql-query->literal'.

(define-module (lucidplan sqlite util)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis db)
  #:use-module (scriba scriba)
  #:use-module (scriba auto)
  #:use-module (kracht prelude)
  #:re-export (symbol->sql-column sql-column->symbol
                                  symbols->sql-columns-list
                                  sql-row->scheme-alist
                                  alist?
                                  value->sql-literal
                                  pair->sql-assignment
                                  escape-sql-string
                                  quote-sql-identifier
                                  sql-select
                                  sql-insert
                                  sql-insert-many
                                  sql-upsert
                                  sql-upsert-many
                                  sql-update
                                  sql-delete
                                  sql-parameterize
                                  sql-where-fragment
                                  sql-query?
                                  sql-query-sql
                                  sql-query-params)
  #:export (quoted-sql-columns order-by-sql sql-query->literal db-read
                               db-write
                               bytes->base64 base64->bytes))

(define (quoted-sql-columns columns)
  "Render COLUMNS (a list of Scheme symbols) as a comma-separated list of
double-quoted SQL column names, the way kracht's builders quote identifiers.

Use this when a hand-written template needs kracht's identifier quoting but the
statement is not one the builders can express -- a `LIKE ... OR ...' search,
say.  The names are developer-controlled constants, never user input."
  (string-join (map (lambda (column)
                      (quote-sql-identifier (symbol->sql-column column)))
                    columns) ", "))

(define (order-by-sql order-by)
  "Render ORDER-BY -- a list of `(column [direction])' pairs, as the kracht
builders accept -- as the ` ORDER BY ...' tail of a statement, quoted the same
way.  DIRECTION is the symbol `asc' or `desc'; an omitted direction is left to
SQLite's default.  Returns the empty string for an empty list.

This is the one piece of statement building the builders do not expose, so a
statement assembled with `sql-parameterize' can still carry an order that comes
from `entity-sort-from-query'."
  (if (null? order-by) ""
      (string-append " ORDER BY "
                     (string-join (map (lambda (clause)
                                         (let ((column (quote-sql-identifier (symbol->sql-column
                                                                              (car
                                                                               clause))))
                                               (direction (and (pair? (cdr
                                                                       clause))
                                                               (cadr clause))))
                                           (case direction
                                             ((#f)
                                              column)
                                             ((asc asc!)
                                              (string-append column " ASC"))
                                             ((desc desc!)
                                              (string-append column " DESC"))
                                             (else (error
                                                    "order-by-sql: unknown ORDER BY direction"
                                                    direction))))) order-by)
                                  ", "))))

(define (sql-query->literal query)
  "Render a kracht <sql-query> as one executable SQL string, with every bound
parameter inlined as an escaped literal.

The driver GNU Artanis wraps (`dbi-query') accepts only a bare SQL string and
cannot bind parameters, so the values kracht's builders collect travel inside
the statement instead.  They are still escaped by kracht's `value->sql-literal'
-- which doubles single quotes -- so a value can never close the literal it sits
in, and the shape of the statement is fixed by the template alone.  A template
must therefore contain exactly as many `?' markers as the query has parameters;
a mismatch is reported rather than silently mis-compiled."
  (let loop
    ((chars (string->list (sql-query-sql query)))
     (params (sql-query-params query))
     (out '()))
    (cond
      ((null? chars)
       (unless (null? params)
         (error
          "sql-query->literal: more bound parameters than `?' placeholders"
          query))
       (list->string (reverse out)))
      ((char=? (car chars) #\?)
       (when (null? params)
         (error
          "sql-query->literal: more `?' placeholders than bound parameters"
          query))
       ;; `out' is accumulated reversed; a multi-character literal therefore
       ;; goes in reversed as a whole so the final `reverse' restores it.
       (loop (cdr chars)
             (cdr params)
             (append (reverse (string->list (value->sql-literal (car params))))
                     out)))
      (else (loop (cdr chars) params
                  (cons (car chars) out))))))

(define (query->sql query)
  "Return QUERY as an executable SQL string: a <sql-query> is inlined with
`sql-query->literal', a plain string is passed through unchanged."
  (if (sql-query? query)
      (sql-query->literal query) query))

(define s
  (scriba-auto-logger))

(define (db-read conn)
  (lambda (name query)
    (let* ((sql (query->sql query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query conn sql)))))
      (log-debug s "~a query:\n\t~a\n" name sql)
      (log-debug s "~a rows: ~a\n" name
                 (length rows)) rows)))

(define (db-write conn)
  (lambda (name query)
    (let ((sql (query->sql query)))
      (log-debug s "~a query:\n\t~a\n" name sql)
      (DB-query conn sql))))

;;; ===== Bytes a row can carry =====
;;;
;;; A file or a picture kept in the database is text by the time it reaches a
;;; statement.  It is not squeamishness: every value here is inlined into SQL
;;; through `value->sql-literal', which escapes *text* and knows numbers,
;;; booleans and strings -- a bytevector would be formatted into garbage, and a
;;; string carrying a NUL (which is what a PNG is full of) is refused outright
;;; by kracht itself.  So anything binary is carried as base64, which is ASCII
;;; by construction, and costs a third more bytes for the privilege of needing
;;; no exception to the rule that keeps every other write safe.
;;;
;;; kracht ships a base64 pair of its own (`base64-encode' / `base64-decode',
;;; re-exported by the prelude) and it is the right tool for text.  It cannot
;;; be used here, and the reason is worth writing down rather than rediscovering:
;;; its encoder takes a *string* and runs `string->utf8' over it, and its
;;; decoder answers `(bytevector->string … "utf-8")' after trimming trailing
;;; whitespace.  A PNG is not UTF-8 and a PDF may end in a newline, so the pair
;;; would either raise or quietly return something a picture cannot be rebuilt
;;; from.  These two work on bytes at both ends, which is what a row that holds
;;; a file needs.

(define base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define (base64-digit n)
  (string-ref base64-alphabet (modulo n 64)))

(define (bytes->base64 bytes)
  "Answer BYTES (a bytevector) as the base64 text a row can carry."
  (let ((len (bytevector-length bytes)))
    (call-with-output-string
     (lambda (port)
       (let loop ((i 0))
         (when (< i len)
           (let* ((left (- len i))
                  (b0 (bytevector-u8-ref bytes i))
                  (b1 (if (>= left 2) (bytevector-u8-ref bytes (+ i 1)) 0))
                  (b2 (if (>= left 3) (bytevector-u8-ref bytes (+ i 2)) 0))
                  (n (+ (* b0 65536) (* b1 256) b2)))
             (display (base64-digit (quotient n 262144)) port)
             (display (base64-digit (quotient n 4096)) port)
             (display (if (>= left 2) (base64-digit (quotient n 64)) #\=)
                      port)
             (display (if (>= left 3) (base64-digit n) #\=) port)
             (loop (+ i 3)))))))))

(define (base64-value c)
  "Answer the six-bit value CHARACTER C stands for, or #f when it stands for
nothing."
  (let ((n (char->integer c)))
    (cond
      ((and (>= n 65) (<= n 90)) (- n 65))
      ((and (>= n 97) (<= n 122)) (+ 26 (- n 97)))
      ((and (>= n 48) (<= n 57)) (+ 52 (- n 48)))
      ((= n 43) 62)
      ((= n 47) 63)
      (else #f))))

(define (base64->bytes text)
  "Answer the bytes TEXT was written from, as a bytevector, or @code{#f} when
TEXT is not base64.

Whitespace is ignored, so text that has been through an editor still reads.
Anything else outside the alphabet, a length that cannot be four characters to
a group, and padding in a place padding cannot be, all answer @code{#f}: a row
that cannot be read is a route that answers 404, not a request that fails with
a stack trace."
  (let* ((clean (filter (lambda (c)
                          (not (char-whitespace? c)))
                        (string->list (if (string? text) text ""))))
         (len (length clean)))
    (and (positive? len)
         (zero? (modulo len 4))
         (let loop ((rest clean) (acc '()))
           (if (null? rest)
               (u8-list->bytevector (reverse acc))
               (let* ((group (take rest 4))
                      (c2 (third group))
                      (c3 (fourth group))
                      (pad2 (char=? c2 #\=))
                      (pad3 (char=? c3 #\=))
                      (v0 (base64-value (first group)))
                      (v1 (base64-value (second group)))
                      (v2 (if pad2 0 (base64-value c2)))
                      (v3 (if pad3 0 (base64-value c3))))
                 (and v0 v1 v2 v3
                      ;; `=' pads the tail of a group, so it cannot come
                      ;; first: a group ending in `=' may end in `==' and no
                      ;; other way round.
                      (or (not pad2) pad3)
                      (let* ((n (+ (* v0 262144) (* v1 4096) (* v2 64) v3))
                             (b0 (modulo (quotient n 65536) 256))
                             (b1 (modulo (quotient n 256) 256))
                             (b2 (modulo n 256))
                             ;; in order; `acc' is the output backwards, so
                             ;; what is kept is reversed onto it and the
                             ;; whole thing is reversed once at the end
                             (kept (if pad2 (list b0)
                                       (if pad3 (list b0 b1)
                                           (list b0 b1 b2)))))
                        (loop (drop rest 4)
                              (append (reverse kept) acc))))))))))
