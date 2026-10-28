;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
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
                               db-write))

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
