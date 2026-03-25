;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>

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

(define-module (lucidplan repository util)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 string-fun)
  #:use-module (lucidplan prelude)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:export (to-column-string db-row-sexp))

(define (to-column-string xs)
  (string-join (map (lambda (x)
                      (string-replace-substring (symbol->string x) "-" "_"))
                    xs) ", "))

(define (alist? obj)
  (cond
    ((null? obj)
     #t)
    ((not (pair? obj))
     #f)
    ((not (pair? (car obj)))
     #f)
    (else (alist? (cdr obj)))))

(define (db-row-sexp xs)
  (cond
    ((alist? xs)
     (map (lambda (pair)
            (cons (string->symbol (string-replace-substring (car pair) "_" "-"))
                  (cdr pair))) xs))
    (else xs)))
