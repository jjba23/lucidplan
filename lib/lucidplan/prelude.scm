;;; lucidplan - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

(define-module (lucidplan prelude)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile abstraction)
  #:use-module (ice-9 iconv))

(define-public (nestring? x)
  (and x
       (not (equal? "" x))))

(define-public (render-if-nestring expr x)
  (if (nestring? x)
      (expr x)
      '()))

(define-public (log-exprs xs)
  (newline)
  (display "========")
  (for-each (lambda (x)
              (newline)
              (write (format #f "~a" x))
              (newline)) xs))

(define (drop-first-last s)
  (string-take (string-drop s 1)
               (- (string-length s) 2)))

(define (drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2) 
                     (drop-right (cdr lines) 1) 
                     '())))  
    (string-join middle "\n")))

(define-public org-code-pattern
  "~([^~]+)~")

(define-public org-code-block-pattern
  "#\\+begin_src.*\n.*#\\+end_src")

(define-public (parse-org org)
  (let* ((with-parsed-code (regexp-substitute/global #f
                                                     org-code-pattern
                                                     org
                                                     'pre
                                                     (lambda (m)
                                                       (format #f
                                                               "<code class=\"~a\">~a</code>"
                                                               (->css '(bg-stone-200/75
                                                                        rounded-lg
                                                                        py-1 px-2))
                                                               (drop-first-last (match:substring
                                                                                 m))))
                                                     'post))
         (with-parsed-code-block (regexp-substitute/global #f
                                                           org-code-block-pattern
                                                           with-parsed-code
                                                           'pre
                                                           (lambda (m)
                                                             (format #f "<pre class=\"~a\">~a</pre>"
                                                                     (->css '(bg-stone-200/75
                                                                              rounded-lg py-2 px-3
                                                                              whitespace-pre))
                                                                     (drop-first-last-line (match:substring m))))
                                                           'post)))
    with-parsed-code-block))

