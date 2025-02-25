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
  #:use-module (ice-9 regex)
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

(define-public org-code-pattern
  "~([^~]+)~")

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
                                                     'post)))
    with-parsed-code))
