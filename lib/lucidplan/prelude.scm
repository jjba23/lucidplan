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
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile abstraction)
  #:use-module (ice-9 iconv))

(define-public (nestring? x)
  (and x
       (not (equal? "" x))))

(define-public (log-exprs xs)
  (newline)
  (display "========")
  (for-each (lambda (x)
              (newline)
              (write (format #f "~a" x))
              (newline)) xs))

(define (drop-first-last s)
  (if (> (string-length s) 2)

      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

(begin
  (define org-code-pattern
    "~([^~]+)~")

  (define org-code-replacement
    "<code class=\"~a\">~a</code>")

  (define org-code-block-pattern
    "#\\+begin_src.*\n.*#\\+end_src")

  (define org-code-block-replacement
    "<pre class=\"~a\">~a</pre>")

  (define org-em-pattern
    "/([^/<>:]+)/")

  (define org-em-replacement
    "<em>~a</em>")

  (define org-strong-pattern
    "\\*([^*]+)\\*")

  (define org-strong-replacement
    "<strong>~a</strong>")

  (define org-link-pattern
    "\\[\\[.*\\]\\]")

  (define org-link-replacement
    "<a class=\"~a\" href=\"~a\">~a</a>")

  (define* (parse-org org)
    (let* ((with-parsed-code (regexp-substitute/global #f
                                                       org-code-pattern
                                                       org
                                                       'pre
                                                       (lambda (m)
                                                         (format #f
                                                          org-code-replacement
                                                          (->css '(bg-stone-200/75
                                                                   rounded-lg
                                                                   py-1 px-2))
                                                          (drop-first-last (match:substring
                                                                            m))))
                                                       'post))
           ;; (_ (display "\nwith-parsed-code\n"))
           ;; (_ (write with-parsed-code))
           (with-parsed-italic (regexp-substitute/global #f
                                                         org-em-pattern
                                                         with-parsed-code
                                                         'pre
                                                         (lambda (m)
                                                           (format #f
                                                            org-em-replacement
                                                            (drop-first-last (match:substring
                                                                              m))))
                                                         'post))
           ;; (_ (display "\nwith-parsed-italic\n"))
           ;; (_ (write with-parsed-italic))
           (with-parsed-strong (regexp-substitute/global #f
                                                         org-strong-pattern
                                                         with-parsed-italic
                                                         'pre
                                                         (lambda (m)
                                                           (format #f
                                                            org-strong-replacement
                                                            (drop-first-last (match:substring
                                                                              m))))
                                                         'post))
           ;; (_ (display "\nwith-parsed-strong\n"))
           ;; (_ (write with-parsed-strong))
           (with-parsed-link (regexp-substitute/global #f
                                                       org-link-pattern
                                                       with-parsed-strong
                                                       'pre
                                                       (lambda (m)
                                                         (let* ((l (drop-first-last
                                                                    (match:substring
                                                                     m)))
                                                                (split-l (string-split
                                                                          (string-replace-substring
                                                                           l
                                                                           "]["
                                                                           "|")
                                                                          #\|))
                                                                ;; (_ (write
                                                                ;; split-l))
                                                                (link (string-drop
                                                                       (car
                                                                        split-l)
                                                                       1))
                                                                (desc (string-drop-right
                                                                       (car (cdr
                                                                             split-l))
                                                                       1)))
                                                           (format #f
                                                            org-link-replacement
                                                            (->css '(text-blue-800
                                                                     font-bold))
                                                            link desc)))
                                                       'post))
           ;; (_ (display "\nwith-parsed-link\n"))
           ;; (_ (write with-parsed-link))
           (with-parsed-code-block (regexp-substitute/global #f
                                    org-code-block-pattern
                                    with-parsed-link
                                    'pre
                                    (lambda (m)
                                      (format #f org-code-block-replacement
                                              (->css '(bg-stone-200/75
                                                       rounded-lg
                                                       my-3
                                                       py-2
                                                       px-3
                                                       whitespace-pre
                                                       overflow-x-scroll))
                                              (drop-first-last-line (match:substring
                                                                     m))))
                                    'post))
           ;; (_ (display "\nwith-parsed-code-block\n"))
           ;; (_ (write with-parsed-code-block))
           )
      with-parsed-code-block))
  (export parse-org))

;; module tests
(test-begin "prelude")

(test-equal "nestring? with non-empty string" #t
            (nestring? "hello"))
(test-equal "nestring? with empty string" #f
            (nestring? ""))
(test-equal "nestring? with #f" #f
            (nestring? #f))
(test-equal "nestring? with #t" #t
            (nestring? #t))

(test-assert "log-exprs runs without error"
             (begin
               (log-exprs '("test" 123)) #t))

(test-equal "drop-first-last normal case" "ell"
            (drop-first-last "hello"))
(test-equal "drop-first-last single char" "a"
            (drop-first-last "a"))
(test-equal "drop-first-last two chars" "ab"
            (drop-first-last "ab"))
(test-equal "drop-first-last empty string" ""
            (drop-first-last ""))

(test-equal "drop-first-last-line normal case" "middle line"
            (drop-first-last-line "first line\nmiddle line\nlast line"))
(test-equal "drop-first-last-line single line" ""
            (drop-first-last-line "single line"))
(test-equal "drop-first-last-line two lines" ""
            (drop-first-last-line "first line\nlast line"))
(test-equal "drop-first-last-line empty input" ""
            (drop-first-last-line ""))

(test-equal "parse-org inline code parsing"
 "This is <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">test</code> code."
 (parse-org "This is ~test~ code."))

(test-equal "parse-org code block parsing"
 "<pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">some code
inside block</pre>"
 (parse-org "#+begin_src
some code
inside block
#+end_src"))

(test-equal "parse-org mixed inline and block parsing"
 "Here is <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">inline</code> code and a block: <pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">code block</pre>"
 (parse-org "Here is ~inline~ code and a block: #+begin_src
code block
#+end_src"))

(test-equal "parse-org no matches" "Just some text with no code."
            (parse-org "Just some text with no code."))

(test-equal "parse-org inline italic parsing" "This is <em>test</em> code."
            (parse-org "This is /test/ code."))

(test-equal "parse-org inline strong parsing"
            "This is <strong>test</strong> code."
            (parse-org "This is *test* code."))

(test-equal "parse-org inline combined formatting"
            "This is <strong><em>test</em></strong> code."
            (parse-org "This is */test/* code."))

(test-equal "parse-org consecutive inline formatting"
 "<strong>bold</strong> <em>italic</em> <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">code</code>"
 (parse-org "*bold* /italic/ ~code~"))

(test-equal "parse-org mixed block and inline formatting"
 "Inline <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">code</code> before block: <pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">block code</pre> after block."
 (parse-org "Inline ~code~ before block: #+begin_src
block code
#+end_src after block."))

(test-equal "parse-org multiline inline formatting"
            "This is <em>multi\nline</em> italic text."
            (parse-org "This is /multi\nline/ italic text."))

(test-equal "parse-org inline link parsing"
 "This is <a class=\"text-blue-800 font-bold\" href=\"https://www.google.com\">some link</a> code."
 (parse-org "This is [[https://www.google.com][some link]] code."))

(test-equal "parse-org inline link parsing of same desc"
 "This is <a class=\"text-blue-800 font-bold\" href=\"https://www.google.com\">https://www.google.com</a> code."
 (parse-org "This is [[https://www.google.com][https://www.google.com]] code."))

(test-end)
