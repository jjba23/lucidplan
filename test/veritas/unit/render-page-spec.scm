;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; What a page writes, and what it leaves alone.
;;;
;;; A page is data and the writer is the one place its text meets HTML, so the
;;; rule is kept here: a string is text and is escaped once, while the bodies
;;; of `script' and `style' are text the browser reads as code and CSS and are
;;; written as they are.  Org prose arrives as markup already -- `org2html'
;;; answers a node the writer splices -- and is not escaped a second time.
;;;
;;; This is what lets the repositories store what a reader typed rather than
;;; what a page needs.

(define-module (veritas unit render-page-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan html)
  #:export (spec))

(define escaping-suite
  (suite "a page writes text as text"
         (test "a string is escaped exactly once"
               (assert-equal #:name "markup characters in text are escaped"
                             #:expect "<div>a &lt; b &amp; c</div>"
                             #:got (render-page '(div "a < b & c"))))
         (test "a script body is code, not text"
               (assert-equal #:name "a less-than in a script stays a less-than"
                             #:expect "<script>if (a < b) {}</script>"
                             #:got (render-page '(script "if (a < b) {}"))))
         (test "a style body is CSS, not text"
               (assert-equal #:name "a child selector in a style stays a selector"
                             #:expect "<style>.a > .b { color: red; }</style>"
                             #:got (render-page '(style ".a > .b { color: red; }"))))
         (test "org prose is markup already"
               (assert-equal #:name "org prose keeps the markup org made"
                             #:expect #t
                             #:got (if (string-contains
                                        (render-page `(div ,(org2html
                                                             "a *bold* word")))
                                        "<strong")
                                       #t #f)))))

(define (spec)
  (veritas-run escaping-suite))
