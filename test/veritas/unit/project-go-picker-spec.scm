;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; What the bar's *go to* menu is made of, with a script and without one.
;;;
;;; The menu is a plain `<select>`: the value, the change event and the form
;;; submission are the native control's, and the control and the menu a reader
;;; presses are drawn around it by `pub/js/lp-select.js'.  What can go wrong in
;;; the markup is therefore what the page writes into the rows -- a row
;;; carrying the page's own ink was painted into the platform's dark popup,
;;; which is how the entries came to be unreadable -- and the name of the
;;; control, which the drawn button borrows because the words on it are the
;;; project that was chosen rather than what the control is for.
;;;
;;; Going to a project is the one thing here that needs a script, so it is also
;;; the one thing that needs a fallback: the menu is hidden without a script
;;; and the same projects are offered as links, which a reader without one can
;;; still follow.

(define-module (veritas unit project-go-picker-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan html)
  #:export (spec))

(define projects
  '(((human-id . "moon") (title . "Moon landing"))
    ((human-id . "rocket") (title . "Rocket science"))))

(define (translate msgid)
  "The translator a page is handed, said plainly: a test reads English."
  msgid)

(define (menu-of projects)
  (render-page (project-go-picker translate #:projects projects)))

(define picker-suite
  (suite "the go to menu is a select a script draws a menu for"
         (test "the menu is the native control the page wrote"
               (assert-equal #:name "the markup is a select, not a drawn box"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects) "<select ")
                                       #t #f)))
         (test "the control says what it is for"
               (assert-equal #:name "the control is named for what it chooses"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects) "aria-label=")
                                       #t #f)))
         (test "a project is a row that goes to its board"
               (assert-equal #:name "the row names the board it goes to"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects)
                                        "<option value=\"/tickets/moon\">Moon landing</option>")
                                       #t #f)))
         (test "a row carries no ink of its own"
               (assert-equal #:name "no row is painted by a utility class"
                             #:expect #f
                             #:got (string-contains (menu-of projects)
                                                    "text-onyx-700")))
         (test "a page of no projects offers no menu"
               (assert-equal #:name "nothing is written for an empty list"
                             #:expect ""
                             #:got (menu-of '())))))

(define no-script-suite
  (suite "the menu a reader without a script meets"
         (test "the menu is hidden, because it cannot go anywhere"
               (assert-equal #:name "the menu carries the class the no-script sheet hides"
                             #:expect #t
                             #:got (if (string-contains (menu-of projects)
                                                        "js-only")
                                       #t #f)))
         (test "the projects are offered as links instead"
               (assert-equal #:name "a project is a link to its board"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects)
                                        "<a href=\"/tickets/moon\"")
                                       #t #f)))
         (test "the links are inside the no-script block"
               (assert-equal #:name "a reader with a script is not offered both"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects) "<noscript><details")
                                       #t #f)))
         (test "the list names the same projects the menu does"
               (assert-equal #:name "the last project is a link too"
                             #:expect #t
                             #:got (if (string-contains
                                        (menu-of projects)
                                        "<a href=\"/tickets/rocket\"")
                                       #t #f)))))

(define (spec)
  (veritas-run picker-suite no-script-suite))
