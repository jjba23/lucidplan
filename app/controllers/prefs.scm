;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller prefs)

(import (ice-9 string-fun)
        (kracht prelude)
        (lucidplan html))

;; A reader's preferences are not session state -- they are what the reader
;; wants the pages to be -- so the choice outlives the visit that made it:
;; the cookie stands for a year rather than lapsing with the session.
;;
;; The language is only ever one the page offers.  A value that is not in
;; `lucidplan-locales' is not written: the cookie is what every route builds
;; its translator from, and a value nothing knows how to answer in would be
;; a request to render the whole application in a language that does not
;; exist.
(post "/preferences/locale"
      #:cookies '(names lang)
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (user-lang (assoc-ref body "user-lang"))
               (return-to (assoc-ref body "return-to"))
               ;; Where the choice was made, so a reader who changes the
               ;; language on a ticket page stays on that ticket.  Only a
               ;; path within this site is followed: a form field is a
               ;; reader's to fill in, and one beginning `//' would send
               ;; them to another host entirely.
               (target (if (and (string? return-to)
                                (string-prefix? "/" return-to)
                                (not (string-prefix? "//" return-to)))
                           return-to "/")))
          (when (assoc user-lang
                       (lucidplan-locales))
            (:cookies-set! rc
                           'lang "lang" user-lang)
            (:cookies-setattr! rc
                               'lang
                               #:expires (* 3600 24 365)
                               #:path "/"
                               #:secure #t
                               #:http-only #t))
          ;; The reader is sent back to the page the choice was made on,
          ;; which is where a language change is easiest to read.  The
          ;; framework turns the path into an absolute address on the
          ;; instance's own name, as it does for every other redirect here.
          (redirect-to rc target
                       #:status 302))))
