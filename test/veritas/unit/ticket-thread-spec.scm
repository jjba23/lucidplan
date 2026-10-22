;;; lucidplan - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;;; A ticket's discussion is a thread rather than a list, and the files it
;;; carries arrive named by whoever uploaded them.  Neither is trusted with
;;; much: the thread is walked by the parent each reply names -- data that
;;; could point at a cycle or at nothing -- and a filename is submitted text
;;; that ends up in a header and on a reader's disk.
;;;
;;; So what is checked here is that the walk always terminates and always
;;; answers a depth the page can indent by, and that only the last component
;;; of a submitted name survives into the header, cleaned of what a header
;;; cannot carry.  The bytes on disk are named by the application, never by
;;; the upload, which is why none of this is a path.
;;;
;;; The module is named `(veritas unit ticket-thread-spec)' because that is
;;; the name the veritas runner derives from where the file sits
;;; (test/veritas/unit), which is how `maak test' loads it.

(define-module (veritas unit ticket-thread-spec)
  #:use-module (veritas veritas)
  #:use-module (lucidplan domain ticket-comment)
  #:use-module (lucidplan attachments)
  #:export (spec))

;;; ====== the thread a ticket's comments make ======

;; Two comments on the ticket, one answering the first, one answering that
;; answer, and one more on the ticket.  The order is the order they were
;; written in, which is the order the page draws them in.
(define thread
  '(((id . "c1") (parent-id . #f)
     (body . "the first word"))
    ((id . "c2") (parent-id . "c1")
     (body . "an answer"))
    ((id . "c3") (parent-id . "c2")
     (body . "an answer to the answer"))
    ((id . "c4") (parent-id . #f)
     (body . "another word"))))

(define (comment-of id)
  (car (filter (lambda (comment)
                 (equal? (assq-ref comment
                                   'id) id)) thread)))

(define thread-suite
  (suite "the thread a ticket's comments make"
         (test "the comments on the ticket itself are the ones it hangs from"
               (assert-equal #:name
                             "the roots are the comments with no parent"
                             #:expect '("c1" "c4")
                             #:got (map (lambda (comment)
                                          (assq-ref comment
                                                    'id))
                                        (comment-children thread #f))))
         (test "a reply is drawn under what it answers"
               (assert-equal #:name "the answer to the first word"
                             #:expect '("c2")
                             #:got (map (lambda (comment)
                                          (assq-ref comment
                                                    'id))
                                        (comment-children thread "c1")))
               (assert-equal #:name "nothing answers the answer but one"
                             #:expect '("c3")
                             #:got (map (lambda (comment)
                                          (assq-ref comment
                                                    'id))
                                        (comment-children thread "c2"))))
         (test "a comment nothing answers has no children"
               (assert-equal #:name "the last word is the last word"
                             #:expect '()
                             #:got (comment-children thread "c4")))
         (test "the depth a comment is indented by is how far down it is"
               (assert-equal #:name "a comment on the ticket is not indented"
                             #:expect 0
                             #:got (comment-depth thread
                                                  (comment-of "c1")))
               (assert-equal #:name "a reply is indented once"
                             #:expect 1
                             #:got (comment-depth thread
                                                  (comment-of "c2")))
               (assert-equal #:name "a reply to a reply is indented twice"
                             #:expect 2
                             #:got (comment-depth thread
                                                  (comment-of "c3"))))
         (test "an id read back from the database names the same comment"
               ;; The parent of a row is whatever the database answered --
               ;; text here, a number there -- so the comparison is made on
               ;; the text of both rather than on their types.
               (assert-equal #:name "a numeric parent names the same comment"
                             #:expect 1
                             #:got (length (comment-children '(((id . "c1")
                                                                (parent-id . #f))
                                                               ((id . "c2")
                                                                (parent-id . 1)))
                                                             "1"))))
         (test "a thread that was written wrong still terminates"
               ;; Two comments that answer each other cannot be walked to a
               ;; root, and a parent that is not in the list cannot be walked
               ;; to at all.  Neither is trusted: the depth reached so far is
               ;; the answer, so the page is drawn rather than hung.
               (assert-equal #:name "a cycle is answered at the depth reached"
                             #:expect 2
                             #:got (comment-depth '(((id . "a")
                                                     (parent-id . "b"))
                                                    ((id . "b")
                                                     (parent-id . "a")))
                                                  '((id . "a")
                                                    (parent-id . "b"))))
               (assert-equal #:name "a missing parent is answered at the root"
                             #:expect 0
                             #:got (comment-depth thread
                                                  '((id . "c9")
                                                    (parent-id . "gone")))))))

;;; ====== the name a file is served under ======

(define attachment-name-suite
  (suite "the name a file is served under"
         (test "a name that is just a name is kept"
               (assert-equal #:name "a plain name survives"
                             #:expect "report.pdf"
                             #:got (safe-attachment-filename "report.pdf")))
         (test "only the last component of a submitted path is kept"
               ;; What a browser sends is data, never a path: the directory
               ;; part is dropped rather than cleaned up.
               (assert-equal #:name "a path is cut to its last component"
                             #:expect "passwd"
                             #:got (safe-attachment-filename
                                    "../../etc/passwd"))
               (assert-equal #:name
                "a separator a path could be written with does not survive"
                #:expect #f
                #:got (string-contains (safe-attachment-filename
                                        "C:\\Users\\joe\\notes.txt") "\\")))
         (test "what a header cannot carry is replaced"
               (assert-equal #:name "quotes and backslashes do not survive"
                             #:expect "my _report_.pdf"
                             #:got (safe-attachment-filename
                                    "my \"report\".pdf"))
               (assert-equal #:name "a line break is replaced"
                             #:expect "line_break.txt"
                             #:got (safe-attachment-filename "line\nbreak.txt")))
         (test "an empty name is still a name"
               (assert-equal #:name "nothing submitted answers the generic"
                             #:expect "attachment"
                             #:got (safe-attachment-filename ""))
               (assert-equal #:name
                             "a name of nothing but space answers it too"
                             #:expect "attachment"
                             #:got (safe-attachment-filename "   "))
               (assert-equal #:name "no name at all answers it too"
                             #:expect "attachment"
                             #:got (safe-attachment-filename #f)))
         (test "a name is cut to a length a header can carry"
               (assert-true (<= (string-length (safe-attachment-filename (make-string
                                                                          500
                                                                          #\a)))
                                200)
                            #:name "a very long name is shortened"))))

;;; ====== the type a file is served as ======

(define attachment-type-suite
  (suite "the type a file is served as"
         (test "a media type is kept"
               (assert-equal #:name "a type a header can carry survives"
                             #:expect "text/plain"
                             #:got (safe-content-type "text/plain"))
               (assert-equal #:name "a type the application made up survives"
                             #:expect
                             "application/vnd.oasis.opendocument.text"
                             #:got (safe-content-type
                                    "application/vnd.oasis.opendocument.text")))
         (test "anything a header cannot carry becomes the generic type"
               (assert-equal #:name "a line break would split the header"
                             #:expect "application/octet-stream"
                             #:got (safe-content-type
                                                      "text/plain\r\nX-Evil: 1"))
               (assert-equal #:name "a quoted type is not a type"
                             #:expect "application/octet-stream"
                             #:got (safe-content-type "text/plain\"x"))
               (assert-equal #:name "no type at all is the generic one"
                             #:expect "application/octet-stream"
                             #:got (safe-content-type #f))
               (assert-equal #:name "an empty type is the generic one"
                             #:expect "application/octet-stream"
                             #:got (safe-content-type "")))))

;;; ====== how a download is named ======

(define attachment-download-suite
  (suite "how a download is named"
         (test "both the plain and the encoded name are carried"
               ;; The plain form is what every browser understands; the
               ;; RFC 5987 form is what carries a name outside ASCII intact.
               ;; The value is the structured one Guile's writer takes: the
               ;; disposition and its parameters, never the header's text.
               (assert-equal #:name "the disposition carries the two names"
                             #:expect '(attachment
                                                   (filename . "my report.pdf")
                                                   (filename* . "UTF-8''my%20report.pdf"))
                             #:got (attachment-disposition "my report.pdf")))
         (test "the disposition is a type and its parameters"
               (assert-equal #:name "the type is the attachment disposition"
                             #:expect 'attachment
                             #:got (car (attachment-disposition "x.txt"))))
         (test "a path in the submitted name never reaches the header"
               (assert-equal #:name "the plain form is cut to the last name"
                             #:expect "hosts"
                             #:got (cdr (assq 'filename
                                              (cdr (attachment-disposition
                                                    "a/../../etc/hosts"))))))))
(define attachment-size-suite
  (suite "how big a file is, said in one line"
         (test "bytes are said as bytes"
               (assert-equal #:name "a small file is counted in bytes"
                             #:expect "512 B"
                             #:got (attachment-size-label 512))
               (assert-equal #:name "an empty file is counted too"
                             #:expect "0 B"
                             #:got (attachment-size-label 0)))
         (test "larger files are said in the unit a reader expects"
               (assert-equal #:name "a couple of kilobytes"
                             #:expect "2.0 KB"
                             #:got (attachment-size-label (* 2 1024)))
               (assert-equal #:name "a couple of megabytes"
                             #:expect "2.0 MB"
                             #:got (attachment-size-label (* 2 1024 1024))))
         (test "a size nobody knows is not a size"
               (assert-equal #:name "no size answers nothing"
                             #:expect ""
                             #:got (attachment-size-label #f)))))

(define (spec)
  (veritas-run thread-suite attachment-name-suite attachment-type-suite
               attachment-download-suite attachment-size-suite))
