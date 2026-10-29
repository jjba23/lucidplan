;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The filesystem side of ticket attachments.  What a browser sends as a
;;; filename is data, never a path: the application stores the bytes under a
;;; name it generated itself, keeps the submitted name in the database for
;;; the download header, and cleans that name before it is written into a
;;; header.  A file therefore cannot escape the attachment directory however
;;; it was named.
;;;
;;; A file now lives in the row that describes it, in the database, so a ticket
;;; travels with what it carries.  `attachment-directory' and `attachment-path'
;;; are what remain of the directory files used to live in: they are how a file
;;; uploaded before that change is found once, stored, and never read again
;;; (see the download route).  They can go when no instance has an old file
;;; left.

(define-module (lucidplan attachments)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (artanis upload)
  #:use-module (rnrs bytevectors)
  #:use-module (lucidplan config)
  #:export (attachment-directory attachment-path
                                 uploaded-file-of
                                 uploaded-file-bytes
                                 safe-attachment-filename
                                 safe-content-type
                                 attachment-disposition
                                 attachment-size-label))

(define (attachment-directory)
  "Answer the directory this instance keeps uploaded files in."
  (config-ref 'attachment-directory))

(define (attachment-path stored-name)
  "Answer the filesystem path the bytes of STORED-NAME live at."
  (format #f "~a/~a"
          (attachment-directory) stored-name))

(define (uploaded-file-bytes rc mfd)
  "Answer the bytes of one uploaded file MFD, as they arrived, or #f when the
request's body is not there to read them out of.

They are read out of the request body rather than out of a file, because the
application keeps what a ticket carries in the ticket's own row and a project's
picture in the project's (see the two migrations named `store_..._in_database').
`begin' and `end' are offsets into that body, and `end' is the index of the
last byte rather than the one past it -- artanis' own reader takes the same
view of them (`mfd-size' is `end - begin + 1'), which is why the copy stops at
`end + 1'.

@code{#f} is an answer rather than an error: the body this reads is the one the
framework read, and a request it did not read -- one sent with no
`Content-Length', in chunks, which is a body artanis does not read at all --
leaves no bytes to answer with.  A file that is not there is a form that is
answered with \"nothing was attached\", not a request that fails."
  ;; `bytevector-copy!' is R6RS's five-argument form here -- source and its
  ;; offset first, then the destination and its -- which is worth saying out
  ;; loud, since the shorter form every other language spells it as puts the
  ;; arguments the other way round and copies nothing but throws.
  (let ((body (rc-body rc))
        (start (mfd-begin mfd))
        (end (+ (mfd-end mfd) 1)))
    (and (bytevector? body)
         (>= start 0)
         (>= end start)
         (<= end (bytevector-length body))
         (let* ((length (- end start))
                (bytes (make-bytevector length 0)))
           (bytevector-copy! body start bytes 0 length)
           bytes))))

(define (uploaded-file-of rc)
  "Answer the first file the multipart request RC carries, or #f.

What the browser called the file is not what identifies it: the file is the
part of the form that has a filename at all, whether the form called it
`file', `avatar' or anything else, so the reader does not have to know the
name a template happened to give the control.

Nothing escapes from here.  A request the multipart reader cannot make sense of
-- a body that is not multipart, a body the framework never read because it
arrived without a length, a body that is not framed the way the reader expects
-- answers @code{#f}, which is the same answer as a form submitted with nothing
chosen.  That is deliberate: an upload that raises is answered with the
framework's own error page, and that page is written without a length and
without closing the connection, so the browser that is still sending its file
waits for an end that never comes.  An upload that answers @code{#f} is a
redirect the browser follows, which is what a reader can act on."
  (catch #t
         (lambda ()
           (let ((operator (get-mfds-op-from-post rc)))
             (and operator
                  (find (lambda (mfd)
                          (let ((name (mfd-filename mfd)))
                            (and name
                                 (not (string-null? name)))))
                        (mfds-operator-mfds operator)))))
         (lambda _
           #f)))

(define (safe-attachment-filename filename)
  "Answer FILENAME as a short name that is safe in a header.

Only the last path component is kept, so a name that arrived with
directory separators cannot suggest a path; quotes, backslashes, control
characters and NUL are replaced, and the result is cut to a length a header
can carry.  The value is only ever displayed and sent back -- the bytes on
disk are named by the stored name -- but a header is still not a place to
write submitted text verbatim."
  (let* ((name (if (and (string? filename)
                        (not (string-null? filename)))
                   (basename filename) "attachment"))
         (cleaned (list->string (map (lambda (c)
                                       (if (or (char<? c #\space)
                                               (memv c
                                                     (list #\" #\\ #\nul)))
                                           #\_ c))
                                     (string->list name))))
         (trimmed (string-trim-both cleaned)))
    (if (string-null? trimmed) "attachment"
        (if (> (string-length trimmed) 200)
            (substring trimmed 0 200) trimmed))))

(define (safe-content-type type)
  "Answer TYPE when it is a content type a header can carry, or the generic
binary one.

The type comes from a request, so it is checked rather than echoed: a value
made only of the characters a media type is written with is kept, and
anything else -- a line break that would split the header, say -- is
answered with `application/octet-stream'."
  (if (and (string? type)
           (not (string-null? type))
           (string-every (lambda (c)
                           (or (char-alphabetic? c)
                               (char-numeric? c)
                               (memv c
                                     (list #\/ #\. #\+ #\- #\_)))) type)) type
      "application/octet-stream"))

(define (attachment-disposition filename)
  "Answer the Content-Disposition value a download of FILENAME is sent with.

The answer is the structured form Guile's response writer takes -- a
disposition type with its parameters -- rather than the header's text: a bare
string is refused for this header with \"Bad value for header\", so the value
is assembled as the writer's own vocabulary describes it, `(attachment
(params ...))'.  The ASCII-safe name is carried the way every browser
understands, and the original name is carried again as the RFC 5987 UTF-8 form
so a name outside ASCII arrives intact rather than as question marks."
  (let ((safe (safe-attachment-filename filename)))
    (list 'attachment
          (cons 'filename safe)
          (cons 'filename*
                (format #f "UTF-8''~a"
                        (uri-encode filename))))))

(define (attachment-size-label bytes)
  "Answer SIZE in bytes as the short text a reader can scan."
  (cond
    ((not (and (number? bytes)
               (>= bytes 0)))
     "")
    ((< bytes 1024)
     (format #f "~a B" bytes))
    ((< bytes
        (* 1024 1024))
     (format #f "~,1f KB"
             (/ bytes 1024.0)))
    (else (format #f "~,1f MB"
                  (/ bytes
                     (* 1024.0 1024.0))))))
