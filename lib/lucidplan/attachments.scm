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

;; You should have received a copy of the GNU Affero General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

;;; The filesystem side of ticket attachments.  What a browser sends as a
;;; filename is data, never a path: the application stores the bytes under a
;;; name it generated itself, keeps the submitted name in the database for
;;; the download header, and cleans that name before it is written into a
;;; header.  A file therefore cannot escape the attachment directory however
;;; it was named.
;;;
;;; The directory lives outside `pub/' (see `attachment-directory' in
;;; `(lucidplan config)'), so a file is served by the application, which can
;;; check that the caller named a ticket the file belongs to, instead of by
;;; the static file server, which cannot.

(define-module (lucidplan attachments)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:use-module (lucidplan config)
  #:export (attachment-directory attachment-path
                                 ensure-attachment-directory
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

(define (ensure-attachment-directory)
  "Create the attachment directory, and its parents, if they are not there."
  (let loop
    ((path (attachment-directory)))
    (unless (file-exists? path)
      (let ((parent (dirname path)))
        (unless (or (equal? parent path)
                    (string-null? parent)
                    (equal? parent "."))
          (loop parent)))
      (catch 'system-error
             (lambda ()
               (mkdir path))
             (lambda (key . args)
               ;; Another request may have created it between the check and the
               ;; call; that is the outcome this wanted, not an error.
               (unless (file-exists? path)
                 (apply throw key args)))))))

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
