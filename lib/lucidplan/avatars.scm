;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The picture a project wears, and the one it wears when it has never been
;;; given one.
;;;
;;; A project is a place in this application -- somewhere tickets live and
;;; people meet -- and a list of them is read by the name of each.  A picture
;;; makes that list scannable at a glance in the way a column of names alone
;;; never is, which is why every tool of this kind grows avatars sooner or
;;; later.  What most of them get wrong is the project that has none: a grey
;;; placeholder, or a broken image, in a list where every row looks unfinished
;;; until somebody does some work.  So the fallback here is not an absence --
;;; it is a letter avatar, generated from the project's own name, in a colour
;;; that is a function of that name.  A project that has never been touched
;;; looks as deliberate as one that has.
;;;
;;; The two halves are therefore deliberately apart.  The *stored* half lives
;;; here: what may be kept, how big it may be, and what identifies one version
;;; of it.  The *drawn* half is the view's, because it is a colour and a
;;; letter rather than a picture -- see `project-avatar' in `(app views
;;; projects)' for the markup and `lp-avatar' in `pub/css/lucidplan-luxe.css'
;;; for the shape.
;;;
;;; The bytes are kept in the database, in the project's own row (see
;;; `2026-10-28_18:00:00_store_project_avatar_in_database.sql'), rather than in
;;; a directory beside the instance.  A picture is small, it is drawn in a
;;; list, and it has to survive a copy of the database taken without the
;;; directories around it -- which is the one thing a picture in a directory
;;; does not do.  Nothing here touches the filesystem, and what an upload was
;;; called -- `../../x.png' is a perfectly ordinary filename on some systems --
;;; never becomes a path or a header.

(define-module (lucidplan avatars)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (lucidplan config)
  #:use-module (uuid generate)
  #:export (avatar-image-types avatar-canonical-type avatar-token
                            avatar-max-bytes avatar-initial avatar-hue))

;;; ===== What may be stored =====

;; A picture and nothing else.  The list is short on purpose and every entry
;; is a raster format: an SVG is a document that can carry script, and
;; although these bytes are served with the type named below rather than the
;; type the request claimed, the safest way to keep a picture from being a
;; document is to refuse the document.  An upload of anything not here is
;; refused rather than stored under a different name, because a file renamed
;; to a type it is not is a file nothing can open.
(define avatar-image-types
  '(("image/png" . "png")
    ("image/jpeg" . "jpg")
    ("image/gif" . "gif")
    ("image/webp" . "webp")))

(define (avatar-canonical-type type)
  "Answer the type a TYPE upload is kept as, or #f when it is not a picture
this instance keeps.

TYPE is what the browser claimed the file is, so it is read as a hint rather
than a fact: it decides which of the four names above the bytes are filed
under, and the bytes are then handed back with *that* name -- never with the
claim.  A file whose claim was wrong is served as what it was filed as, which
is a picture the browser fails to draw: the one failure that costs nothing."
  (let ((pair (and (string? type)
                   (assoc (string-downcase (string-trim-both type))
                          avatar-image-types))))
    (and pair
         (car pair))))

(define (avatar-token)
  "Answer a fresh token, the name of one version of a project's picture.

It is a UUID with nothing appended, and it is what a page puts after the
picture's address (`.../avatar?v=<token>').  A picture can therefore be served
`immutable' -- kept for ever by every cache between here and the reader -- and
still be replaced, because the address a page names changes when the picture
does and a reader with an old page asks for the old address and gets what that
address always meant."
  (string-replace-substring (generate-string-uuid) "\n" ""))

(define (avatar-max-bytes)
  "Answer the largest picture this instance accepts, in bytes."
  (config-ref 'avatar-max-bytes))

;;; ===== The picture a project has not been given =====

;; A letter and a colour, both a function of the project's own name, so the
;; same project is the same avatar on every page, in every session, on every
;; instance this database is copied to -- and two projects are unlikely to be
;; the same colour by accident.  That the colour is *derived* rather than
;; chosen is the point: nobody has to pick one, and a project created in a
;; hurry still looks finished.

(define (avatar-initial title)
  "Answer the one letter TITLE is drawn as.

The first letter or digit of the name, so a project called `2026 roadmap'
keeps its year rather than being drawn as the `r' of its second word.  A
name with nothing to draw -- an empty one, which the schema does not allow
but a view should survive -- is answered as a question mark rather than as an
empty avatar nobody can see."
  (let loop ((characters (string->list (if (string? title) title ""))))
    (cond
      ((null? characters) "?")
      ((or (char-alphabetic? (car characters))
           (char-numeric? (car characters)))
       (string (char-upcase (car characters))))
      (else (loop (cdr characters))))))

(define (avatar-hue seed)
  "Answer the hue, in degrees, a project called SEED is drawn in.

The name is folded into a number and the number into the wheel.  The
multiplier spreads names that differ in one letter far apart -- a plain sum
would give `alpha' and `alphb' adjacent hues, which is exactly the pair a list
has to tell apart -- and the modulus keeps the answer a hue rather than any
number at all."
  (let loop ((characters (string->list (if (string? seed) seed "")))
             (hash 0))
    (if (null? characters) (modulo (abs hash) 360)
        (loop (cdr characters)
              (modulo (+ (* hash 31)
                         (char->integer (car characters)))
                      4096)))))
