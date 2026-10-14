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

;;; ===== Handing a message to the mail service =====

;;; Pingwing is the mail and notification service this instance speaks to, and
;;; it is a network service: it can be restarting, slow, or briefly
;;; unreachable.  A password reset, an invite or a word that a ticket has
;;; arrived that quietly never arrives is worse than one that is tried again,
;;; so a message is posted with a few attempts and a growing wait between
;;; them, and only after the last attempt fails is the failure reported --
;;; never as an exception that would abandon the request that asked for it,
;;; but as a value its caller can act on.
;;;
;;; What lives here is the service and the envelope: the address a task is
;;; posted to, the credentials, the attempts, and the fields every message
;;; carries because the instance, not the caller, decides them -- the sender
;;; and the subject prefix.  What a particular message *says* is the caller's:
;;; it names a template pingwing knows and fills in that template's own
;;; variables, so a new message is a template in pingwing and one call here.
;;;
;;; Everything a caller hands over becomes JSON in one place, so the shape of
;;; the request is written down once: a call either posts the payload the
;;; service documents (see pingwing's README, `POST /api/v1/tasks'), or it
;;; does not post at all.
;;;
;;; Two readings matter to a caller, and the transfer alone gives neither.  A
;;; request that never left is not an exception: curl answers with an error
;;; code and a reason behind it.  And a request that was made and refused is
;;; not an error as far as curl is concerned -- a 400 from pingwing is a
;;; transfer that went through -- so the status is read out of the answer's
;;; own status line instead (the answer's headers are asked for along with its
;;; body).  A message is therefore only reported as sent when the service
;;; said it took it.

(define-module (lucidplan mail)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (curl)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (lucidplan config)
  #:use-module (lucidplan env)
  #:export (send-email! send-to-pingwing))

;;; A word about what happened, on the error port every handler shares and the
;;; deployment's own log collects -- never on the answer the reader is being
;;; sent, and never as a thrown exception that would abandon the request that
;;; asked for the message.
(define (say . parts)
  (apply format (current-error-port)
         (string-append "pingwing: " (car parts) "~%")
         (cdr parts)))

(define (status-of headers)
  "The number in the status line of HEADERS, or @code{#f} when there is none.

HEADERS is what curl hands back when it is asked for the answer's headers too:
that answer's own lines, status line first, as `HTTP/1.1 201 Created'."
  (if (string? headers)
      (let* ((line (car (string-split headers #\newline)))
             (words (string-split line #\space)))
        (if (>= (length words) 2)
            (string->number (cadr words))
            #f))
      #f))

(define (answer-body answer)
  "ANSWER's own words about the refusal, as text, or the empty string.

The service says why in the body of a refused request, and that body is the
whole reason the status is read from the answer rather than from curl: a
message that reports a refusal without repeating it has told the operator
nothing they can act on."
  (string-trim-both
   (cond
     ;; An answer asked for its headers comes back as the header text and the
     ;; body beside it, and the body is what curl read: one or more chunks of
     ;; bytes, which are joined here rather than each said separately.
     ((pair? answer) (body-text (cdr answer)))
     (else (body-text answer)))))

(define (body-text body)
  "BODY -- what curl read of a response -- as text, or the empty string."
  (cond
    ((bytevector? body) (utf8->string body))
    ((list? body)
     (apply string-append
            (map (lambda (chunk)
                   (if (bytevector? chunk) (utf8->string chunk) ""))
                 body)))
    (else "")))

(define (pingwing-post body)
  "Post BODY to the notification service once.

Answer @code{#t} only when the service answered that it took the task, and
@code{#f} for everything else -- a request that never left, and a request that
was made and refused, which is the difference the retrying turns on."
  (let ((handle (curl-easy-init)))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (curl-easy-setopt handle 'post #t)
       (curl-easy-setopt handle 'url
                         (format #f "~a/api/v1/tasks" pingwing-url))
       ;; The API takes a JSON object, and curl's own word for a request with a
       ;; body is `application/x-www-form-urlencoded' -- which is not what is
       ;; being posted, so it is said here rather than left to a default.
       (curl-easy-setopt handle 'httpheader
                         (list "Content-Type: application/json"))
       (curl-easy-setopt handle 'postfields
                         (string->utf8 (scm->json-string body)))
       (curl-easy-setopt handle 'use-ssl 1)
       (curl-easy-setopt handle 'username pingwing-basic-auth-user)
       (curl-easy-setopt handle 'password pingwing-basic-auth-password)
       (let* ((answer (curl-easy-perform handle #t #t))
              (code (catch #t
                           (lambda () (curl-error-code))
                           (lambda _ -1))))
         (cond
           ((not (and (number? code) (zero? code)))
            (say "the request did not go through (curl ~a): ~a"
                 code
                 (catch #t
                        (lambda () (curl-error-string))
                        (lambda _ "")))
            #f)
           ((not (pair? answer))
            (say "the service answered with no headers this could read")
            #f)
           (else
            (let ((status (status-of (car answer))))
              (cond
                ((not status)
                 (say "the service answered with no status this could read")
                 #f)
                ((and (>= status 200) (< status 300))
                 (say "notification taken (~a)" status)
                 #t)
                (else
                 (let ((why (answer-body answer)))
                   (say "the service refused it (~a)~a"
                        status
                        (if (string-null? why) "" (format #f ": ~a" why))))
                 #f)))))))
     (lambda () (curl-easy-cleanup handle)))))

(define* (send-to-pingwing #:key body)
  "Hand BODY to the notification service, retrying a few times.

The number of attempts and the base wait between them are configuration
(@code{mail-retry-attempts}, @code{mail-retry-delay}); the wait doubles after
each failed attempt, so a service that is coming back has room to.  Answer
@code{#t} when a post succeeded and @code{#f} when every attempt failed."
  (let ((attempts (max 1 (config-ref 'mail-retry-attempts)))
        (base-delay (max 0 (config-ref 'mail-retry-delay))))
    (let loop ((attempt 1))
      (let ((ok (catch #t
                      (lambda () (pingwing-post body))
                      (lambda (key . args)
                        (say "attempt ~a threw ~a: ~a" attempt key args)
                        #f))))
        (cond
          (ok #t)
          ((< attempt attempts)
           (say "not taken yet (attempt ~a of ~a); waiting ~as"
                attempt attempts (* base-delay (expt 2 (- attempt 1))))
           (sleep (* base-delay (expt 2 (- attempt 1))))
           (loop (+ attempt 1)))
          (else
           (say "giving up after ~a attempts" attempts)
           #f))))))

(define (recipient-list recipients)
  "RECIPIENTS as the service reads them: one object of a name and an address
for each, and the same address only once, whoever named it."
  (let loop ((people recipients) (seen '()) (out '()))
    (cond
      ((null? people) (list->vector (reverse out)))
      (else
       (let* ((person (car people))
              (name (car person))
              (address (cdr person)))
         (if (or (not (string? address))
                 (string-null? address)
                 (member address seen))
             (loop (cdr people) seen out)
             (loop (cdr people)
                   (cons address seen)
                   (cons `((name . ,(if (string? name) name ""))
                           (address . ,address))
                         out))))))))

(define* (send-email! #:key template vars subject recipients)
  "Ask the notification service to send one message.

@var{template} names a template the service knows and @var{vars} is an
association list of the values that template reads.  @var{recipients} is a
list of @code{(name . address)} pairs; @var{subject} is written after the
instance's own subject prefix, so the caller words the message and the
instance marks it as its own.  The sender's name and address are the
instance's configuration, not the caller's.

Answer @code{#f} when there is nobody to write to -- a message with no
recipient is not a message -- and otherwise what @code{send-to-pingwing}
answers: @code{#t} once the service accepted it, @code{#f} when every attempt
failed."
  (let ((to (recipient-list recipients)))
    (and (> (vector-length to) 0)
         (send-to-pingwing
          #:body `((task-type . send-email)
                   (template . ,template)
                   (template-vars ,@vars)
                   (sender-name . ,(config-ref 'mail-sender-name))
                   (sender-address . ,(config-ref 'mail-sender-address))
                   (subject . ,(format #f "~a~a"
                                       (config-ref 'mail-subject-prefix)
                                       subject))
                   (recipients . ,to))))))
