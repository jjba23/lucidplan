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

(define-module (lucidplan auth)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (artanis route)
  #:use-module (bewaking bewaking)
  #:use-module (lucidplan env)
  #:export (back-to-login unauthorized-api-message bewaking-endpoint
                          bewaking-api-endpoint session-has-permission?
                          session-permissions))

(define* (bewaking-endpoint #:key token-name
                            needed-permissions
                            happy-flow
                            unhappy-flow
                            rc)
  "Protect an HTTP endpoint with bewaking, preventing unauthorized access and
executing either happy-flow-thunk or unhappy-flow-thunk."
  (cond
    ((not (equal? #f
                  (:cookies-value rc
                                  (symbol->string token-name))))
     (let* ((t (decrypt-token #:token (:cookies-value rc
                                                      (symbol->string
                                                       token-name))
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-needed-permissions (and t
                                         (has-needed-permissions? #:needed
                                          needed-permissions
                                          #:present (assoc-ref t
                                                               'owner-permissions)))))
       ;; `token-valid?' (bewaking >= 0.4.0) is the payload check and the
       ;; lifetime check in one place, so a token that is well-formed but has
       ;; aged out takes the unhappy path here.
       (cond
         ((and has-needed-permissions
               (token-valid? #:token t))
          (happy-flow))
         (else (unhappy-flow rc)))))
    (else (unhappy-flow rc))))

(define* (bewaking-api-endpoint #:key token-name
                                needed-permissions
                                happy-flow
                                unhappy-flow
                                rc)
  "Protect an HTTP REST endpoint with bewaking, preventing unauthorized access and
executing either happy-flow-thunk or unhappy-flow-thunk."
  (cond
    ((not (equal? #f
                  (get-header rc token-name)))
     (let* ((t (decrypt-token #:token (get-header rc token-name)
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-needed-permissions (and t
                                         (has-needed-permissions? #:needed
                                          needed-permissions
                                          #:present (assoc-ref t
                                                               'owner-permissions)))))
       (cond
         ((and has-needed-permissions
               (token-valid? #:token t))
          (happy-flow))
         (else (unhappy-flow rc)))))
    (else (unhappy-flow rc))))

;; A page and the guards on it must agree about who may do what.  The guard a
;; writing endpoint is wrapped in refuses a request that does not hold the
;; permission the write needs, but a page that *offers* the write -- a form,
;; or the small inline control on a ticket -- is rendered before any request
;; is made, so it has to ask the same question itself: whose permissions the
;; session carries.  These two answer that, from the same cookie and the same
;; token the guard would read, so a control is shown exactly when the endpoint
;; behind it would accept it.

(define* (session-permissions #:key rc token-name)
  "Answer the permissions of the request's session, or the empty list when
it carries no valid session.

@var{token-name} is the cookie name the token travels in, as the guards name
it.  A missing, malformed or expired token is not an error here -- it is a
reader who is simply signed out -- so it answers the empty list."
  (let ((raw (:cookies-value rc (symbol->string token-name))))
    (if (or (not (string? raw)) (string-null? raw))
        '()
        (catch #t
               (lambda ()
                 (let ((t (decrypt-token #:token raw
                                         #:encryption-key encryption-key
                                         #:signing-key signing-key)))
                   (if (and t (token-valid? #:token t))
                       (or (assoc-ref t 'owner-permissions) '())
                       '())))
               (lambda _ '())))))

(define* (session-has-permission? #:key rc token-name permission)
  "Answer @code{#t} when the request's session holds @var{permission}.

The question is asked of @code{session-permissions}, so it is the same
subset test the guards perform, and a page can decide whether to offer a
control by the same rule that decides whether the endpoint would honour it."
  (has-needed-permissions? #:needed (list permission)
                           #:present (session-permissions #:rc rc
                                                          #:token-name
                                                          token-name)))

(define (unauthorized-api-message rc)
  (response-emit '((error . "You must be authenticated to use this API endpoint"))
                 #:status 401))

(define (back-to-login rc)
  (redirect-to rc
               (string->uri (format #f "~a/login"
                                    (current-myhost)))
               #:status 302))
