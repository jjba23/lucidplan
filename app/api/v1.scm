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

(define-restful-api v1)

(import (lucidplan auth)
        (kracht prelude)
        (bewaking bewaking)
        (ice-9 textual-ports)
        (artanis security nss)
        (app models project)
        (app models user)
        (app models ticket))

(get "/api/v1/tickets/:project"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (:mime rc
                ($ticket 'get
                         #:condition (where #:project_id (params rc "project"))
                         #:order-by '(human_id asc))))
       (bewaking-api-endpoint #:token-name 'x-bewaking-lucidplan
                              #:needed-permissions '()
                              #:rc rc
                              #:happy-flow do-get
                              #:unhappy-flow unauthorized-api-message)))

(get "/api/v1/projects"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (:mime rc
                ($project 'get
                          #:order-by '(human_id asc))))
       (bewaking-api-endpoint #:token-name 'x-bewaking-lucidplan
                              #:needed-permissions '()
                              #:rc rc
                              #:happy-flow do-get
                              #:unhappy-flow unauthorized-api-message)))

(post "/api/v1/login"
      #:mime 'json
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (password (assoc-ref body "password"))
               (u ($user 'get
                         #:ret 'top
                         #:condition (where #:email email)))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assoc-ref (car u) "id"))
                                      (email unquote
                                             (assoc-ref (car u) "email"))
                                      (name unquote
                                            (assoc-ref (car u) "username"))
                                      (permissions unquote
                                                   (eval-string (assoc-ref (car
                                                                            u)
                                                                 "permissions"))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))
          
          (cond
            ((equal? (string->sha-256 (string-append password
                                                     (assoc-ref (car u) "salt")))
                     (assoc-ref (car u) "password"))
             (response-emit ""
                            #:status 204
                            #:headers `((x-bewaking-byggsteg unquote t))))
            (else (response-emit '((error . "invalid credentials, could not make token"))
                                 #:status 401
                                 #:headers '()))))))
