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

(define-artanis-controller me)

(import (lucidplan html)
        (web uri)
        (ice-9 string-fun)
        (curl)
        (app models user)
        (lucidplan env)
        (lucidplan uri)
        (lucidplan auth)
        (app views me)
        (app models project)
        (bewaking bewaking))

(get "/me"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((t (decrypt-token #:token (:cookies-value rc
                                         "x-bewaking-lucidplan")
                                #:encryption-key encryption-key
                                #:signing-key signing-key))
              (proj ($project 'get
                              #:order-by '(human_id asc)))
              (user ($user 'get
                           #:ret 'top
                           #:condition (where #:id (assoc-ref t
                                                              'owner-id)))))
         (response-emit (:mime rc
                               (me-page (:i18n rc)
                                        #:user (car user)
                                        #:projects proj
                                        #:token t
                                        #:is-logged-in (:cookies-value rc
                                                        "x-bewaking-lucidplan")))))))

(get "/users/manage"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (go-to-users-management)
         (let* ((t (decrypt-token #:token (:cookies-value rc
                                           "x-bewaking-lucidplan")
                                  #:encryption-key encryption-key
                                  #:signing-key signing-key))
                
                (proj ($project 'get
                                #:order-by '(human_id asc)))
                (user ($user 'get
                             #:ret 'top
                             #:condition (where #:id (assoc-ref t
                                                                'owner-id))))
                (users ($user 'get
                              #:ret 'all
                              #:order-by '(username asc))))
           (response-emit (:mime rc
                                 (users-page (:i18n rc)
                                             #:users users
                                             #:projects proj
                                             #:token t
                                             #:is-logged-in (:cookies-value rc
                                                             "x-bewaking-lucidplan"))))))

       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-users-management
                          #:unhappy-flow back-to-login)))


;; (define (send-password-reset-mail)
;;   )

(post "/password/reset"
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (let* ((existing-token (if (:cookies-value rc "x-bewaking-lucidplan")
                                   (decrypt-token #:token (:cookies-value rc
                                                           "x-bewaking-lucidplan")
                                                  #:encryption-key
                                                  encryption-key
                                                  #:signing-key signing-key)
                                   #f))
               (body (rc-body rc))
               (email (if (not body) #f
                          (assoc-ref (read-url-encoded-body body) "email")))
               (u (if (not email)
                      ($user 'get
                             #:ret 'top
                             #:condition (where #:id (assoc-ref existing-token
                                                      'owner-id)))
                      ($user 'get
                             #:ret 'top
                             #:condition (where #:email (uri-decode (car email))))))
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
               (reset-token (generate-token #:encryption-key encryption-key
                                            #:signing-key signing-key
                                            #:get-owner-details
                                            get-owner-details
                                            #:token-validity-seconds (* 24
                                                                        3600)))
               (mail-payload `((task-type . send-email)
                               (template . password-reset)
                               (template-vars (system-name . LucidPlan)
                                              (user unquote
                                                    (assoc-ref (car u) "email"))
                                              (reset-link unquote
                                                          (format #f
                                                           "~a/password/reset/token/~a"
                                                           (current-myhost)
                                                           reset-token)))
                               (sender-name . "No Reply - LucidPlan")
                               (sender-address . "noreply@lucidplan.jointhefreeworld.org")
                               (subject . "LucidPlan - Password Reset")
                               (recipients (name unquote
                                                 (assoc-ref (car u) "username"))
                                           (address unquote
                                                    (assoc-ref (car u) "email"))))))
          (write (assoc-ref (assoc-ref mail-payload
                                       'template-vars)
                            'reset-link))
          (redirect-to rc
                       (string->uri (format #f "~a/?sent-password-reset=true"
                                            (current-myhost)))
                       #:status 302))))
