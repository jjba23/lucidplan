;;; index.scm

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

(define-artanis-controller tickets)

(import (app views tickets)
        (web uri)
        (ice-9 string-fun)
        (lucidplan auth)
        (lucidplan uri)
        (lucidplan uuid)
        (lucidplan base64)
        (app models ticket)
        (artanis security nss)
        (app models project))

(define (project-id-from-human-id project)
  (let ((p ($project 'get
                     #:ret 'top
                     #:condition (where #:human_id project))))
    (cond
      ((null? p)
       "")
      (else (assoc-ref (car p) "id")))))

(get "/tickets/:project"
     #:mime 'xml
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (let* ((project-id (project-id-from-human-id (params rc "project")))
                (proj ($project 'get
                                #:order-by '(human_id
                                             asc)))
                (project-tickets ($ticket 'get
                                          #:condition (where #:project_id
                                                             project-id)
                                          #:order-by '(human_id asc)))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assoc-ref t "id") unquote
                                                (assoc-ref t "human_id")))
                                            ($ticket 'get
                                                     #:columns '(id human_id)
                                                     #:order-by '(human_id asc)))))
           (response-emit (:mime rc
                           (html-from-tickets
                            proj
                            (params rc "project")
                            project-tickets
                            ticket-id-to-human-id)))))
       (protected-endpoint rc do-get)))

(get "/tickets/:project/new"
     #:mime 'xml
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get-new)
         (response-emit (:mime rc
                         (html-new-ticket-form ($project 'get
                                                         #:order-by '(human_id
                                                                      asc)) (params rc "project")))))
       (protected-endpoint rc do-get-new)))

(post "/tickets/:project/delete"
      #:mime 'xml
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (id (uri-decode (car (assoc-ref kv "id")))))

            ($ticket 'mod 'row-delete (where #:id id))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a" (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (protected-endpoint rc do-delete)))

(post "/tickets/:project/new"
      #:mime 'xml
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-new)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (ticket-status (uri-decode (car (assoc-ref kv "ticket-status"))))
                 (ticket-type (uri-decode (car (assoc-ref kv "ticket-type"))))
                 (description (uri-decode (car (assoc-ref kv "description"))))                 
                 (project-id (project-id-from-human-id (params rc "project")))
                 (new-uuid (string-replace-substring (uuidgen) "\n" "")))
            
            ($ticket 'set
                     #:id new-uuid
                     #:project_id project-id
                     #:title (safe-base64-encode title)
                     #:ticket_status (safe-base64-encode ticket-status)
                     #:ticket_type (safe-base64-encode ticket-type)
                     #:description (safe-base64-encode description)
                     #:created_at (current-time))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a" (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (protected-endpoint rc do-new)))

(get "/tickets/:project/:ticket-id/edit"
     #:mime 'xml
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (get-edit)
         (let ((proj ($project 'get
                               #:order-by '(human_id
                                            asc)))
               (t (car ($ticket 'get
                                #:ret 'top
                                #:condition (where #:id (params rc "ticket-id"))))))
           (response-emit (:mime rc
                           (html-edit-ticket-form proj (params rc "project")
                                                  t)))))
       (protected-endpoint rc get-edit)))

(post "/tickets/:project/:ticket-id/edit"
      #:mime 'xml
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-edit)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (description (uri-decode (car (assoc-ref kv "description"))))
                 (ticket-status (uri-decode (car (assoc-ref kv "ticket-status"))))
                 (ticket-type (uri-decode (car (assoc-ref kv "ticket-type")))))
            
            (DB-query (DB-open rc)
                      (->sql update
                             'ticket set
                             `((ticket_status ,(safe-base64-encode
                                                ticket-status))
                               (ticket_type ,(safe-base64-encode ticket-type))
                               (description ,(safe-base64-encode description))
                               (title ,(safe-base64-encode title))
                               (updated_at ,(current-time)))
                             (where #:id (params rc "ticket-id"))))
            (write (params rc "project"))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a" (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (protected-endpoint rc do-edit)))
