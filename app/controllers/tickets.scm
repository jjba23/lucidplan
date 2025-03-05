;;; index.scm

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

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
        (lucidplan abstraction)
        (lucidplan query-escape)
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
     #:session #t
     #:i18n '(cookie "lang")
     #:cookies '(names lucidplan-show-done lucidplan-show-rejected)
     (lambda (rc)
       (define (get)
         (let* ((project-id (project-id-from-human-id (params rc "project")))
                (with-done (cond
                            ((not (equal? #f (get-from-qstr rc "with-done")))
                             (:cookies-set! rc
                              'lucidplan-show-done
                              "lucidplan-show-done" (get-from-qstr rc "with-done"))
                             (:cookies-setattr! rc
                              'lucidplan-show-done
                              #:expires 12000000
                              #:path "/"
                              #:http-only #t)
                             (equal? (get-from-qstr rc "with-done") "true"))
                            ((not (equal? #f (:cookies-value rc "lucidplan-show-done")))
                             (equal? (:cookies-value rc "lucidplan-show-done") "true"))
                            (else #f)))
                (with-rejected #f)
                (proj ($project 'get
                                #:order-by '(human_id asc)))
                (write (params rc "project"))
                (project-details (car ($project 'get
                                                #:ret 'top
                                                #:condition (where #:human_id (params
                                                                               rc
                                                                               "project")))))
                (project-tickets ($ticket 'get
                                          #:condition (where
                                                       #:project_id
                                                       project-id)
                                          #:order-by '(updated_at desc)))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assoc-ref t "id") unquote
                                                (assoc-ref t "human_id")))
                                            ($ticket 'get
                                                     #:columns '(id human_id)
                                                     #:condition (where
                                                                  #:project_id
                                                                  project-id)
                                                     #:order-by '(human_id asc)))))
           
           (response-emit (:mime rc
                           (html-from-tickets (:i18n rc)
                                              #:projects proj
                                              #:project-details project-details
                                              #:tickets project-tickets
                                              #:ticket-id-to-human-id ticket-id-to-human-id
                                              #:with-done with-done
                                              #:with-rejected with-rejected)))))
       (get)))

(get "/tickets/:project/:ticket-id"
     #:mime 'xml
     #:session #t
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-single)
         (let* ((proj ($project 'get
                                #:order-by '(human_id asc)))
                (t (car ($ticket 'get
                                 #:ret 'top
                                 #:condition (where #:id (params rc
                                                                 "ticket-id"))))))
           (response-emit (:mime rc
                           (html-edit-ticket-form (:i18n rc) proj
                                                  (params rc "project")
                                                  t)))))
       (get-single)))

(get "/tickets/:project/new"
     #:mime 'xml
     #:session #t
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-new)
         (response-emit (:mime rc
                               (html-new-ticket-form (:i18n rc)
                                                     ($project 'get
                                                               #:order-by '(human_id
                                                                            asc))
                                                     (params rc "project")))))
       (get-new)))

(post "/tickets/:project/delete"
      #:mime 'xml
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (id (uri-decode (car (assoc-ref kv "id")))))
            
            ($ticket 'mod
                     'row-delete
                     (where #:id id))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
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
                     #:title (escape-str title)
                     #:ticket_status ticket-status
                     #:ticket_type ticket-type
                     #:description (escape-str description)
                     #:created_at (current-time))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (protected-endpoint rc do-new)))

(get "/tickets/:project/:human-id/edit"
     #:mime 'xml
     #:session #t
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-edit)
         (let* ((proj ($project 'get
                                #:order-by '(title asc)))
                (t (car ($ticket 'get
                                 #:ret 'top
                                 #:condition (where #:human_id (params rc
                                                                "human-id"))))))
           (response-emit (:mime rc
                                 (html-edit-ticket-form (:i18n rc) proj
                                                        (params rc "project")
                                                        t)))))
       (get-edit)))

(post "/tickets/:project/:human-id/edit"
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
                             `((ticket_status ,ticket-status)
                               (ticket_type ,ticket-type)
                               (description ,(escape-str description))
                               (title ,(escape-str title))
                               (updated_at ,(current-time)))
                             (where #:human_id (params rc "human-id"))))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (protected-endpoint rc do-edit)))
