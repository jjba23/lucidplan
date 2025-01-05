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
                (project-tickets ($ticket 'get
                                          #:condition (where #:project_id
                                                             project-id)
                                          #:order-by "human_id ASC"))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assoc-ref t "id") unquote
                                                (assoc-ref t "human_id")))
                                            ($ticket 'get
                                                     #:columns '(id human_id)
                                                     #:order-by "human_id ASC"))))
           (display ticket-id-to-human-id)
           (response-emit (:mime rc
                                 (html-from-tickets (params rc "project")
                                                    project-tickets
                                                    ticket-id-to-human-id)))))
       (cond
         ((:session rc
                    'check)
          (do-get))
         ((:auth rc)
          (:session rc
                    'spawn)
          (do-get))
         (else (redirect-to rc
                            (string->uri (format #f "~a/login"
                                                 (current-myhost)))
                            #:status 302)))))

(get "/tickets/:project/new"
     #:mime 'xml
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get-new)
         (response-emit (:mime rc
                               (html-new-ticket-form (params rc "project")))))
       (cond
         ((:session rc
                    'check)
          (do-get-new))
         ((:auth rc)
          (:session rc
                    'spawn)
          (do-get-new))
         (else (redirect-to rc
                            (string->uri (format #f "~a/login"
                                                 (current-myhost)))
                            #:status 302)))))

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
                         (format #f "/tickets/~a"
                                 (params rc "project"))
                         #:status 302)))
        (cond
         ((:session rc
           'check)
          (do-delete))
         ((:auth rc)
          (:session rc
           'spawn)
          (do-delete))
         (else (redirect-to rc
                            (string->uri (format #f "~a/login"
                                                 (current-myhost)))
                            #:status 302)))))

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
                 (child-of (uri-decode (car (assoc-ref kv "child-of"))))
                 (blocked-by (uri-decode (car (assoc-ref kv "blocked-by"))))
                 (splits-from (uri-decode (car (assoc-ref kv "splits-from"))))
                 (clone-of (uri-decode (car (assoc-ref kv "clone-of"))))
                 (blocks (uri-decode (car (assoc-ref kv "blocks"))))
                 (related-to (uri-decode (car (assoc-ref kv "related-to"))))
                 (project-id (project-id-from-human-id (params rc "project")))
                 (new-uuid (string-replace-substring (uuidgen) "\n" "")))
            
            ($ticket 'set
                     #:id new-uuid
                     #:project_id project-id
                     #:title (safe-base64-encode title)
                     #:ticket_status (safe-base64-encode ticket-status)
                     #:ticket_type (safe-base64-encode ticket-type)
                     #:description (safe-base64-encode description)
                     #:created_at (current-time)
                     #:child_of (safe-base64-encode child-of)
                     #:blocked_by (safe-base64-encode blocked-by)
                     #:splits_from (safe-base64-encode splits-from)
                     #:clone_of (safe-base64-encode clone-of)
                     #:blocks (safe-base64-encode blocks)
                     #:related_to (safe-base64-encode related-to))

            (redirect-to rc
                         (string->uri (format #f "/tickets/~a"
                                              (params rc "project")))
                         #:status 302)))
        (cond
          ((:session rc
                     'check)
           (do-new))
          ((:auth rc)
           (:session rc
                     'spawn)
           (do-new))
          (else (redirect-to rc
                             (string->uri (format #f "~a/login"
                                                  (current-myhost)))
                             #:status 302)))))

(get "/tickets/:project/:ticket-id/edit"
     #:mime 'xml
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (get-edit)
         (let ((t (car ($ticket 'get
                                #:ret 'top
                                #:condition (where #:id (params rc "ticket-id"))))))
           (response-emit (:mime rc
                                 (html-edit-ticket-form (params rc "project")
                                                        t)))))
       (cond
         ((:session rc
                    'check)
          (get-edit))
         ((:auth rc)
          (:session rc
                    'spawn)
          (get-edit))
         (else (redirect-to rc
                            (string->uri (format #f "~a/login"
                                                 (current-myhost)))
                            #:status 302)))))

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
                 (child-of (uri-decode (car (assoc-ref kv "child-of"))))
                 (blocked-by (uri-decode (car (assoc-ref kv "blocked-by"))))
                 (splits-from (uri-decode (car (assoc-ref kv "splits-from"))))
                 (clone-of (uri-decode (car (assoc-ref kv "clone-of"))))
                 (blocks (uri-decode (car (assoc-ref kv "blocks"))))
                 (related-to (uri-decode (car (assoc-ref kv "related-to"))))
                 (ticket-type (uri-decode (car (assoc-ref kv "ticket-type")))))
            
            (DB-query (DB-open rc)
                      (->sql update
                             'ticket set
                             `((ticket_status ,(safe-base64-encode
                                                ticket-status))
                               (ticket_type ,(safe-base64-encode ticket-type))
                               (description ,(safe-base64-encode description))
                               (title ,(safe-base64-encode title))
                               (updated_at ,(current-time))
                               (child_of ,(safe-base64-encode child-of))
                               (blocked_by ,(safe-base64-encode blocked-by))
                               (splits_from ,(safe-base64-encode splits-from))
                               (clone_of ,(safe-base64-encode clone-of))
                               (blocks ,(safe-base64-encode blocks))
                               (related_to ,(safe-base64-encode related-to)))
                             (where #:id (params rc "ticket-id"))))

            (redirect-to rc
                         (string->uri (format #f "/tickets/~a"
                                              (params rc "project")))
                         #:status 302)))
        (cond
          ((:session rc
                     'check)
           (do-edit))
          ((:auth rc)
           (:session rc
                     'spawn)
           (do-edit))
          (else (redirect-to rc
                             (string->uri (format #f "~a/login"
                                                  (current-myhost)))
                             #:status 302)))))
