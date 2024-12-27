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
        (app models ticket))

(get "/backlog/:project"
     #:mime 'xml
     (lambda (rc)
       (let ((project-tickets ($ticket 'get
                                       #:condition
                                       (where #:project_id (params rc "project"))
                                       #:order-by "human_id ASC")))
         (response-emit (:mime rc
                         (html-from-backlog (params rc "project")
                                            project-tickets))))
       ))

(get "/backlog/:project/tickets/new"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                       (html-new-ticket-form (params rc "project"))))))

(post "/backlog/:project/tickets/delete"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (id (uri-decode (car (assoc-ref kv "id")))))

          (DB-query (DB-open rc) (->sql delete from 'tickets (where #:id id)))

          (redirect-to rc
                       (format #f "/backlog/~a"
                               (params rc "project"))))))

(post "/backlog/:project/tickets/new-submit"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (title (uri-decode (car (assoc-ref kv "title"))))
               (ticket-status (uri-decode (car (assoc-ref kv "ticket-status"))))
               (ticket-type (uri-decode (car (assoc-ref kv "ticket-type"))))
               (description (uri-decode (car (assoc-ref kv "description"))))
               (project-id ($project 'get #:condition (where #:human_id (params rc "project"))))
               (new-uuid (string-replace-substring (uuidgen) "\n" "")))

          ($ticket 'set
                   #:id new-uuid
                   #:project-id project-id
                   #:title title
                   #:ticket-status ticket-status
                   #:ticket-type ticket-type
                   #:description description)

          (redirect-to rc
                       (format #f "/backlog/~a"
                               (params rc "project"))))))

