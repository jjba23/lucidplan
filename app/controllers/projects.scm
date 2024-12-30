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

(define-artanis-controller projects)

(import (app views projects)
        (web uri)
        (ice-9 string-fun)
        (lucidplan uri)
        (lucidplan uuid)
        (app models project))

(get "/projects"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (html-from-projects ($project 'get
                                                           #:order-by
                                                           "title ASC"))))))

(get "/projects/new"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (html-new-project-form)))))

(post "/projects/delete"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (id (uri-decode (car (assoc-ref kv "id"))))
               (new-project-projects (assoc-remove! (assoc-ref projects
                                                               'projects) id)))
          (DB-query (DB-open rc)
                    (->sql delete from
                           'project
                           (where #:id id)))

          (redirect-to rc "/projects"))))

(post "/projects/new-submit"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (title (uri-decode (car (assoc-ref kv "title"))))
               (human-id (uri-decode (car (assoc-ref kv "human-id"))))
               (new-uuid (string-replace-substring (uuidgen) "\n" "")))
          
          ($project 'set
                    #:id new-uuid
                    #:human_id human-id
                    #:title title
                    #:created_at (current-time))

          (redirect-to rc "/projects"))))

(get "/projects/:project-id/edit"
     #:mime 'xml
     (lambda (rc)
       (let ((p (car ($project 'get
                               #:ret 'top
                               #:condition (where #:id (params rc "project-id"))))))
         (response-emit (:mime rc
                               (html-edit-project-form p))))))

(post "/projects/:project-id/edit"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (title (uri-decode (car (assoc-ref kv "title"))))
               (human-id (uri-decode (car (assoc-ref kv "human-id")))))
          
          (DB-query (DB-open rc)
                    (->sql update
                           'project set
                           `((human_id ,human-id)
                             (title ,title)
                             (updated_at ,(current-time)))
                           (where #:id (params rc "project-id"))))

          (redirect-to rc "/projects"))))
