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

(import (lucidplan persistence projects)
        (lucidplan html projects)
        (web uri)
        (ice-9 string-fun)
        (lucidplan uri)
        (lucidplan uuid)
        (app models project))

(get "/projects"
     #:mime 'xml
     (lambda (rc)
       (display ($project 'table-exists?))
       (response-emit (:mime rc
                       (html-from-projects (read-lucidplan-projects))))))


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
               (projects (read-lucidplan-projects))
               (new-project-projects (assoc-remove! (assoc-ref projects
                                                               'projects) id))
               (new-projects (assoc-set! projects
                                         'projects new-project-projects)))
          
          ;; (display "===========") (newline)
          ;; (display new-projects) (newline)
          ;; (display "===========") (newline)

          (write-lucidplan-projects new-projects)

          (redirect-to rc "/projects"))))

(post "/projects/new-submit"
      #:mime 'xml
      (lambda (rc)
        ;;(display ($project 'table-exists?))
        
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (title (uri-decode (car (assoc-ref kv "title"))))
               (human-id (uri-decode (car (assoc-ref kv "human-id"))))
               (projects (read-lucidplan-projects))
               (new-uuid (string-replace-substring (uuidgen) "\n" ""))
               (new-project `((id unquote new-uuid)
                              (human-id unquote human-id)
                              (title unquote title)
                              )))

          ($project 'set
                    #:id new-uuid
                    #:human_id human-id
                    #:title title
                    #:created_at (current-time))

          ;;(write-lucidplan-projects new-projects)

          (redirect-to rc "/projects"))))

