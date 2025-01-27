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

(import (lucidplan auth)
        (web uri)
        (ice-9 string-fun)
        (lucidplan uri)
        (lucidplan uuid)
        (lucidplan base64)
        (artanis security nss)
        (app models project)
        (app views projects))

(get "/"
     (lambda (rc)
       (redirect-to rc
                    (string->uri (format #f "~a/projects"
                                         (current-myhost)))
                    #:status 302)))

(get "/projects"
     ;; #:auth '(table user "username" "password")
     #:session #t
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-p)
         (let* ((proj ($project 'get
                                #:order-by '(human_id
                                             asc))))
           (response-emit (:mime rc
                           (html-from-projects (:i18n rc) proj)))))
       (get-p)))

(get "/projects/new"
     ;; #:auth '(table user "username" "password")
     #:session #t
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-new)
         (response-emit (:mime rc
                         (html-new-project-form (:i18n rc) ($project 'get
                                                                     #:order-by '(human_id
                                                                                  asc))))))
       (get-new)))

(post "/projects/delete"
      #:auth '(table user "username" "password")
      #:session #t
      #:mime 'xml
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (id (uri-decode (car (assoc-ref kv "id")))))
            ($project 'mod 'row-delete (where #:id id))

            (redirect-to rc (string->uri (format #f "~a/projects" (current-myhost)))
                         #:status 302)))
        (protected-endpoint rc do-delete)))

(post "/projects/new"
      #:auth '(table user "username" "password")
      #:session #t
      #:mime 'xml
      (lambda (rc)
        (define (do-new)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (human-id (uri-decode (car (assoc-ref kv "human-id"))))
                 (new-uuid (string-replace-substring (uuidgen) "\n" "")))
            
            ($project 'set
                      #:id new-uuid
                      #:human_id (safe-base64-encode human-id)
                      #:title (safe-base64-encode title)
                      #:created_at (current-time))

            (redirect-to rc (string->uri (format #f "~a/projects" (current-myhost)))
                         #:status 302)))
        (protected-endpoint rc do-new)))

(get "/projects/:project-id/edit"
     ;; #:auth '(table user "username" "password")
     #:session #t
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (define (get-edit)
         (let ((p (car ($project 'get
                                 #:ret 'top
                                 #:condition (where #:id (params rc
                                                                 "project-id"))))))
           (response-emit (:mime rc
                           (html-edit-project-form (:i18n rc) ($project 'get
                                                                        #:order-by '(human_id
                                                                                     asc)) p)))))

       (get-edit)))

(post "/projects/:project-id/edit"
      #:auth '(table user "username" "password")
      #:session #t
      #:mime 'xml
      (lambda (rc)
        (define (do-edit)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (human-id (uri-decode (car (assoc-ref kv "human-id")))))
            
            (DB-query (DB-open rc)
                      (->sql update
                             'project set
                             `((human_id ,(safe-base64-encode human-id))
                               (title ,(safe-base64-encode title))
                               (updated_at ,(current-time)))
                             (where #:id (params rc "project-id"))))

            (redirect-to rc (string->uri (format #f "~a/projects" (current-myhost)))
                         #:status 302)))
        (protected-endpoint rc do-edit)))

