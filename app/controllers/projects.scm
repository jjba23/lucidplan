;;; index.scm

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

(define-artanis-controller projects)

(import (lucidplan auth)
        (web uri)
        (ice-9 string-fun)
        (lucidplan uri)
        (uuid generate)
        (lucidplan query-escape)
        (artanis security nss)
        (app models project)
        (app views projects))

(get "/projects"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-p)
         (let* ((is-logged-in (:cookies-value rc "x-bewaking-lucidplan"))
                (user-search (cond
                               ((equal? #f
                                        (get-from-qstr rc "search"))
                                #f)
                               (else (escape-str (get-from-qstr rc "search")))))
                (proj (DB-get-all-rows (DB-query (DB-open rc)
                                                 (cond
                                                   ((equal? #f user-search)
                                                    (->sql select * from
                                                     'project
                                                     " ORDER BY title ASC"))
                                                   (else (->sql select * from
                                                                'project
                                                                (format #f
                                                                        (string-join '
                                                                         (" WHERE title LIKE '%~a%' OR description LIKE '%~a%' "
                                                                          " ORDER BY title ASC ")
                                                                         " ")
                                                                        (escape-str
                                                                         user-search)
                                                                        (escape-str
                                                                         user-search)))))))))
           (response-emit (:mime rc
                                 (html-from-projects (:i18n rc)
                                                     #:projects proj
                                                     #:search-query
                                                     user-search
                                                     #:is-logged-in
                                                     is-logged-in)))))
       (get-p)))

(get "/projects/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-new)
         (response-emit (:mime rc
                               (html-new-project-form (:i18n rc)
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-lucidplan")
                                                      #:projects ($project 'get
                                                                           #:order-by '
                                                                           (human_id
                                                                            asc))))))
       (get-new)))

(post "/projects/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (id (uri-decode (car (assoc-ref kv "id")))))
            ($project 'mod
                      'row-delete
                      (where #:id id))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))

(post "/projects/new"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-new)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (description (uri-decode (car (assoc-ref kv "description"))))
                 (url (uri-decode (car (assoc-ref kv "url"))))
                 (vcs-url (uri-decode (car (assoc-ref kv "vcs-url"))))
                 (human-id (uri-decode (car (assoc-ref kv "human-id"))))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" "")))
            
            ($project 'set
                      #:id new-uuid
                      #:human_id human-id
                      #:title (escape-str title)
                      #:description (escape-str description)
                      #:url url
                      #:vcs_url vcs-url
                      #:created_at (current-time))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-new
                           #:unhappy-flow back-to-login)))

(get "/projects/:human-id/edit"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-edit)
         (let* ((p (car ($project 'get
                                  #:ret 'top
                                  #:condition (where #:human_id (params rc
                                                                 "human-id"))))))
           (response-emit (:mime rc
                                 (html-edit-project-form (:i18n rc)
                                                         #:is-logged-in (:cookies-value
                                                                         rc
                                                                         "x-bewaking-lucidplan")
                                                         #:projects ($project 'get
                                                                              #:order-by '
                                                                              (human_id
                                                                               asc))
                                                         #:project p)))))

       (get-edit)))

(post "/projects/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (title (uri-decode (car (assoc-ref kv "title"))))
                 (description (uri-decode (car (assoc-ref kv "description"))))
                 (url (uri-decode (car (assoc-ref kv "url"))))
                 (vcs-url (uri-decode (car (assoc-ref kv "vcs-url"))))
                 (human-id (uri-decode (car (assoc-ref kv "human-id")))))
            
            (DB-query (DB-open rc)
                      (->sql update
                             'project set
                             `((human_id ,human-id)
                               (url ,url)
                               (vcs_url ,vcs-url)
                               (description ,(escape-str description))
                               (title ,(escape-str title))
                               (updated_at ,(current-time)))
                             (where #:human_id (params rc "human-id"))))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))

