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

(import (app views projects)
        (artanis security nss)
        (ice-9 string-fun)
        (kracht prelude)
        (lucidplan auth)
        (lucidplan sqlite util)
        (lucidplan sqlite project)
        (lucidplan domain project)
        (uuid generate)
        (web uri)
        (web uri))

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
                               (else (html-escape (get-from-qstr rc "search")))))
                (project-repo (make-sqlite-project-repository rc))
                (proj (cond
                        ((nil? user-search)
                         (get-projects project-repo))
                        (else (search-projects project-repo
                                               (html-escape user-search))))))
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
         (let* ((project-repo (make-sqlite-project-repository rc)))
           (response-emit (:mime rc
                                 (html-new-project-form (:i18n rc)
                                                        #:is-logged-in (:cookies-value
                                                                        rc
                                                                        "x-bewaking-lucidplan")
                                                        #:projects (get-projects
                                                                    project-repo))))))
       (get-new)))

(post "/projects/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id"))
                 (execute-write (db-write (DB-open rc))))
            (execute-write "soft-delete-project"
                           (sql-update 'project
                                       `((deleted-at unquote (current-time)))
                                       #:where (list (cons 'id id))))

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
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (url (uri-encode (assoc-ref body "url")))
                 (vcs-url (uri-encode (assoc-ref body "vcs-url")))
                 (human-id (assoc-ref body "human-id"))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" ""))
                 (project-repo (make-sqlite-project-repository rc)))
            
            (insert-project project-repo
                            `((id unquote new-uuid)
                              (human-id unquote human-id)
                              (title unquote title)
                              (description unquote description)
                              (url unquote url)
                              (vcs-url unquote vcs-url)
                              (created-at unquote
                                          (current-time))))
            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-new
                           #:unhappy-flow back-to-login)))

(get "/projects/id/:human-id/edit"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-edit)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (p (get-project-by-human-id project-repo
                                            (params rc "human-id"))))
           (response-emit (:mime rc
                                 (html-edit-project-form (:i18n rc)
                                                         #:is-logged-in (:cookies-value
                                                                         rc
                                                                         "x-bewaking-lucidplan")
                                                         #:projects (get-projects
                                                                     project-repo)
                                                         #:project p)))))

       (get-edit)))

(post "/projects/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (url (uri-encode (assoc-ref body "url")))
                 (vcs-url (uri-encode (assoc-ref body "vcs-url")))
                 (human-id (assoc-ref body "human-id"))
                 (project-repo (make-sqlite-project-repository rc)))
            (update-project-by-human-id project-repo
                                        (params rc "human-id")
                                        `((human-id unquote human-id)
                                          (url unquote url)
                                          (vcs-url unquote vcs-url)
                                          (description unquote description)
                                          (title unquote title)
                                          (updated-at unquote
                                                      (current-time))))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))
