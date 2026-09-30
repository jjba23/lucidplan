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

(define-artanis-controller tickets)

(import (app views tickets)
        (web uri)
        (ice-9 string-fun)
        (ice-9 iconv)
        (srfi srfi-1)
        (lucidplan auth)
        (kracht prelude)
        (uuid generate)
        (lucidplan html)
        (lucidplan domain)
        (json)
        (artanis security nss)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain ticket-assignee)
        (lucidplan domain user)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite ticket-assignee)
        (lucidplan sqlite user)
        (hygguile hygguile))

(get "/tickets/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names lucidplan-show-done lucidplan-show-rejected
                       lucidplan-ticket-sort x-bewaking-lucidplan)
     (lambda (rc)
       (define (get)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (user-repo (make-sqlite-user-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                       rc))
                (is-logged-in (:cookies-value rc "x-bewaking-lucidplan"))
                (user-search (cond
                               ((equal? #f
                                        (get-from-qstr rc "search"))
                                #f)
                               (else (html-escape (get-from-qstr rc "search")))))
                (user-sort (cond
                             ((not (equal? #f
                                           (get-from-qstr rc "sort")))
                              (:cookies-set! rc
                                             'lucidplan-ticket-sort
                                             "lucidplan-ticket-sort"
                                             (get-from-qstr rc "sort"))
                              (:cookies-setattr! rc
                                                 'lucidplan-ticket-sort
                                                 #:expires 12000000
                                                 #:path "/"
                                                 #:secure #t
                                                 #:http-only #t)
                              (get-from-qstr rc "sort"))
                             ((not (equal? #f
                                           (:cookies-value rc
                                            "lucidplan-ticket-sort")))
                              (:cookies-value rc "lucidplan-ticket-sort"))
                             (else "last-edited-at-desc")))
                (with-done (cond
                             ((not (equal? #f
                                           (get-from-qstr rc "with-done")))
                              (:cookies-set! rc
                                             'lucidplan-show-done
                                             "lucidplan-show-done"
                                             (get-from-qstr rc "with-done"))
                              (:cookies-setattr! rc
                                                 'lucidplan-show-done
                                                 #:expires 12000000
                                                 #:path "/"
                                                 #:secure #t
                                                 #:http-only #t)
                              (equal? (get-from-qstr rc "with-done") "true"))
                             ((not (equal? #f
                                           (:cookies-value rc
                                            "lucidplan-show-done")))
                              (equal? (:cookies-value rc "lucidplan-show-done")
                                      "true"))
                             (else #f)))
                (with-rejected (cond
                                 ((not (equal? #f
                                               (get-from-qstr rc
                                                              "with-rejected")))
                                  (:cookies-set! rc
                                                 'lucidplan-show-rejected
                                                 "lucidplan-show-rejected"
                                                 (get-from-qstr rc
                                                  "with-rejected"))
                                  (:cookies-setattr! rc
                                                     'lucidplan-show-rejected
                                                     #:expires 12000000
                                                     #:path "/"
                                                     #:secure #t
                                                     #:http-only #t)
                                  (equal? (get-from-qstr rc "with-rejected")
                                          "true"))
                                 ((not (equal? #f
                                               (:cookies-value rc
                                                "lucidplan-show-rejected")))
                                  (equal? (:cookies-value rc
                                           "lucidplan-show-rejected") "true"))
                                 (else #f)))
                (proj (get-projects project-repo))
                (query-sort (entity-sort-from-query user-sort))
                (project-details (get-project-by-human-id project-repo
                                                          (params rc "project")))
                (my-sort (format #f "~a ~a"
                                 (car query-sort)
                                 (cadr query-sort)))
                (tickets (cond
                           ((eq? #f user-search)
                            (get-tickets ticket-repo
                                         (params rc "project") my-sort))
                           (else (search-tickets ticket-repo
                                                 (params rc "project")
                                                 (html-escape user-search)
                                                 my-sort))))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assq-ref t
                                                           'id) unquote
                                                (assq-ref t
                                                          'human-id))) tickets))
                (user-names (get-id-to-username user-repo))
                (ticket-assignee (get-ticket-assignee-by-project-human-id
                                  ticket-assignee-repo
                                  (params rc "project"))))
           
           (response-emit (:mime rc
                                 (html-from-tickets (:i18n rc)
                                                    #:is-logged-in
                                                    is-logged-in
                                                    #:current-sort user-sort
                                                    #:projects proj
                                                    #:project-details
                                                    project-details
                                                    #:ticket-assignee
                                                    ticket-assignee
                                                    #:user-names user-names
                                                    #:tickets tickets
                                                    #:ticket-id-to-human-id
                                                    ticket-id-to-human-id
                                                    #:with-done with-done
                                                    #:with-rejected
                                                    with-rejected
                                                    #:search-query user-search)))))
       (get)))

(get "/tickets/:project/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-new)
         (let* ((project-repo (make-sqlite-project-repository rc)))
           (response-emit (:mime rc
                                 (html-new-ticket-form (:i18n rc)
                                                       #:is-logged-in (:cookies-value
                                                                       rc
                                                                       "x-bewaking-lucidplan")
                                                       #:projects (get-projects
                                                                   project-repo)
                                                       #:project (params rc
                                                                  "project"))))))
       (get-new)))

(post "/tickets/:project/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id")))
            (DB-query (DB-open rc)
                      (->sql update
                             'ticket set
                             `((deleted_at ,(current-time)))
                             (where #:id id)))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))

(post "/tickets/:project/new"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-new)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (latest-ticket (get-ticket-of-latest-human-id ticket-repo
                                                               (params rc
                                                                "project")))
                 (latest-id (assq-ref latest-ticket
                                      'human-id))
                 (new-human-id (+ 1
                                  (if latest-id latest-id 0)))
                 (_ (write new-human-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (ticket-status (assoc-ref body "ticket-status"))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (description (html-escape (assoc-ref body "description")))
                 (priority (string->number (assoc-ref body "priority")))
                 (project-id (project-id-from-human-id project-repo
                                                       (params rc "project")))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" "")))
            (insert-ticket ticket-repo
                           `((id unquote new-uuid)
                             (human-id unquote new-human-id)
                             (project-id unquote project-id)
                             (title unquote title)
                             (ticket-status unquote ticket-status)
                             (ticket-type unquote ticket-type)
                             (priority unquote priority)
                             (description unquote description)
                             (created-at unquote
                                         (current-time))))

            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-new
                           #:unhappy-flow back-to-login)))

(get "/tickets/:project/id/:human-id/edit"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-edit)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (proj (get-projects project-repo))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                       rc))
                (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                (params rc
                                                                 "project")
                                                                (params rc
                                                                 "human-id")))
                (assignee-data (get-ticket-assignee-by-ticket-id
                                ticket-assignee-repo
                                (assq-ref t
                                          'id)))
                (assignee (if (nil? assignee-data) ""
                              (assq-ref assignee-data
                                        'user-id))))
           (response-emit (:mime rc
                                 (html-edit-ticket-form (:i18n rc)
                                                        #:projects proj
                                                        #:project (params rc
                                                                   "project")
                                                        #:t t
                                                        #:assignee assignee
                                                        #:is-logged-in (:cookies-value
                                                                        rc
                                                                        "x-bewaking-lucidplan"))))))
       (get-edit)))

(post "/tickets/:project/reorder"
      #:mime 'json
      #:from-post 'json
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-reorder)
          (let* ((proj (params rc "project"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (raw-b (bytevector->string (rc-body rc) "utf-8"))
                 (b (json-string->scm raw-b))
                 (_ (write b))
                 (upd-r (begin
                          (for-each (lambda (item)
                                      (let ((human-id (assoc-ref item "id"))
                                            (priority (assoc-ref item
                                                                 "priority")))
                                        (update-ticket-by-project-human-id-and-human-id
                                         ticket-repo proj human-id
                                         `((priority unquote priority)))))
                                    (vector->list (assoc-ref b "tickets"))) #t)))
            
            (if upd-r
                (:mime rc
                       '(("result" . "success")))
                (http-status 500
                             (lambda ()
                               (:mime rc
                                      '(("error" . "Failed to update priorities"))))))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-reorder
                           #:unhappy-flow back-to-login)))

(post "/tickets/:project/id/:human-id/status"
      #:mime 'json
      #:from-post 'json
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-re-status)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (raw-b (bytevector->string (rc-body rc) "utf-8"))
                 (b (json-string->scm raw-b))
                 (new-status (assoc-ref b "status")))
            
            (if (and new-status
                     (update-ticket-by-project-human-id-and-human-id
                      ticket-repo proj human-id
                      `((ticket-status unquote new-status))))
                (:mime rc
                       '(("result" . "success")))
                (http-status 500
                             (lambda ()
                               (:mime rc
                                      '(("error" . "Failed to update status"))))))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-re-status
                           #:unhappy-flow back-to-login)))

(post "/tickets/:project/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (ticket-status (assoc-ref body "ticket-status"))
                 (priority (string->number (assoc-ref body "priority")))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (project-id (project-id-from-human-id project-repo
                                                       (params rc "project")))
                 (ticket-id (ticket-id-from-human-id ticket-repo
                                                     (params rc "project")
                                                     (params rc "human-id")))

                 (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                        rc)))
            
            (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                            (params rc
                                                                    "project")
                                                            (params rc
                                                             "human-id")
                                                            `((ticket-status
                                                               unquote
                                                               ticket-status)
                                                              (ticket-type
                                                               unquote
                                                               ticket-type)
                                                              (description
                                                               unquote
                                                               description)
                                                              (title unquote
                                                                     title)
                                                              (priority
                                                               unquote
                                                               priority)
                                                              (updated_at
                                                               unquote
                                                               (current-time))))
            (upsert-ticket-assignee ticket-assignee-repo
                                    `((project-id unquote project-id)
                                      (ticket-id unquote ticket-id)
                                      (user-id unquote
                                               (assoc-ref body "assignee"))))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost)
                                              (params rc "project")))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))

(get "/tickets/:project/id/:human-id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-single)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (proj (get-projects project-repo))
                (user-repo (make-sqlite-user-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                       rc))
                (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                (params rc
                                                                 "project")
                                                                (params rc
                                                                 "human-id")))
                (user-names (get-id-to-username user-repo))
                (ticket-assignee (get-ticket-assignee-by-ticket-id
                                  ticket-assignee-repo
                                  (assq-ref t
                                            'id)))
                (ticket-assignee-user (find (lambda (u)
                                              (equal? (assq-ref u
                                                                'id)
                                                      (assq-ref
                                                       ticket-assignee
                                                       'user-id))) user-names)))
           (response-emit (:mime rc
                                 (html-template (:i18n rc)
                                                #:is-logged-in (:cookies-value
                                                                rc
                                                                "x-bewaking-lucidplan")
                                                #:body `(,(a (format #f
                                                              "← back to backlog: ~a"
                                                              (params rc
                                                               "project"))
                                                             #:size '(text-md
                                                                      px-2)
                                                             #:href (format #f
                                                                            "/tickets/~a"

                                                                            (params
                                                                             rc
                                                                             "project")))
                                                         (div (@ (class ,(mk-class '
                                                                          (p-2))))

                                                              ,(render-ticket (:i18n
                                                                               rc)
                                                                #:project (params
                                                                           rc
                                                                           "project")
                                                                #:ticket t
                                                                #:assignee (if
                                                                            (nil?
                                                                             ticket-assignee-user)
                                                                            #f
                                                                            (assq-ref
                                                                             ticket-assignee-user
                                                                             'username))))))))))
       (get-single)))
