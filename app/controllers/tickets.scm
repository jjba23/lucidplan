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
        (lucidplan auth)
        (kracht prelude)
        (uuid generate)
        (lucidplan html)
        (lucidplan domain)
        (app models ticket)
        (app models user)
        (app models ticket_assignee)
        (artanis security nss)
        (lucidplan persistence project)
        (lucidplan persistence ticket)
        (lucidplan persistence user)
        (hygguile hygguile))

(define (project-id-from-human-id rc human-id)
  (let* ((project-repo (make-sqlite-project-repository rc))
         (p (get-active-project-by-human-id project-repo human-id)))
    (assq-ref p
              'id)))

(define (ticket-id-from-human-id rc human-id)
  (let* ((ticket-repo (make-sqlite-ticket-repository rc))
         (t (get-active-ticket-by-human-id ticket-repo human-id)))
    (assq-ref t
              'id)))

(get "/tickets/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names lucidplan-show-done lucidplan-show-rejected
                       lucidplan-ticket-sort x-bewaking-lucidplan)
     (lambda (rc)
       (define (get)
         (let* ((is-logged-in (:cookies-value rc "x-bewaking-lucidplan"))
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
                (project-id (project-id-from-human-id rc
                                                      (params rc "project")))
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
                (project-repo (make-sqlite-project-repository rc))
                (user-repo (make-sqlite-user-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (proj (get-active-projects project-repo))
                (query-sort (entity-sort-from-query user-sort))
                (project-details (get-active-project-by-human-id project-repo
                                                                 (params rc
                                                                  "project")))
                (my-sort (format #f "~a ~a"
                                 (car query-sort)
                                 (cadr query-sort)))
                (tickets (cond
                           ((eq? #f user-search)
                            (get-active-tickets ticket-repo
                                                (assq-ref project-details
                                                          'id) my-sort))
                           (else (search-active-tickets ticket-repo
                                                        (params rc "project")
                                                        (html-escape
                                                         user-search) my-sort))))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assq-ref t
                                                           'id) unquote
                                                (assq-ref t
                                                          'human-id))) tickets))
                (user-names (get-id-to-username user-repo))
                (ticket-assignee (map (lambda (t)
                                        `(,(assq-ref t
                                                     'ticket-id) unquote
                                          (assq-ref t
                                                    'user-id)))
                                      ($ticket_assignee 'get
                                                        #:columns '(ticket_id
                                                                    user_id)))))
           
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
                                                       #:projects (get-active-projects
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
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (ticket-status (assoc-ref body "ticket-status"))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (description (html-escape (assoc-ref body "description")))
                 (priority (string->number (assoc-ref body "priority")))
                 (project-id (project-id-from-human-id rc
                                                       (params rc "project")))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" "")))
            
            ($ticket 'set
                     #:id new-uuid
                     #:project_id project-id
                     #:title title
                     #:ticket_status ticket-status
                     #:ticket_type ticket-type
                     #:priority priority
                     #:description description
                     #:created_at (current-time))

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
                (proj (get-active-projects project-repo))
                (project-details (get-active-project-by-human-id project-repo
                                                                 (params rc
                                                                  "project")))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (t (get-active-ticket-by-project-id-and-human-id ticket-repo
                                                                 (assq-ref
                                                                  project-details
                                                                  'id)
                                                                 (params rc
                                                                  "human-id")))
                (assignee-data ($ticket_assignee 'get
                                                 #:ret 'top
                                                 #:condition (where
                                                                    #:ticket_id
                                                                    (assq-ref
                                                                     t
                                                                     'id))))
                (assignee (if (nil? assignee-data) ""
                              (assoc-ref (car assignee-data) "user_id"))))
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

(post "/tickets/:project/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (ticket-status (assoc-ref body "ticket-status"))
                 (priority (string->number (assoc-ref body "priority")))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (ticket-id (ticket-id-from-human-id rc
                                                     (params rc "human-id"))))
            
            (DB-query (DB-open rc)
                      (->sql update
                             'ticket set
                             `((ticket_status ,ticket-status)
                               (ticket_type ,ticket-type)
                               (description ,description)
                               (title ,title)
                               (priority ,priority)
                               (updated_at ,(current-time)))
                             (where #:human_id (params rc "human-id"))))
            (DB-query (DB-open rc)
                      (format #f
                       "INSERT INTO ticket_assignee (user_id, ticket_id)
                        VALUES ('~a', '~a') ON CONFLICT(ticket_id) DO UPDATE SET user_id = excluded.user_id"
                       (assoc-ref body "assignee") ticket-id))

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
                (proj (get-active-projects project-repo))
                (user-repo (make-sqlite-user-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (project-details (get-active-project-by-human-id project-repo
                                                                 (params rc
                                                                  "project")))
                (t (get-active-ticket-by-project-id-and-human-id ticket-repo
                                                                 (assq-ref
                                                                  project-details
                                                                  'id)
                                                                 (params rc
                                                                  "human-id")))
                (user-names (get-id-to-username user-repo))
                (ticket-assignee ($ticket_assignee 'get
                                                   #:ret 'top
                                                   #:columns '(user_id)
                                                   #:condition (where
                                                                      #:ticket_id
                                                                      (assq-ref
                                                                       t
                                                                       'id)))))
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
                                                                            (not
                                                                             (nil?
                                                                              ticket-assignee))

                                                                            (assoc-ref
                                                                             user-names

                                                                             (assoc-ref
                                                                              (car
                                                                               ticket-assignee)
                                                                              "user_id"))
                                                                            #f)))))))))
       (get-single)))
