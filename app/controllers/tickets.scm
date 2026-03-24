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
                (project-id (project-id-from-human-id (params rc "project")))
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
                (proj ($project 'get
                                #:order-by '(human_id asc)))
                (query-sort (entity-sort-from-query user-sort))
                (project-details (car ($project 'get
                                                #:ret 'top
                                                #:condition (where #:human_id (params
                                                                               rc
                                                                               "project")))))
                (tickets (DB-get-all-rows (DB-query (DB-open rc)
                                                    (cond
                                                      ((equal? #f user-search)
                                                       (->sql select * from
                                                              'ticket
                                                              (format #f
                                                               " WHERE project_id = '~a' AND deleted_at IS NULL ORDER BY ~a ~a"
                                                               project-id
                                                               (car query-sort)
                                                               (cadr
                                                                query-sort))))
                                                      (else (->sql select *
                                                                   from
                                                                   'ticket
                                                                   (format #f
                                                                    (string-join '
                                                                     (" WHERE project_id = '~a' AND deleted_at IS NULL "
                                                                      " AND (title LIKE '%~a%' OR description LIKE '%~a%') "
                                                                      " ORDER BY ~a ~a ")
                                                                     " ")
                                                                    project-id
                                                                    (html-escape
                                                                     user-search)
                                                                    (html-escape
                                                                     user-search)
                                                                    (car
                                                                     query-sort)
                                                                    (cadr
                                                                     query-sort))))))))
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
                                                    #:is-logged-in
                                                    is-logged-in
                                                    #:current-sort user-sort
                                                    #:projects proj
                                                    #:project-details
                                                    project-details
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
         (response-emit (:mime rc
                               (html-new-ticket-form (:i18n rc)
                                                     #:is-logged-in (:cookies-value
                                                                     rc
                                                                     "x-bewaking-lucidplan")
                                                     #:projects ($project 'get
                                                                          #:order-by '
                                                                          (human_id
                                                                           asc))
                                                     #:project (params rc
                                                                "project")))))
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
                 (project-id (project-id-from-human-id (params rc "project")))
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
         (let* ((proj ($project 'get
                                #:order-by '(title asc)))
                (t (car ($ticket 'get
                                 #:ret 'top
                                 #:condition (where #:human_id (params rc
                                                                "human-id"))))))
           (response-emit (:mime rc
                                 (html-edit-ticket-form (:i18n rc)
                                                        #:projects proj
                                                        #:project (params rc
                                                                   "project")
                                                        #:t t
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
                 (ticket-type (assoc-ref body "ticket-type")))
            
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
         (let* ((proj ($project 'get
                                #:order-by '(human_id asc)))
                (t (car ($ticket 'get
                                 #:ret 'top
                                 #:condition (where #:human_id (params rc
                                                                "human-id"))))))
           (response-emit (:mime rc
                                 (html-template (:i18n rc)
                                                #:is-logged-in (:cookies-value
                                                                rc
                                                                "x-bewaking-lucidplan")
                                                #:body `(div (@ (class "p-2"))
                                                             ,(render-ticket (:i18n
                                                                              rc)
                                                               (params rc
                                                                "project") t)))))))
       (get-single)))
