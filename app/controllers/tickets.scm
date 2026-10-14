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
        (lucidplan notifications)
        (json)
        (artanis security nss)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain ticket-assignee)
        (lucidplan domain user)
        (lucidplan sqlite util)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite ticket-assignee)
        (lucidplan sqlite user)
        (hygguile hygguile))

;; A refused change is answered as JSON, with the status that says which kind
;; of refusal it was.  (`http-status' cannot do this: it *registers* the
;; handler a status is rendered with, so calling it here would make the
;; handler's own return value a procedure -- and the response would be a 500
;; about an invalid body.)
(define (refuse-json status message)
  (response-emit (scm->json-string `((error unquote message)))
                 #:status status
                 #:headers '((content-type application/json))))

;; A status a form submitted is honoured only when the project's own ladder
;; offers it: a status renamed away, or a value typed by hand, falls back to
;; the first open status, so a ticket is never left in a state its own board
;; cannot show.
(define (submitted-status statuses value)
  (if (and (string? value)
           (member value
                   (ticket-status-values statuses))) value
      (let ((open (ticket-status-values-in-bucket statuses "open")))
        (if (pair? open)
            (car open)
            (car (ticket-status-values statuses))))))

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
                ;; The statuses this project's tickets move through: its own
                ;; list if it kept one, the instance's otherwise.
                (statuses (effective-ticket-statuses project-details))
                (tickets (cond
                           ((eq? #f user-search)
                            (get-tickets ticket-repo
                                         (params rc "project") query-sort))
                           (else (search-tickets ticket-repo
                                                 (params rc "project")
                                                 (html-escape user-search)
                                                 query-sort))))
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
                                                    #:statuses statuses
                                                    #:search-query user-search
                                                    ;; Whether a ticket's row
                                                    ;; offers its inline triage
                                                    ;; controls is the same
                                                    ;; question the endpoint
                                                    ;; behind them answers.
                                                    #:can-triage? (session-has-permission?
                                                                   #:rc rc
                                                                   #:token-name 'x-bewaking-lucidplan
                                                                   #:permission 'lucidplan:ticket-create))))))
       (get)))

(get "/tickets/:project/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-new)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (user-repo (make-sqlite-user-repository rc))
                (project-details (get-project-by-human-id project-repo
                                                          (params rc "project"))))
           (response-emit (:mime rc
                                 (html-new-ticket-form (:i18n rc)
                                                       #:is-logged-in (:cookies-value
                                                                       rc
                                                                       "x-bewaking-lucidplan")
                                                       #:projects (get-projects
                                                                   project-repo)
                                                       #:project (params rc
                                                                  "project")
                                                       #:statuses (effective-ticket-statuses
                                                                   project-details)
                                                       #:users (get-id-to-username
                                                                user-repo))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:ticket-create)
                          #:rc rc
                          #:happy-flow get-new
                          #:unhappy-flow back-to-login)))

(post "/tickets/:project/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id"))
                 (execute-write (db-write (DB-open rc))))
            (execute-write "soft-delete-ticket"
                           (sql-update 'ticket
                                       `((deleted-at unquote
                                                     (current-time)))
                                       #:where (list (cons 'id id))))

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
                 (project (get-project-by-human-id project-repo
                                                   (params rc "project")))
                 (statuses (effective-ticket-statuses project))
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
                 (ticket-status (submitted-status statuses
                                                  (assoc-ref body
                                                             "ticket-status")))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (description (html-escape (assoc-ref body "description")))
                 (priority (string->number (assoc-ref body "priority")))
                 (project-id (project-id-from-human-id project-repo
                                                       (params rc "project")))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" ""))
                 (user-repo (make-sqlite-user-repository rc))
                 (user-ids (map (lambda (u)
                                  (assq-ref u
                                            'id))
                                (get-id-to-username user-repo)))
                 (submitted-assignee (assoc-ref body "assignee"))
                 (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                        rc)))
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

            ;; A new ticket may be assigned to one of the accounts that
            ;; exist, or to nobody -- which is simply the absence of an
            ;; assignment row, so there is nothing to write.
            (let ((assignee-id (known-user-id user-ids submitted-assignee)))
              (when assignee-id
                (upsert-ticket-assignee ticket-assignee-repo
                                        `((project-id unquote project-id)
                                          (ticket-id unquote new-uuid)
                                          (user-id unquote assignee-id))))

              ;; Everyone who takes part in the project hears that the board
              ;; moved.  The ticket is read back rather than described from
              ;; the submitted body, so the message says what was actually
              ;; written -- the number the schema gave it, the status the
              ;; project's own ladder settled on -- and the author is the
              ;; session the write came from.  A mail service that cannot be
              ;; reached is logged and answered with #f; it never unmakes the
              ;; ticket, and it never fails the request.
              (notify-new-ticket! #:user-repo user-repo
                                  #:assignee-repo ticket-assignee-repo
                                  #:project project
                                  #:statuses statuses
                                  #:ticket (get-ticket-by-project-human-id-and-human-id
                                            ticket-repo
                                            (params rc "project") new-human-id)
                                  #:author-id (session-owner-id #:rc rc
                                                                #:token-name 'x-bewaking-lucidplan)
                                  #:assignee-id assignee-id))

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
                (user-repo (make-sqlite-user-repository rc))
                (proj (get-projects project-repo))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                       rc))
                (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                (params rc
                                                                 "project")
                                                                (params rc
                                                                 "human-id")))
                (project-details (get-project-by-human-id project-repo
                                                          (params rc "project")))
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
                                                        #:statuses (effective-ticket-statuses
                                                                    project-details)
                                                        #:assignee assignee
                                                        #:users (get-id-to-username
                                                                 user-repo)
                                                        #:is-logged-in (:cookies-value
                                                                        rc
                                                                        "x-bewaking-lucidplan"))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:ticket-create)
                          #:rc rc
                          #:happy-flow get-edit
                          #:unhappy-flow back-to-login)))

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
                 (project-repo (make-sqlite-project-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 ;; A card can only be dropped into a column the board drew,
                 ;; so a status the project's own ladder does not name is a
                 ;; request the server refuses rather than one it writes.
                 (statuses (effective-ticket-statuses (get-project-by-human-id
                                                       project-repo proj)))
                 (raw-b (bytevector->string (rc-body rc) "utf-8"))
                 (b (json-string->scm raw-b))
                 (new-status (assoc-ref b "status")))
            
            (cond
              ((not (and (string? new-status)
                         (member new-status
                                 (ticket-status-values statuses))))
               (refuse-json 400 "Unknown status"))
              ((update-ticket-by-project-human-id-and-human-id ticket-repo
                                                               proj human-id
                                                               `((ticket-status
                                                                  unquote
                                                                  new-status)))
               (:mime rc
                      '(("result" . "success"))))
              (else (refuse-json 500 "Failed to update status")))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-re-status
                           #:unhappy-flow back-to-login)))

;; The status and the type of a ticket are changed from the ticket's own page
;; by the two small controls at the top of it, which post the field they name
;; as soon as it is changed.  Both changes are the same change -- one column
;; of one ticket -- so they are one endpoint, and the ladder the submitted
;; value has to be on is the same ladder the menu offered.
(post "/tickets/:project/id/:human-id/field"
      #:mime 'json
      #:from-post 'json
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-field-change)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (project-repo (make-sqlite-project-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (statuses (effective-ticket-statuses (get-project-by-human-id
                                                       project-repo proj)))
                 (raw-b (bytevector->string (rc-body rc) "utf-8"))
                 (b (json-string->scm raw-b))
                 (change (ticket-field-change (assoc-ref b "field")
                                              (assoc-ref b "value") statuses))
                 ;; Ask for the ticket before writing it: an unknown project or
                 ;; human id then answers a plain "no such ticket" rather than
                 ;; resting on what the driver reports for a write that
                 ;; matched no row.
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id)))
            (cond
              ((not change)
               (refuse-json 400 "Unknown field or value"))
              ((not t)
               (refuse-json 404 "No such ticket"))
              (else
               ;; A change is an edit, so the ticket's own "last edited" stamp
               ;; moves with it and the backlog can order by it.
               (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                               proj human-id
                                                               (append change
                                                                       `((updated_at
                                                                          unquote
                                                                          (current-time)))))
               (:mime rc
                      '(("result" . "success")))))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-field-change
                           #:unhappy-flow back-to-login)))

(post "/tickets/:project/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (statuses (effective-ticket-statuses (get-project-by-human-id
                                                       project-repo
                                                       (params rc "project"))))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (ticket-status (submitted-status statuses
                                                  (assoc-ref body
                                                             "ticket-status")))
                 (priority (string->number (assoc-ref body "priority")))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (project-id (project-id-from-human-id project-repo
                                                       (params rc "project")))
                 (ticket-id (ticket-id-from-human-id ticket-repo
                                                     (params rc "project")
                                                     (params rc "human-id")))
                 (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                        rc))
                 (user-repo (make-sqlite-user-repository rc))
                 (user-ids (map (lambda (u)
                                  (assq-ref u
                                            'id))
                                (get-id-to-username user-repo)))
                 (submitted-assignee (assoc-ref body "assignee")))
            
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
            ;; Who a ticket is assigned to is one of the accounts that exist,
            ;; or nobody.  Nobody is the row that the form offers to put the
            ;; ticket back to, so an empty value clears the assignment rather
            ;; than recording a user called ""; anything else that is not one
            ;; of the offered accounts leaves the ticket as it was.
            (cond
              ((and (string? submitted-assignee)
                    (string-null? submitted-assignee))
               (unassign-ticket-assignee ticket-assignee-repo ticket-id))
              ((known-user-id user-ids submitted-assignee)
               =>
               (lambda (assignee-id)
                 (upsert-ticket-assignee ticket-assignee-repo
                                         `((project-id unquote project-id)
                                           (ticket-id unquote ticket-id)
                                           (user-id unquote assignee-id)))))
              (else #f))
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
                (project-details (get-project-by-human-id project-repo
                                                          (params rc "project")))
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
                                                       'user-id))) user-names))
                ;; The card a change is confirmed with, inert in the page
                ;; until the script has something to say.
                (quiet-notice (render-quiet-notice (:i18n rc)))
                ;; The ticket itself.  Signed out, its status and its type
                ;; stay the badges they read as at a glance; signed in, each
                ;; of the two is the small control that posts its own change.
                (ticket-card (render-ticket (:i18n rc)
                                            #:project (params rc "project")
                                            #:ticket t
                                            #:statuses (effective-ticket-statuses
                                                        project-details)
                                            #:assignee (if (nil?
                                                            ticket-assignee-user)
                                                           #f
                                                           (assq-ref
                                                            ticket-assignee-user
                                                            'username))
                                            ;; The card offers its inline controls to the
                                            ;; same reader the endpoint behind them would
                                            ;; accept: holding the permission, not merely
                                            ;; being signed in.
                                            #:editable? (session-has-permission?
                                                         #:rc rc
                                                         #:token-name 'x-bewaking-lucidplan
                                                         #:permission 'lucidplan:ticket-create))))
           (response-emit (:mime rc
                                 (html-template (:i18n rc)
                                                #:title (assq-ref t
                                                                  'title)
                                                #:canonical-path (format #f
                                                                  "/tickets/~a/id/~a"
                                                                  (params rc
                                                                   "project")
                                                                  (params rc
                                                                   "human-id"))
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
                                                              ,ticket-card)
                                                         ,@quiet-notice))))))
       ;; An address naming a ticket that does not exist is a wrong turn, not
       ;; a page: the reader is sent back to the backlog they came from
       ;; rather than shown a ticket with nothing in it.
       (let ((ticket-repo (make-sqlite-ticket-repository rc)))
         (if (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                          (params rc "project")
                                                          (params rc
                                                                  "human-id"))
             (get-single)
             (redirect-to rc
                          (string->uri (format #f "~a/tickets/~a"
                                               (current-myhost)
                                               (params rc "project")))
                          #:status 302)))))
