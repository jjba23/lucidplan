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
        (ice-9 binary-ports)
        (srfi srfi-1)
        (lucidplan auth)
        (kracht prelude)
        (uuid generate)
        (lucidplan html)
        (lucidplan attachments)
        (lucidplan config)
        (lucidplan domain)
        (lucidplan notifications)
        (json)
        (artanis security nss)
        (artanis upload)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain ticket-assignee)
        (lucidplan domain ticket-attachment)
        (lucidplan domain ticket-comment)
        (lucidplan domain ticket-event)
        (lucidplan domain ticket-revision)
        (lucidplan domain user)
        (lucidplan domain sprint)
        (lucidplan domain milestone)
        (lucidplan domain time-entry)
        (lucidplan sqlite util)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite ticket-assignee)
        (lucidplan sqlite ticket-attachment)
        (lucidplan sqlite ticket-comment)
        (lucidplan sqlite ticket-event)
        (lucidplan sqlite ticket-revision)
        (lucidplan sqlite user)
        (lucidplan sqlite sprint)
        (lucidplan sqlite milestone)
        (lucidplan sqlite time-entry)
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

;; ===== What every write to a ticket leaves behind =====
;;
;; A change used to be just the UPDATE: the row said what the ticket is now,
;; and nothing said how it got there.  The helpers below are the other half.
;; Each write appends to the ticket's audit trail -- who, when, which field,
;; what it was, what it became -- and, when the ticket's own content changed,
;; writes the resulting content down as the next numbered version.  They are
;; called from the endpoints that write, after the write, with the ticket as
;; it was read back, so the history describes what the database actually
;; holds rather than what the request meant to say.

(define (new-id)
  (string-replace-substring (generate-string-uuid) "\n" ""))

;; Add COLUMN to ATTRS unless VALUE is nothing a row should carry.  The
;; builders bind #f as 0, so an absent optional value is left out of the
;; statement entirely and the column keeps its NULL instead.  An attribute is
;; a dotted pair, the shape the `(column unquote value)' templates in the
;; repositories expand to; a three-element list is not one, and the builder
;; would bind the whole `(unquote value)' as the value.
(define (value-attr attrs column value)
  (if (or (not value)
          (and (string? value)
               (string-null? value))) attrs
      (append attrs
              (list (cons column value)))))

(define* (record-event! event-repo
                        #:key ticket-id
                        project-id
                        actor-id
                        action
                        (field #f)
                        (old-value #f)
                        (new-value #f))
  (insert-ticket-event event-repo
                       (value-attr (value-attr (value-attr (append `((id
                                                                      unquote
                                                                      (new-id))
                                                                     (ticket-id
                                                                      unquote
                                                                      ticket-id)
                                                                     (project-id
                                                                      unquote
                                                                      project-id)
                                                                     (action
                                                                      unquote
                                                                      action)
                                                                     (created-at
                                                                      unquote
                                                                      (current-time)))
                                                                   (if
                                                                    actor-id
                                                                    `((actor-id
                                                                       unquote
                                                                       actor-id))
                                                                    '()))
                                                           'field field)
                                               'old-value old-value)
                                   'new-value new-value)))

(define* (record-revision! revision-repo ticket
                           #:key (actor-id #f))
  (let* ((ticket-id (assq-ref ticket
                              'id))
         (attrs (append `((id unquote
                              (new-id))
                          (ticket-id unquote ticket-id)
                          (project-id unquote
                                      (assq-ref ticket
                                                'project-id))
                          (version unquote
                                   (next-ticket-version revision-repo
                                                        ticket-id))
                          (title unquote
                                 (assq-ref ticket
                                           'title))
                          (created-at unquote
                                      (current-time)))
                        (if actor-id
                            `((author-id unquote actor-id))
                            '()))))
    (insert-ticket-revision revision-repo
                            (value-attr (value-attr (value-attr (value-attr (value-attr
                                                                             (value-attr
                                                                              attrs
                                                                              'description
                                                                              
                                                                              (assq-ref
                                                                               ticket
                                                                               'description))
                                                                             'ticket-status
                                                                             (assq-ref
                                                                              ticket
                                                                              'ticket-status))
                                                                            'ticket-type
                                                                            (assq-ref
                                                                             ticket
                                                                             'ticket-type))
                                                                'priority
                                                                (assq-ref
                                                                 ticket
                                                                 'priority))
                                                    'estimate-points
                                                    (assq-ref ticket
                                                              'estimate-points))
                                        'estimate-size
                                        (assq-ref ticket
                                                  'estimate-size)))))

(define* (record-tracked-changes! event-repo
                                  changes
                                  #:key ticket-id
                                  project-id
                                  actor-id)
  (for-each (lambda (change)
              (record-event! event-repo
                             #:ticket-id ticket-id
                             #:project-id project-id
                             #:actor-id actor-id
                             #:action "changed"
                             #:field (cadr change)
                             #:old-value (caddr change)
                             #:new-value (cadddr change))) changes))

(define (current-version revision-repo ticket-id)
  (let ((revisions (get-ticket-revisions revision-repo ticket-id)))
    (if (null? revisions) 0
        (or (assq-ref (car revisions)
                      'version) 0))))

;; The map from a tracked column to the name its change is recorded under.
(define (tracked-field-label column)
  (let ((field (find (lambda (f)
                       (eq? (car f) column)) ticket-tracked-fields)))
    (if field
        (cadr field)
        (format #f "~a" column))))

(define (username-of user-names user-id)
  "Answer the username USER-ID names in USER-NAMES, or the empty string.

An account may have been removed since the row naming it was written, and a
history entry that says somebody is better than one that fails to render."
  (let ((row (and user-id
                  (find (lambda (u)
                          (equal? (assq-ref u
                                            'id) user-id)) user-names))))
    (if row
        (or (assq-ref row
                      'username) "") "")))

;; A ticket's estimates are written by their own statement (see the ticket
;; repository), so every writer needs the other estimate the ticket holds:
;; changing one estimate must not clear the other.
(define (update-estimates! ticket-repo ticket points size)
  (update-ticket-estimates-by-id ticket-repo
                                 (assq-ref ticket
                                           'id)
                                 (if (eq? points
                                          'keep)
                                     (assq-ref ticket
                                               'estimate-points) points)
                                 (if (eq? size
                                          'keep)
                                     (assq-ref ticket
                                               'estimate-size) size)))

;; The values a ticket is being given, as the tracker compares them.  A
;; column whose value is not being changed carries what the ticket holds, so
;; a form that leaves a field alone is not recorded as a change to it.
(define* (submitted-values old
                           #:key (title 'keep)
                           (description 'keep)
                           (ticket-status 'keep)
                           (ticket-type 'keep)
                           (priority 'keep)
                           (points 'keep)
                           (size 'keep))
  (let ((kept (lambda (sent column)
                (if (eq? sent
                         'keep)
                    (and old
                         (assq-ref old column)) sent))))
    `((title unquote
             (kept title
                   'title))
      (description unquote
                   (kept description
                         'description))
      (ticket-status unquote
                     (kept ticket-status
                           'ticket-status))
      (ticket-type unquote
                   (kept ticket-type
                         'ticket-type))
      (priority unquote
                (kept priority
                      'priority))
      (estimate-points unquote
                       (kept points
                             'estimate-points))
      (estimate-size unquote
                     (kept size
                           'estimate-size)))))

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
                 (project-repo (make-sqlite-project-repository rc))
                 (execute-write (db-write (DB-open rc))))
            (execute-write "soft-delete-ticket"
                           (sql-update 'ticket
                                       `((deleted-at unquote
                                                     (current-time)))
                                       #:where (list (cons 'id id))))
            ;; The ticket is gone from the pages, but the trail of what
            ;; happened to it is not: deleting is one of the things that can
            ;; happen to a ticket, and it is recorded like the rest.
            (record-event! (make-sqlite-ticket-event-repository rc)
                           #:ticket-id id
                           #:project-id (project-id-from-human-id project-repo
                                         (params rc "project"))
                           #:actor-id (session-owner-id #:rc rc
                                                        #:token-name 'x-bewaking-lucidplan)
                           #:action "deleted")

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
                 (estimate-points (parse-estimate-points (assoc-ref body
                                                          "estimate-points")))
                 (estimate-size (parse-estimate-size (assoc-ref body
                                                      "estimate-size")))
                 (actor-id (session-owner-id #:rc rc
                                             #:token-name 'x-bewaking-lucidplan))
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
                           (append `((id unquote new-uuid)
                                     (human-id unquote new-human-id)
                                     (project-id unquote project-id)
                                     (title unquote title)
                                     (ticket-status unquote ticket-status)
                                     (ticket-type unquote ticket-type)
                                     (priority unquote priority)
                                     (description unquote description)
                                     (created-at unquote
                                                 (current-time)))
                                   ;; An unestimated ticket keeps the NULL the
                                   ;; schema gave the column, so the columns
                                   ;; are only named when there is something
                                   ;; to put in them.
                                   (if estimate-points
                                       `((estimate-points unquote
                                                          estimate-points))
                                       '())
                                   (if estimate-size
                                       `((estimate-size unquote estimate-size))
                                       '())))

            ;; A new ticket may be assigned to one of the accounts that
            ;; exist, or to nobody -- which is simply the absence of an
            ;; assignment row, so there is nothing to write.
            (let* ((assignee-id (known-user-id user-ids submitted-assignee))
                   (created (get-ticket-by-project-human-id-and-human-id
                             ticket-repo
                             (params rc "project") new-human-id))
                   (event-repo (make-sqlite-ticket-event-repository rc))
                   (revision-repo (make-sqlite-ticket-revision-repository rc)))
              (when assignee-id
                (upsert-ticket-assignee ticket-assignee-repo
                                        `((project-id unquote project-id)
                                          (ticket-id unquote new-uuid)
                                          (user-id unquote assignee-id))))

              ;; The ticket begins its own history: the first thing the
              ;; trail says is that it exists, and the first version is the
              ;; content it was created with.  Both are written from the row
              ;; read back, so they describe the ticket the database holds --
              ;; including the status the project's ladder settled on.
              (record-event! event-repo
                             #:ticket-id new-uuid
                             #:project-id project-id
                             #:actor-id actor-id
                             #:action "created")
              (record-revision! revision-repo created
                                #:actor-id actor-id)

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
                                  #:ticket created
                                  #:author-id actor-id
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
                 (project-repo (make-sqlite-project-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (event-repo (make-sqlite-ticket-event-repository rc))
                 (project-id (project-id-from-human-id project-repo proj))
                 (actor-id (session-owner-id #:rc rc
                                             #:token-name 'x-bewaking-lucidplan))
                 (raw-b (bytevector->string (rc-body rc) "utf-8"))
                 (b (json-string->scm raw-b))
                 (upd-r (begin
                          (for-each (lambda (item)
                                      (let* ((human-id (assoc-ref item "id"))
                                             (priority (assoc-ref item
                                                                  "priority"))
                                             (old (get-ticket-by-project-human-id-and-human-id
                                                   ticket-repo proj human-id)))
                                        (when old
                                          (update-ticket-by-project-human-id-and-human-id
                                           ticket-repo proj human-id
                                           `((priority unquote priority)))
                                          ;; A drag writes the whole list's
                                          ;; priorities; only the rows whose
                                          ;; number actually moved are
                                          ;; recorded, so one drop is a
                                          ;; handful of entries rather than
                                          ;; a page of them.
                                          (unless (equal? (tracked-value (assq-ref
                                                                          old
                                                                          'priority))
                                                          (tracked-value
                                                           priority))
                                            (record-event! event-repo
                                                           #:ticket-id (assq-ref
                                                                        old
                                                                        'id)
                                                           #:project-id
                                                           project-id
                                                           #:actor-id actor-id
                                                           #:action "changed"
                                                           #:field "priority"
                                                           #:old-value (tracked-value
                                                                        (assq-ref
                                                                         old
                                                                         'priority))
                                                           #:new-value (tracked-value
                                                                        priority))))))
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
                 (new-status (assoc-ref b "status"))
                 (old-ticket (get-ticket-by-project-human-id-and-human-id
                              ticket-repo proj human-id)))
            
            (cond
              ((not (and (string? new-status)
                         (member new-status
                                 (ticket-status-values statuses))))
               (refuse-json 400 "Unknown status"))
              ((not old-ticket)
               (refuse-json 404 "No such ticket"))
              ((update-ticket-by-project-human-id-and-human-id ticket-repo
                                                               proj human-id
                                                               `((ticket-status
                                                                  unquote
                                                                  new-status)
                                                                 (updated_at
                                                                  unquote
                                                                  (current-time))))
               (let* ((project-id (project-id-from-human-id project-repo proj))
                      (actor-id (session-owner-id #:rc rc
                                                  #:token-name 'x-bewaking-lucidplan))
                      (updated (get-ticket-by-project-human-id-and-human-id
                                ticket-repo proj human-id))
                      (values (submitted-values old-ticket
                                                #:ticket-status new-status))
                      (changes (ticket-changes old-ticket values)))
                 (record-tracked-changes! (make-sqlite-ticket-event-repository
                                           rc)
                                          changes
                                          #:ticket-id (assq-ref old-ticket
                                                                'id)
                                          #:project-id project-id
                                          #:actor-id actor-id)
                 (when (pair? changes)
                   (record-revision! (make-sqlite-ticket-revision-repository
                                      rc) updated
                                     #:actor-id actor-id))
                 (:mime rc
                        '(("result" . "success")))))
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
                 ;; A change is a one-entry association list -- the column and
                 ;; its new value -- not the pair alone, because that is the
                 ;; shape the repository update takes.
                 (column (and change
                              (caar change)))
                 (submitted (and change
                                 (cdar change)))
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
              (else (let* ((project-id (project-id-from-human-id project-repo
                                                                 proj))
                           (actor-id (session-owner-id #:rc rc
                                                       #:token-name 'x-bewaking-lucidplan))
                           (event-repo (make-sqlite-ticket-event-repository rc))
                           (revision-repo (make-sqlite-ticket-revision-repository
                                           rc))
                           (points (if (eq? column
                                            'estimate-points)
                                       (parse-estimate-points submitted)
                                       'keep))
                           (size (if (eq? column
                                          'estimate-size)
                                     (parse-estimate-size submitted)
                                     'keep)))
                      ;; A change is an edit, so the ticket's own "last edited"
                      ;; stamp moves with it and the backlog can order by it.
                      (if (or (eq? column
                                   'estimate-points)
                              (eq? column
                                   'estimate-size))
                          (update-ticket-estimates-by-id ticket-repo
                                                         (assq-ref t
                                                                   'id)
                                                         (if (eq? points
                                                                  'keep)
                                                             (assq-ref t
                                                                       'estimate-points)
                                                             points)
                                                         (if (eq? size
                                                                  'keep)
                                                             (assq-ref t
                                                                       'estimate-size)
                                                             size))
                          (update-ticket-by-project-human-id-and-human-id
                           ticket-repo proj human-id
                           (append change
                                   `((updated_at unquote
                                                 (current-time))))))
                      (let* ((updated (get-ticket-by-project-human-id-and-human-id
                                       ticket-repo proj human-id))
                             (values (submitted-values t
                                                       #:ticket-status (if (eq?
                                                                            column
                                                                            'ticket-status)
                                                                        submitted
                                                                        'keep)
                                                       #:ticket-type (if (eq?
                                                                          column
                                                                          'ticket-type)
                                                                      submitted
                                                                      'keep)
                                                       #:points points
                                                       #:size size))
                             (changes (ticket-changes t values)))
                        (record-tracked-changes! event-repo
                                                 changes
                                                 #:ticket-id (assq-ref t
                                                                       'id)
                                                 #:project-id project-id
                                                 #:actor-id actor-id)
                        (when (pair? changes)
                          (record-revision! revision-repo updated
                                            #:actor-id actor-id)))
                      (:mime rc
                             '(("result" . "success"))))))))
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
                 (proj-human (params rc "project"))
                 (human-id (params rc "human-id"))
                 (statuses (effective-ticket-statuses (get-project-by-human-id
                                                       project-repo proj-human)))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (ticket-status (submitted-status statuses
                                                  (assoc-ref body
                                                             "ticket-status")))
                 (priority (string->number (assoc-ref body "priority")))
                 (ticket-type (assoc-ref body "ticket-type"))
                 (estimate-points (parse-estimate-points (assoc-ref body
                                                          "estimate-points")))
                 (estimate-size (parse-estimate-size (assoc-ref body
                                                      "estimate-size")))
                 (project-id (project-id-from-human-id project-repo proj-human))
                 (old-ticket (get-ticket-by-project-human-id-and-human-id
                              ticket-repo proj-human human-id))
                 (ticket-id (assq-ref old-ticket
                                      'id))
                 (ticket-assignee-repo (make-sqlite-ticket-assignee-repository
                                        rc))
                 (old-assignee (and old-ticket
                                    (get-ticket-assignee-by-ticket-id
                                     ticket-assignee-repo ticket-id)))
                 (user-repo (make-sqlite-user-repository rc))
                 (user-names (get-id-to-username user-repo))
                 (user-ids (map (lambda (u)
                                  (assq-ref u
                                            'id)) user-names))
                 (submitted-assignee (assoc-ref body "assignee"))
                 (actor-id (session-owner-id #:rc rc
                                             #:token-name 'x-bewaking-lucidplan))
                 (event-repo (make-sqlite-ticket-event-repository rc))
                 (revision-repo (make-sqlite-ticket-revision-repository rc))
                 (values (submitted-values old-ticket
                                           #:title title
                                           #:description description
                                           #:ticket-status ticket-status
                                           #:ticket-type ticket-type
                                           #:priority priority
                                           #:points estimate-points
                                           #:size estimate-size))
                 (changes (ticket-changes old-ticket values)))
            (when old-ticket
              (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                              proj-human
                                                              human-id
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
              (update-ticket-estimates-by-id ticket-repo ticket-id
                                             estimate-points estimate-size)

              ;; Who a ticket is assigned to is one of the accounts that
              ;; exist, or nobody.  Nobody is the row that the form offers to
              ;; put the ticket back to, so an empty value clears the
              ;; assignment rather than recording a user called ""; anything
              ;; else that is not one of the offered accounts leaves the
              ;; ticket as it was.
              (let* ((old-assignee-id (and old-assignee
                                           (assq-ref old-assignee
                                                     'user-id)))
                     (old-name (username-of user-names old-assignee-id))
                     (new-assignee-id (known-user-id user-ids
                                                     submitted-assignee))
                     (clear? (and (string? submitted-assignee)
                                  (string-null? submitted-assignee))))
                (cond
                  (clear? (unassign-ticket-assignee ticket-assignee-repo
                                                    ticket-id)
                          (unless (equal? old-assignee-id #f)
                            (record-event! event-repo
                                           #:ticket-id ticket-id
                                           #:project-id project-id
                                           #:actor-id actor-id
                                           #:action "changed"
                                           #:field "assignee"
                                           #:old-value old-name
                                           #:new-value "")))
                  (new-assignee-id (upsert-ticket-assignee
                                    ticket-assignee-repo
                                    `((project-id unquote project-id)
                                      (ticket-id unquote ticket-id)
                                      (user-id unquote new-assignee-id)))
                                   (unless (equal? new-assignee-id
                                                   old-assignee-id)
                                     (record-event! event-repo
                                                    #:ticket-id ticket-id
                                                    #:project-id project-id
                                                    #:actor-id actor-id
                                                    #:action "changed"
                                                    #:field "assignee"
                                                    #:old-value old-name
                                                    #:new-value (username-of
                                                                 user-names
                                                                 new-assignee-id))))
                  (else #f)))

              (record-tracked-changes! event-repo
                                       changes
                                       #:ticket-id ticket-id
                                       #:project-id project-id
                                       #:actor-id actor-id)
              ;; A version is written only when the ticket's content
              ;; actually moved: opening the edit page and submitting it
              ;; unchanged is not a new version of anything.
              (when (pair? changes)
                (record-revision! revision-repo
                                  (get-ticket-by-project-human-id-and-human-id
                                   ticket-repo proj-human human-id)
                                  #:actor-id actor-id)))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a"
                                              (current-myhost) proj-human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))

;; ===== The discussion on a ticket =====
;;
;; A comment names the comment it answers, and the page draws the replies
;; under what they answer.  The parent is checked against the comments the
;; ticket actually has before it is written: a reply to a comment of another
;; ticket, or to an id that never existed, becomes a comment on the ticket
;; rather than a node the page can never reach.
(post "/tickets/:project/id/:human-id/comments"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-comment)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (comment-repo (make-sqlite-ticket-comment-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (text (assoc-ref body "body"))
                 (requested-parent (assoc-ref body "parent-id")))
            (when (and t
                       (string? text)
                       (not (string-null? (string-trim-both text))))
              (let* ((project-id (project-id-from-human-id project-repo proj))
                     (ticket-id (assq-ref t
                                          'id))
                     (existing (get-ticket-comments comment-repo ticket-id))
                     (parent (and requested-parent
                                  (not (string-null? requested-parent))
                                  (find (lambda (c)
                                          (equal? (format #f "~a"
                                                          (assq-ref c
                                                                    'id))
                                                  requested-parent)) existing)))
                     (comment-id (new-id))
                     (attrs `((id unquote comment-id)
                              (ticket-id unquote ticket-id)
                              (project-id unquote project-id)
                              (author-id unquote
                                         (or (session-owner-id #:rc rc
                                                               #:token-name 'x-bewaking-lucidplan)
                                             ""))
                              (body unquote
                                    (html-escape text))
                              (created-at unquote
                                          (current-time)))))
                (insert-ticket-comment comment-repo
                                       (if parent
                                           (append attrs
                                                   `((parent-id unquote
                                                                (assq-ref
                                                                 parent
                                                                 'id)))) attrs))
                (record-event! (make-sqlite-ticket-event-repository rc)
                               #:ticket-id ticket-id
                               #:project-id project-id
                               #:actor-id (session-owner-id #:rc rc
                                                            #:token-name 'x-bewaking-lucidplan)
                               #:action "commented")))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-comment
                           #:unhappy-flow back-to-login)))

;; Removing a comment is a soft delete, so the replies that answer it keep
;; their place in the thread: the row stays as the node they hang from and
;; the page draws its body as removed.  The comment is looked for among the
;; comments of the ticket in the URL, never by its id alone, so a comment of
;; another ticket cannot be removed by naming it here.
;;
;; A comment belongs to whoever wrote it, and that is who may remove it.
;; Anyone holding `lucidplan:ticket-delete' may remove another person's too:
;; a thread still has to be moderatable, and the trail records who did it.
(post "/tickets/:project/id/:human-id/comments/:comment-id/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-remove-comment)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (comment-id (params rc "comment-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (comment-repo (make-sqlite-ticket-comment-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (actor-id (session-owner-id #:rc rc
                                             #:token-name 'x-bewaking-lucidplan))
                 (moderator? (session-has-permission? #:rc rc
                                                      #:token-name 'x-bewaking-lucidplan
                                                      #:permission 'lucidplan:ticket-delete))
                 (comment (and t
                               (find (lambda (c)
                                       (equal? (format #f "~a"
                                                       (assq-ref c
                                                                 'id))
                                               comment-id))
                                     (get-ticket-comments comment-repo
                                                          (assq-ref t
                                                                    'id))))))
            (when (and t
                       comment
                       ;; A comment that was removed already is not removed
                       ;; twice; the trail should say it happened once.
                       (not (assq-ref comment
                                      'deleted-at))
                       (or moderator?
                           (and actor-id
                                (equal? (format #f "~a" actor-id)
                                        (format #f "~a"
                                                (assq-ref comment
                                                          'author-id))))))
              (soft-delete-ticket-comment comment-repo
                                          (assq-ref comment
                                                    'id))
              (record-event! (make-sqlite-ticket-event-repository rc)
                             #:ticket-id (assq-ref t
                                                   'id)
                             #:project-id (project-id-from-human-id
                                           project-repo proj)
                             #:actor-id actor-id
                             #:action "comment-removed"
                             #:field "comment"
                             #:old-value (assq-ref comment
                                                   'body)))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-remove-comment
                           #:unhappy-flow back-to-login)))

;; ===== The files on a ticket =====
;;
;; The bytes are stored under a name generated here; what the uploader
;; called the file is kept in the row and used for the download's name.  The
;; form is multipart, so it is read by Artanis's upload reader rather than by
;; the url-encoded body reader the other forms use.
(post "/tickets/:project/id/:human-id/attachments"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-upload)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (attachment-repo (make-sqlite-ticket-attachment-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (mo (get-mfds-op-from-post rc
                                            #:path (attachment-directory)))
                 (file (find (lambda (m)
                               (let ((name (mfd-filename m)))
                                 (and name
                                      (not (string-null? name)))))
                             (mfds-operator-mfds mo)))
                 (size (and file
                            (mfd-size file)))
                 (max-bytes (config-ref 'attachment-max-bytes)))
            (when t
              (ensure-attachment-directory)
              (cond
                ((not file)
                 (redirect-to rc
                              (string->uri (format #f
                                            "~a/tickets/~a/id/~a?attachment-error=no-file"
                                            (current-myhost) proj human-id))
                              #:status 302))
                ((or (not (number? size))
                     (<= size 0)
                     (> size max-bytes))
                 (redirect-to rc
                              (string->uri (format #f
                                            "~a/tickets/~a/id/~a?attachment-error=size"
                                            (current-myhost) proj human-id))
                              #:status 302))
                (else (let* ((stored-name (new-id))
                             (project-id (project-id-from-human-id
                                          project-repo proj))
                             (ticket-id (assq-ref t
                                                  'id))
                             (filename (safe-attachment-filename (mfd-filename
                                                                  file))))
                        (mfds-op-store mo
                                       #:renamer (lambda (old)
                                                   stored-name))
                        (insert-ticket-attachment attachment-repo
                                                  `((id unquote
                                                        (new-id))
                                                    (ticket-id unquote
                                                               ticket-id)
                                                    (project-id unquote
                                                                project-id)
                                                    (uploader-id unquote
                                                                 (or (session-owner-id
                                                                      #:rc rc
                                                                      #:token-name 'x-bewaking-lucidplan)
                                                                     ""))
                                                    (filename unquote filename)
                                                    (stored-name unquote
                                                                 stored-name)
                                                    (content-type unquote
                                                                  (safe-content-type
                                                                   (mfd-type
                                                                    file)))
                                                    (size unquote size)
                                                    (created-at unquote
                                                                (current-time))))
                        (record-event! (make-sqlite-ticket-event-repository rc)
                                       #:ticket-id ticket-id
                                       #:project-id project-id
                                       #:actor-id (session-owner-id #:rc rc
                                                                    #:token-name 'x-bewaking-lucidplan)
                                       #:action "attachment-added"
                                       #:field "attachment"
                                       #:new-value filename)
                        (redirect-to rc
                                     (string->uri (format #f
                                                   "~a/tickets/~a/id/~a"
                                                   (current-myhost) proj
                                                   human-id))
                                     #:status 302)))))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-upload
                           #:unhappy-flow back-to-login)))

(post "/tickets/:project/id/:human-id/attachments/:attachment-id/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-remove-attachment)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (attachment-id (params rc "attachment-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (attachment-repo (make-sqlite-ticket-attachment-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (attachment (and t
                                  (get-ticket-attachment-by-id attachment-repo
                                   attachment-id))))
            (when (and t attachment
                       (equal? (format #f "~a"
                                       (assq-ref attachment
                                                 'ticket-id))
                               (format #f "~a"
                                       (assq-ref t
                                                 'id))))
              (soft-delete-ticket-attachment attachment-repo attachment-id)
              (record-event! (make-sqlite-ticket-event-repository rc)
                             #:ticket-id (assq-ref t
                                                   'id)
                             #:project-id (project-id-from-human-id
                                           project-repo proj)
                             #:actor-id (session-owner-id #:rc rc
                                                          #:token-name 'x-bewaking-lucidplan)
                             #:action "attachment-removed"
                             #:field "attachment"
                             #:old-value (assq-ref attachment
                                                   'filename)))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-remove-attachment
                           #:unhappy-flow back-to-login)))

;; A download is answered by the application, not by the static file server:
;; the attachment id has to name a file of the ticket in the URL, so a file
;; cannot be fetched by guessing a name in the upload directory.
(get "/tickets/:project/id/:human-id/attachments/:attachment-id"
     #:mime 'xml
     (lambda (rc)
       (let* ((ticket-repo (make-sqlite-ticket-repository rc))
              (attachment-repo (make-sqlite-ticket-attachment-repository rc))
              (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                              (params rc
                                                               "project")
                                                              (params rc
                                                               "human-id")))
              (attachment (and t
                               (get-ticket-attachment-by-id attachment-repo
                                                            (params rc
                                                             "attachment-id"))))
              (path (and attachment
                         (attachment-path (assq-ref attachment
                                                    'stored-name)))))
         (if (and t attachment path
                  (file-exists? path)
                  (equal? (format #f "~a"
                                  (assq-ref attachment
                                            'ticket-id))
                          (format #f "~a"
                                  (assq-ref t
                                            'id))))
             (let* ((bytes (call-with-input-file path
                             get-bytevector-all))
                    (type (safe-content-type (assq-ref attachment
                                                       'content-type))))
               ;; Artanis hands these headers to Guile's own response writer,
               ;; which wants a media type in the shape `(text/plain)' -- the
               ;; type as a symbol, with any parameters beside it -- not the
               ;; text `"text/plain"'.  A string there is refused with
               ;; "Bad value for header content-type", so the sanitized type
               ;; is read back as the symbol it names; `safe-content-type' has
               ;; already refused anything that is not one.
               (response-emit bytes
                              #:headers `((content-type ,(string->symbol type))
                                          (content-disposition unquote
                                                               (attachment-disposition
                                                                (assq-ref
                                                                 attachment
                                                                 'filename))))))
             (response-emit ""
                            #:status 404)))))

;; ===== The hours spent on a ticket =====
;;
;; A time entry says three things -- how long, which day, and what was done --
;; and all three are checked before anything is written.  The duration is read
;; by `parse-duration', which is where every way a person writes an hour and a
;; half (`1.5h', `90m', `1h30m', `1h 30m', `1:30') becomes the same ninety
;; minutes; a form that says nothing a duration can be read from writes
;; nothing at all rather than an entry of zero.
;;
;; The day defaults to today, because "I worked on this for two hours" is said
;; about today far more often than about any other day, and a form that insists
;; on a date before it will take an hour is a form people stop using.
;;
;; The comment is stored as it was typed and escaped when the page draws it,
;; which is where markup is escaped everywhere else: what is written down is
;; what somebody said, and what a reader is shown is that text as text.
(post "/tickets/:project/id/:human-id/time"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-log-time)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (time-repo (make-sqlite-time-entry-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (minutes (parse-duration (assoc-ref body "duration")))
                 ;; A day that is not a day is not a reason to refuse the
                 ;; entry: the hours were still spent, so the day falls back
                 ;; to today and the log stays complete.
                 (spent-on (or (parse-iso-date (assoc-ref body "spent-on"))
                               (today-iso)))
                 (comment (assoc-ref body "comment")))
            (when (and t minutes)
              (let* ((project-id (project-id-from-human-id project-repo proj))
                     (ticket-id (assq-ref t
                                          'id))
                     (actor-id (session-owner-id #:rc rc
                                                 #:token-name 'x-bewaking-lucidplan)))
                (insert-time-entry time-repo
                                   (value-attr (append `((id unquote
                                                             (new-id))
                                                         (ticket-id unquote
                                                                    ticket-id)
                                                         (project-id unquote
                                                                     project-id)
                                                         (duration-minutes
                                                          unquote minutes)
                                                         (spent-on unquote
                                                                   spent-on)
                                                         (created-at unquote
                                                                     (current-time)))
                                                       (if actor-id
                                                           `((author-id
                                                              unquote
                                                              actor-id)) '()))
                                               'comment (and (string? comment)
                                                             (string-trim-both
                                                              comment))))
                (record-event! (make-sqlite-ticket-event-repository rc)
                               #:ticket-id ticket-id
                               #:project-id project-id
                               #:actor-id actor-id
                               #:action "time-logged"
                               #:field "time"
                               #:new-value (format-duration minutes))))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-log-time
                           #:unhappy-flow back-to-login)))

;; Taking an entry back is a soft delete, so the trail still says the hours
;; were logged and then taken back, and the ticket's total stops counting
;; them.  The entry is looked for among the entries of the ticket in the URL,
;; never by its id alone: an entry of another ticket cannot be removed by
;; naming it here.
;;
;; Time belongs to whoever logged it, and that is who may take it back.
;; Anyone holding `lucidplan:ticket-delete' may remove another person's too --
;; a log that could only be corrected by its author would be wrong forever
;; once that person left -- and the trail records who did it.
(post "/tickets/:project/id/:human-id/time/:entry-id/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-remove-time)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (entry-id (params rc "entry-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (time-repo (make-sqlite-time-entry-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (actor-id (session-owner-id #:rc rc
                                             #:token-name 'x-bewaking-lucidplan))
                 (moderator? (session-has-permission? #:rc rc
                                                      #:token-name 'x-bewaking-lucidplan
                                                      #:permission 'lucidplan:ticket-delete))
                 (entry (and t
                             (find (lambda (e)
                                     (equal? (format #f "~a"
                                                     (assq-ref e
                                                               'id))
                                             entry-id))
                                   (get-time-entries time-repo (assq-ref t
                                                                         'id))))))
            (when (and t
                       entry
                       (or moderator?
                           (and actor-id
                                (equal? (format #f "~a" actor-id)
                                        (format #f "~a"
                                                (assq-ref entry
                                                          'author-id))))))
              (soft-delete-time-entry time-repo
                                      (assq-ref entry
                                                'id))
              (record-event! (make-sqlite-ticket-event-repository rc)
                             #:ticket-id (assq-ref t
                                                   'id)
                             #:project-id (project-id-from-human-id
                                           project-repo proj)
                             #:actor-id actor-id
                             #:action "time-removed"
                             #:field "time"
                             #:old-value (format-duration (assq-ref entry
                                                                    'duration-minutes))))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-remove-time
                           #:unhappy-flow back-to-login)))

;; ===== Where the ticket sits in the plan =====
;;
;; Three references are set from the ticket's own page in one form: the day it
;; is due, the sprint it is being worked in, and the milestone it counts
;; toward.  All three are optional and all three may be cleared, so the form
;; posts all three every time and what it says is written as it stands.
;;
;; A sprint or a milestone is accepted only when it names one the project
;; actually has: a form is not a promise, and a ticket pointing at a sprint
;; that does not exist would be a ticket no board can show.  The empty row is
;; how the form says "none", and stores a NULL rather than a reference to
;; nothing.
;;
;; Putting a ticket into a sprint and taking it out are recorded as events of
;; their own -- `sprint-added' and `sprint-removed' -- rather than as an
;; ordinary change of a column.  The burndown replays them to know how much
;; work was still in the sprint on any given day, which a column holding only
;; what is true now could not answer.
(post "/tickets/:project/id/:human-id/planning"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-plan)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (sprint-repo (make-sqlite-sprint-repository rc))
                 (milestone-repo (make-sqlite-milestone-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (project-id (project-id-from-human-id project-repo proj))
                 (sprints (if project-id
                              (get-sprints sprint-repo project-id) '()))
                 (milestones (if project-id
                                 (get-milestones milestone-repo project-id)
                                 '()))
                 ;; A date the form left empty is a deadline somebody removed;
                 ;; a date it filled in with something that is not a day is a
                 ;; mistake, and leaves the deadline the ticket had.
                 (submitted-due (assoc-ref body "due-on"))
                 (submitted-sprint (assoc-ref body "sprint-id"))
                 (submitted-milestone (assoc-ref body "milestone-id"))
                 (due-on (cond
                           ((not (string? submitted-due))
                            (assq-ref t
                                      'due-on))
                           ((string-null? (string-trim-both submitted-due)) #f)
                           (else (or (parse-iso-date submitted-due)
                                     (assq-ref t
                                               'due-on)))))
                 ;; The row a form offers is the row that is taken; anything
                 ;; else -- a sprint of another project, an id typed by hand --
                 ;; is the same as choosing none, because a ticket must not be
                 ;; left pointing at something no page can show.
                 (offered-sprint (and (string? submitted-sprint)
                                      (find (lambda (s)
                                              (equal? (format #f "~a"
                                                              (assq-ref s
                                                                        'id))
                                                      submitted-sprint))
                                            sprints)))
                 (offered-milestone (and (string? submitted-milestone)
                                         (find (lambda (m)
                                                 (equal? (format #f "~a"
                                                                 (assq-ref m
                                                                           'id))
                                                         submitted-milestone))
                                               milestones)))
                 (wanted-sprint (and offered-sprint
                                     (format #f "~a"
                                             (assq-ref offered-sprint
                                                       'id))))
                 (wanted-milestone (and offered-milestone
                                        (format #f "~a"
                                                (assq-ref offered-milestone
                                                          'id)))))
            (when t
              (let* ((ticket-id (assq-ref t
                                          'id))
                     (actor-id (session-owner-id #:rc rc
                                                 #:token-name 'x-bewaking-lucidplan))
                     (event-repo (make-sqlite-ticket-event-repository rc))
                     (old-sprint (assq-ref t
                                           'sprint-id))
                     (old-milestone (assq-ref t
                                              'milestone-id)))
                (update-ticket-planning-by-id ticket-repo ticket-id due-on
                                              wanted-sprint wanted-milestone)
                (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                                proj human-id
                                                                `((updated-at
                                                                   unquote
                                                                   (current-time))))
                ;; A deadline is an ordinary field of the ticket, so it is
                ;; recorded the way every other change of one is.
                (unless (equal? (tracked-value (assq-ref t
                                                         'due-on))
                                (tracked-value due-on))
                  (record-event! event-repo
                                 #:ticket-id ticket-id
                                 #:project-id project-id
                                 #:actor-id actor-id
                                 #:action "changed"
                                 #:field "deadline"
                                 #:old-value (tracked-value (assq-ref t
                                                                      'due-on))
                                 #:new-value (tracked-value due-on)))
                ;; The two references are recorded as the movement they are,
                ;; because the burndown reads the movement rather than the
                ;; column.
                (unless (equal? (format #f "~a" (or old-sprint ""))
                                (format #f "~a" (or wanted-sprint "")))
                  (record-event! event-repo
                                 #:ticket-id ticket-id
                                 #:project-id project-id
                                 #:actor-id actor-id
                                 #:action (if wanted-sprint
                                              "sprint-added"
                                              "sprint-removed")
                                 #:field "sprint"
                                 #:old-value (and old-sprint
                                                 (format #f "~a" old-sprint))
                                 #:new-value (if wanted-sprint
                                                 (format #f "~a"
                                                         wanted-sprint)
                                                 (and old-sprint
                                                      (format #f "~a"
                                                              old-sprint)))))
                (unless (equal? (format #f "~a" (or old-milestone ""))
                                (format #f "~a" (or wanted-milestone "")))
                  (record-event! event-repo
                                 #:ticket-id ticket-id
                                 #:project-id project-id
                                 #:actor-id actor-id
                                 #:action "changed"
                                 #:field "milestone"
                                 #:old-value (and old-milestone
                                                 (format #f "~a"
                                                         old-milestone))
                                 #:new-value (and wanted-milestone
                                                 (format #f "~a"
                                                         wanted-milestone))))))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-plan
                           #:unhappy-flow back-to-login)))

;; ===== A ticket's own versions =====
;;
;; Restoring an old version is an ordinary edit whose new content is the old
;; content: it writes the ticket, records the change in the audit trail and
;; saves the result as a new version, so the versions in between stay where
;; they are.
(post "/tickets/:project/id/:human-id/revisions/:version/restore"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-restore)
          (let* ((proj (params rc "project"))
                 (human-id (params rc "human-id"))
                 (version (string->number (params rc "version")))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (project-repo (make-sqlite-project-repository rc))
                 (revision-repo (make-sqlite-ticket-revision-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 proj human-id))
                 (revision (and t version
                                (get-ticket-revision-by-version revision-repo
                                                                (assq-ref t
                                                                          'id)
                                                                version))))
            (when (and t revision)
              (let* ((project-id (project-id-from-human-id project-repo proj))
                     (actor-id (session-owner-id #:rc rc
                                                 #:token-name 'x-bewaking-lucidplan))
                     (old-version (current-version revision-repo
                                                   (assq-ref t
                                                             'id)))
                     (title (or (assq-ref revision
                                          'title)
                                (assq-ref t
                                          'title)))
                     (description (or (assq-ref revision
                                                'description) "")))
                (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                                proj human-id
                                                                `((title
                                                                   unquote
                                                                   title)
                                                                  (description
                                                                   unquote
                                                                   description)
                                                                  (ticket-status
                                                                   unquote
                                                                   (or (assq-ref
                                                                        revision
                                                                        'ticket-status)
                                                                       (assq-ref
                                                                        t
                                                                        'ticket-status)))
                                                                  (ticket-type
                                                                   unquote
                                                                   (or (assq-ref
                                                                        revision
                                                                        'ticket-type)
                                                                       (assq-ref
                                                                        t
                                                                        'ticket-type)))
                                                                  (priority
                                                                   unquote
                                                                   (or (assq-ref
                                                                        revision
                                                                        'priority)
                                                                       0))
                                                                  (updated_at
                                                                   unquote
                                                                   (current-time))))
                (update-ticket-estimates-by-id ticket-repo
                                               (assq-ref t
                                                         'id)
                                               (assq-ref revision
                                                         'estimate-points)
                                               (assq-ref revision
                                                         'estimate-size))
                (let ((restored (get-ticket-by-project-human-id-and-human-id
                                 ticket-repo proj human-id)))
                  (record-event! (make-sqlite-ticket-event-repository rc)
                                 #:ticket-id (assq-ref t
                                                       'id)
                                 #:project-id project-id
                                 #:actor-id actor-id
                                 #:action "restored"
                                 #:field "version"
                                 #:old-value (tracked-value old-version)
                                 #:new-value (tracked-value version))
                  (record-revision! revision-repo restored
                                    #:actor-id actor-id))))
            (redirect-to rc
                         (string->uri (format #f "~a/tickets/~a/id/~a"
                                              (current-myhost) proj human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-restore
                           #:unhappy-flow back-to-login)))

;; ===== Moving a ticket to another project =====
;;
;; The destination numbers the ticket itself: human ids count within a
;; project, so a moved ticket takes the next number of its new project.  Its
;; comments, files and assignment follow it; its history already names the
;; ticket by its internal id and is read that way.
(post "/tickets/:project/id/:human-id/move"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-move)
          (let* ((source (params rc "project"))
                 (human-id (params rc "human-id"))
                 (project-repo (make-sqlite-project-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (t (get-ticket-by-project-human-id-and-human-id ticket-repo
                                                                 source
                                                                 human-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (target (assoc-ref body "target-project")))
            (if (and t
                     (string? target)
                     (not (string-null? target))
                     (not (equal? target source))
                     (get-project-by-human-id project-repo target))
                (let* ((source-id (project-id-from-human-id project-repo
                                                            source))
                       (target-id (project-id-from-human-id project-repo
                                                            target))
                       (latest (get-ticket-of-latest-human-id ticket-repo
                                                              target))
                       (new-human-id (+ 1
                                        (or (assq-ref latest
                                                      'human-id) 0)))
                       (ticket-id (assq-ref t
                                            'id))
                       (actor-id (session-owner-id #:rc rc
                                                   #:token-name 'x-bewaking-lucidplan)))
                  (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                                  source
                                                                  human-id
                                                                  `((project-id
                                                                     unquote
                                                                     target-id)
                                                                    (human-id
                                                                     unquote
                                                                     new-human-id)
                                                                    (updated-at
                                                                     unquote
                                                                     (current-time))))
                  (move-ticket-assignee (make-sqlite-ticket-assignee-repository
                                         rc) ticket-id target-id)
                  (move-ticket-comments (make-sqlite-ticket-comment-repository
                                         rc) ticket-id target-id)
                  (move-ticket-attachments (make-sqlite-ticket-attachment-repository
                                            rc) ticket-id target-id)
                  ;; The hours spent on a ticket are part of its record, so
                  ;; they move with it: a time log counted in a project whose
                  ;; board no longer shows the ticket would be a report
                  ;; nothing can reconcile.
                  (move-time-entries (make-sqlite-time-entry-repository rc)
                                     ticket-id target-id)
                  (record-event! (make-sqlite-ticket-event-repository rc)
                                 #:ticket-id ticket-id
                                 #:project-id source-id
                                 #:actor-id actor-id
                                 #:action "moved"
                                 #:field "project"
                                 #:old-value source
                                 #:new-value target)
                  (redirect-to rc
                               (string->uri (format #f "~a/tickets/~a/id/~a"
                                                    (current-myhost) target
                                                    new-human-id))
                               #:status 302))
                (redirect-to rc
                             (string->uri (format #f "~a/tickets/~a/id/~a"
                                                  (current-myhost) source
                                                  human-id))
                             #:status 302))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-move
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
                (statuses (effective-ticket-statuses project-details))
                ;; The record the ticket carries with it: its discussion, its
                ;; files, what happened to it and the versions of what it
                ;; said.  They are read by ticket id, so a ticket that was
                ;; moved between projects takes them along.
                (comments (get-ticket-comments (make-sqlite-ticket-comment-repository
                                                rc)
                                               (assq-ref t
                                                         'id)))
                (attachments (get-ticket-attachments (make-sqlite-ticket-attachment-repository
                                                      rc)
                                                     (assq-ref t
                                                               'id)))
                (events (get-ticket-events (make-sqlite-ticket-event-repository
                                            rc)
                                           (assq-ref t
                                                     'id)))
                (revisions (get-ticket-revisions (make-sqlite-ticket-revision-repository
                                                  rc)
                                                 (assq-ref t
                                                           'id)))
                ;; The hours logged against the ticket, and what they add up
                ;; to: the entries are read newest day first, and the total is
                ;; the one number a reader who does not want to read the list
                ;; is shown.
                (time-entries (get-time-entries (make-sqlite-time-entry-repository
                                                 rc)
                                                (assq-ref t
                                                          'id)))
                (logged-minutes (time-entries-minutes time-entries))
                ;; The plan the ticket can be put in: the project's own
                ;; sprints and milestones, offered by the same page that
                ;; writes the choice.  A ticket whose project cannot be
                ;; resolved has no plan to be put in, which is an empty list
                ;; rather than an error.
                (project-id (and project-details
                                 (assq-ref project-details
                                           'id)))
                (sprints (if project-id
                             (get-sprints (make-sqlite-sprint-repository rc)
                                          project-id) '()))
                (milestones (if project-id
                                (get-milestones (make-sqlite-milestone-repository
                                                 rc) project-id) '()))
                ;; The card offers its inline controls to the same reader the
                ;; endpoint behind them would accept: holding the permission,
                ;; not merely being signed in.
                (editable? (session-has-permission? #:rc rc
                                                    #:token-name 'x-bewaking-lucidplan
                                                    #:permission 'lucidplan:ticket-create))
                ;; The card a change is confirmed with, inert in the page
                ;; until the script has something to say.
                (quiet-notice (render-quiet-notice (:i18n rc)))
                ;; The ticket itself.  Signed out, its status and its type
                ;; stay the badges they read as at a glance; signed in, each
                ;; of the two is the small control that posts its own change.
                (ticket-card (render-ticket (:i18n rc)
                                            #:project (params rc "project")
                                            #:ticket t
                                            #:statuses statuses
                                            #:today (today-iso)
                                            #:logged-minutes logged-minutes
                                            #:assignee (if (nil?
                                                            ticket-assignee-user)
                                                           #f
                                                           (assq-ref
                                                            ticket-assignee-user
                                                            'username))
                                            #:editable? editable?
                                            ;; The ticket's own page is the
                                            ;; one card that offers to
                                            ;; estimate it at a table, and it
                                            ;; offers that to the reader the
                                            ;; estimate menus above are drawn
                                            ;; for -- the endpoint behind it
                                            ;; asks the same permission.
                                            #:planning-table? editable?)))
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
                                                ;; One column, centred and of a
                                                ;; readable width: the card and
                                                ;; the panels under it are the
                                                ;; page, and a page stretched
                                                ;; across a wide screen leaves
                                                ;; a reader hunting for the
                                                ;; ends of its own lines.
                                                #:body `((div (@ (class ,(mk-class '(w-full
                                                                                     max-w-256
                                                                                     mx-auto
                                                                                     flex
                                                                                     flex-col
                                                                                     gap-4
                                                                                     px-3
                                                                                     md:px-6
                                                                                     py-6))))
                                                              ,(a (format #f
                                                              "← back to backlog: ~a"
                                                              (params rc
                                                               "project"))
                                                             #:size '(text-sm
                                                                      w-fit)
                                                             #:href (format #f
                                                                            "/tickets/~a"

                                                                            (params
                                                                             rc
                                                                             "project")))
                                                              ,ticket-card
                                                         ,(html-ticket-collaboration
                                                           (:i18n rc)
                                                           #:project (params
                                                                      rc
                                                                      "project")
                                                           #:ticket t
                                                           #:projects proj
                                                           #:statuses statuses
                                                           #:users user-names
                                                           #:comments comments
                                                           #:attachments
                                                           attachments
                                                           #:events events
                                                           #:revisions
                                                           revisions
                                                           ;; The hours spent on the
                                                           ;; ticket, and the plan it
                                                           ;; can be put in.
                                                           #:time-entries
                                                           time-entries
                                                           #:logged-minutes
                                                           logged-minutes
                                                           #:sprints sprints
                                                           #:milestones
                                                           milestones
                                                           #:today (today-iso)
                                                           #:editable?
                                                           editable?
                                                           ;; Who is reading, and whether
                                                           ;; they may moderate: the
                                                           ;; discussion offers each
                                                           ;; comment's remove control by
                                                           ;; the same rule the endpoint
                                                           ;; applies it by.
                                                           #:viewer-id (session-owner-id
                                                                        #:rc
                                                                        rc
                                                                        #:token-name 'x-bewaking-lucidplan)
                                                           #:moderator? (session-has-permission?
                                                                         #:rc
                                                                         rc
                                                                         #:token-name 'x-bewaking-lucidplan
                                                                         #:permission 'lucidplan:ticket-delete))
                                                              ,@quiet-notice)))))))
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
