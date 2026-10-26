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

;;; The pages of the plan: a project's sprints with their burndown, and the
;;; milestones its work counts toward.
;;;
;;; Three rules are worth stating once, because every endpoint below keeps
;;; them and none of them is obvious from a single handler.
;;;
;;; A project is addressed by the human id its URLs carry, and its internal id
;;; -- which is what a sprint, a milestone and an event actually reference --
;;; is resolved once, here, at the top of a handler.  A project that cannot be
;;; resolved ends the request with a plain "no such project" rather than with
;;; a query that matches nothing.
;;;
;;; A calendar date arrives from an `<input type="date">' as `YYYY-MM-DD' and
;;; is checked by `parse-iso-date', which refuses the 31st of February rather
;;; than rolling it into March.  What a form left empty is the absence of a
;;; date, which is a state -- a sprint nobody has dated, a milestone nobody
;;; has promised a day for -- and is stored as NULL.
;;;
;;; A sprint is started, closed or reopened through the same endpoint,
;;; because they are the same change: one column, moved to a value on the
;;; ladder.  A project works in at most one sprint at a time, so starting a
;;; sprint puts any other running one back to `planned' -- it is not closed,
;;; since closing is the team's own statement that the work is done, but it
;;; stops claiming to be the sprint the team is in.

(define-artanis-controller scrum)

(import (app views scrum)
        (web uri)
        (ice-9 string-fun)
        (srfi srfi-1)
        (lucidplan auth)
        (kracht prelude)
        (uuid generate)
        (lucidplan domain)
        (lucidplan domain project)
        (lucidplan domain ticket)
        (lucidplan domain ticket-event)
        (lucidplan domain sprint)
        (lucidplan domain milestone)
        (lucidplan sqlite project)
        (lucidplan sqlite ticket)
        (lucidplan sqlite ticket-event)
        (lucidplan sqlite sprint)
        (lucidplan sqlite milestone)
        (artanis security nss)
        (hygguile hygguile))

(define (new-id)
  (string-replace-substring (generate-string-uuid) "\n" ""))

(define (sprints-url project)
  (format #f "~a/projects/~a/sprints" (current-myhost) project))

(define (sprint-url project sprint-id)
  (format #f "~a/projects/~a/sprints/~a" (current-myhost) project sprint-id))

(define (milestones-url project)
  (format #f "~a/projects/~a/milestones" (current-myhost) project))

(define (url-of text)
  (string->uri text))

;; Every write is left behind in the ticket's own audit trail: a ticket taken
;; into a sprint, one taken out of it, and a deadline that moved.  The burndown
;; is replayed from these rows, so a change that was not recorded is a change
;; the chart does not know happened.
(define* (record-ticket-event! rc
                               #:key
                               ticket-id
                               project-id
                               actor-id
                               action
                               field
                               old-value
                               new-value)
  (insert-ticket-event (make-sqlite-ticket-event-repository rc)
                       (append `((id unquote
                                     (new-id))
                                 (ticket-id unquote ticket-id)
                                 (project-id unquote project-id)
                                 (action unquote action)
                                 (created-at unquote
                                             (current-time)))
                               (if actor-id
                                   `((actor-id unquote actor-id))
                                   '())
                               (if field
                                   `((field unquote field))
                                   '())
                               (if (and old-value
                                        (not (string-null? old-value)))
                                   `((old-value unquote old-value))
                                   '())
                               (if (and new-value
                                        (not (string-null? new-value)))
                                   `((new-value unquote new-value))
                                   '()))))

;; ===== A project's sprints =====

(get "/projects/:project/sprints"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (sprint-repo (make-sqlite-sprint-repository rc))
              (ticket-repo (make-sqlite-ticket-repository rc))
              (human (params rc "project"))
              (p (get-project-by-human-id project-repo human)))
         (if (not p)
             (redirect-to rc (url-of (format #f "~a/projects"
                                             (current-myhost)))
                          #:status 302)
             (let ((project-id (assq-ref p
                                         'id)))
               (response-emit (:mime rc
                                     (html-from-sprints (:i18n rc)
                                                        #:projects (get-projects
                                                                    project-repo)
                                                        #:project human
                                                        #:sprints (get-sprints
                                                                   sprint-repo
                                                                   project-id)
                                                        #:tickets (get-tickets
                                                                   ticket-repo human
                                                                   '((human-id
                                                                      asc)))
                                                        #:statuses (effective-ticket-statuses
                                                                    p)
                                                        #:today (today-iso)
                                                        #:editable? (session-has-permission?
                                                                     #:rc rc
                                                                     #:token-name 'x-bewaking-lucidplan
                                                                     #:permission 'lucidplan:ticket-create)
                                                        #:is-logged-in (:cookies-value
                                                                        rc
                                                                        "x-bewaking-lucidplan")))))))))

(post "/projects/:project/sprints"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-create)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (sprint-repo (make-sqlite-sprint-repository rc))
                 (human (params rc "project"))
                 (p (get-project-by-human-id project-repo human))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (assoc-ref body "title"))
                 (goal (assoc-ref body "goal"))
                 ;; An empty field is a sprint nobody has dated, and a value
                 ;; that is not a day is one too: the sprint is still written,
                 ;; because a plan that refuses to be recorded until its dates
                 ;; are agreed is a plan people stop recording.
                 (starts-on (parse-iso-date (assoc-ref body "starts-on")))
                 (ends-on (parse-iso-date (assoc-ref body "ends-on"))))
            (when (and p
                       (string? title)
                       (not (string-null? (string-trim-both title)))
                       (sprint-spans-valid? starts-on ends-on))
              (insert-sprint sprint-repo
                             (append `((id unquote
                                           (new-id))
                                       (project-id unquote
                                                   (assq-ref p
                                                             'id))
                                       (title unquote
                                              (html-escape
                                               (string-trim-both title)))
                                       (sprint-status unquote
                                                      "planned")
                                       (created-at unquote
                                                   (current-time)))
                                     (if (and goal
                                              (not (string-null? (string-trim-both
                                                                  goal))))
                                         `((goal unquote
                                                 (html-escape
                                                  (string-trim-both goal))))
                                         '())
                                     (if starts-on
                                         `((starts-on unquote starts-on))
                                         '())
                                     (if ends-on
                                         `((ends-on unquote ends-on))
                                         '()))))
            (redirect-to rc (url-of (sprints-url human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-create
                           #:unhappy-flow back-to-login)))

;; One sprint's own page: what is in it, what it was given, and the line it
;; should have burnt down along.  The numbers come from the burndown the domain
;; computes over the audit trail of the sprint's tickets, so the chart and the
;; totals above it are the same figures said twice.
(get "/projects/:project/sprints/:sprint-id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (sprint-repo (make-sqlite-sprint-repository rc))
              (ticket-repo (make-sqlite-ticket-repository rc))
              (event-repo (make-sqlite-ticket-event-repository rc))
              (human (params rc "project"))
              (sprint-id (params rc "sprint-id"))
              (p (get-project-by-human-id project-repo human))
              (sprint (get-sprint-by-id sprint-repo sprint-id)))
         (cond
           ((not p)
            (redirect-to rc (url-of (format #f "~a/projects"
                                            (current-myhost)))
                         #:status 302))
           ;; A sprint id that names nothing, or names a sprint of another
           ;; project, is a wrong turn: the reader is sent back to the list
           ;; they came from rather than shown a page with nothing in it.
           ((not (and sprint
                      (equal? (format #f "~a" (assq-ref sprint
                                                        'project-id))
                              (format #f "~a" (assq-ref p
                                                        'id)))))
            (redirect-to rc (url-of (sprints-url human))
                         #:status 302))
           (else (let* ((statuses (effective-ticket-statuses p))
                        (in-sprint (get-tickets-by-sprint ticket-repo sprint-id
                                                          '((human-id asc))))
                        (all-tickets (get-tickets ticket-repo human
                                                  '((human-id asc))))
                        (in-ids (map (lambda (t)
                                       (format #f "~a" (assq-ref t
                                                                 'id)))
                                     in-sprint))
                        (backlog (filter (lambda (t)
                                           (not (member (format #f "~a"
                                                               (assq-ref t
                                                                         'id))
                                                        in-ids)))
                                         all-tickets))
                        (events (append-map (lambda (t)
                                              (get-ticket-events event-repo
                                                                 (assq-ref t
                                                                           'id)))
                                            in-sprint))
                        (rows (burndown-series #:sprint-id sprint-id
                                               #:starts-on (assq-ref sprint
                                                                     'starts-on)
                                               #:ends-on (assq-ref sprint
                                                                   'ends-on)
                                               #:tickets in-sprint
                                               #:events events
                                               #:statuses statuses
                                               #:today (today-iso))))
                   (response-emit (:mime rc
                                         (html-sprint-board (:i18n rc)
                                                            #:projects (get-projects
                                                                        project-repo)
                                                            #:project human
                                                            #:sprint sprint
                                                            #:tickets in-sprint
                                                            #:backlog backlog
                                                            #:statuses statuses
                                                            #:rows rows
                                                            #:summary (burndown-summary
                                                                       rows)
                                                            #:today (today-iso)
                                                            #:editable? (session-has-permission?
                                                                         #:rc
                                                                         rc
                                                                         #:token-name 'x-bewaking-lucidplan
                                                                         #:permission 'lucidplan:ticket-create)
                                                            #:is-logged-in (:cookies-value
                                                                            rc
                                                                            "x-bewaking-lucidplan"))))))))))

;; Starting, closing and reopening a sprint are one change, so they are one
;; endpoint: the state the form posts has to be one the ladder names, and the
;; sprint it names has to be this project's.
(post "/projects/:project/sprints/:sprint-id/status"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-status)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (sprint-repo (make-sqlite-sprint-repository rc))
                 (human (params rc "project"))
                 (sprint-id (params rc "sprint-id"))
                 (p (get-project-by-human-id project-repo human))
                 (sprint (get-sprint-by-id sprint-repo sprint-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (status (parse-sprint-status (assoc-ref body "sprint-status"))))
            (when (and p sprint status
                       (equal? (format #f "~a" (assq-ref sprint
                                                         'project-id))
                               (format #f "~a" (assq-ref p
                                                         'id))))
              ;; One sprint at a time: whatever else was the sprint the team
              ;; was in stops claiming to be, so `get-running-sprint' can
              ;; answer with one row rather than a list the pages would have
              ;; to choose from.
              (when (sprint-running? status)
                (for-each (lambda (other)
                            (when (and (sprint-running? (assq-ref other
                                                                  'sprint-status))
                                       (not (equal? (format #f "~a"
                                                           (assq-ref other
                                                                     'id))
                                                    sprint-id)))
                              (update-sprint-by-id sprint-repo (assq-ref other
                                                                         'id)
                                                   `((sprint-status unquote
                                                                    "planned")
                                                     (updated-at unquote
                                                                 (current-time))))))
                          (get-sprints sprint-repo (assq-ref p
                                                             'id))))
              (update-sprint-by-id sprint-repo sprint-id
                                   `((sprint-status unquote status)
                                     (updated-at unquote
                                                 (current-time)))))
            (redirect-to rc (url-of (sprints-url human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-status
                           #:unhappy-flow back-to-login)))

;; Deleting a sprint does not delete the work in it.  The tickets that were in
;; the sprint are taken out of it first -- their rows are left exactly as they
;; were, only no longer in a sprint -- and then the sprint itself is removed,
;; so a deleted sprint leaves a backlog rather than a set of tickets pointing
;; at something no page can show.
(post "/projects/:project/sprints/:sprint-id/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (sprint-repo (make-sqlite-sprint-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (human (params rc "project"))
                 (sprint-id (params rc "sprint-id"))
                 (p (get-project-by-human-id project-repo human))
                 (sprint (get-sprint-by-id sprint-repo sprint-id)))
            (when (and p sprint
                       (equal? (format #f "~a" (assq-ref sprint
                                                         'project-id))
                               (format #f "~a" (assq-ref p
                                                         'id))))
              (unassign-sprint ticket-repo sprint-id)
              (soft-delete-sprint sprint-repo sprint-id))
            (redirect-to rc (url-of (sprints-url human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))

;; Putting a ticket into a sprint and taking it out are the two directions of
;; one change, so they are one endpoint with the direction in the body.  Both
;; are recorded as events of their own -- `sprint-added' and `sprint-removed'
;; -- because the burndown replays them to know how much work was in the sprint
;; on a given day; a column holding only what is true now could not answer
;; that question.  The ticket's deadline and milestone are carried over
;; untouched: only the sprint moved.
(post "/projects/:project/sprints/:sprint-id/tickets"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-assign)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (sprint-repo (make-sqlite-sprint-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (human (params rc "project"))
                 (sprint-id (params rc "sprint-id"))
                 (p (get-project-by-human-id project-repo human))
                 (sprint (get-sprint-by-id sprint-repo sprint-id))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (ticket-id (assoc-ref body "ticket"))
                 (direction (assoc-ref body "action"))
                 ;; The ticket is looked for among this project's own tickets,
                 ;; never by its id alone: a ticket of another project cannot
                 ;; be pulled into this sprint by naming it here.
                 (ticket (and p
                              (find (lambda (t)
                                      (equal? (format #f "~a"
                                                      (assq-ref t
                                                                'id))
                                              ticket-id))
                                    (get-tickets ticket-repo human
                                                 '((human-id asc))))))
                 (target (cond
                           ((equal? direction "add") sprint-id)
                           ((equal? direction "remove") #f)
                           (else 'unknown))))
            (when (and p sprint ticket
                       (equal? (format #f "~a" (assq-ref sprint
                                                         'project-id))
                               (format #f "~a" (assq-ref p
                                                         'id)))
                       (not (eq? target 'unknown)))
              (let ((project-id (assq-ref p
                                          'id))
                    (ticket-id (assq-ref ticket
                                         'id))
                    (actor-id (session-owner-id #:rc rc
                                                #:token-name 'x-bewaking-lucidplan)))
                ;; The deadline and the milestone are read back and written
                ;; again with the sprint: they are one statement, and leaving
                ;; them out of it would clear them.
                (update-ticket-planning-by-id ticket-repo ticket-id
                                              (assq-ref ticket
                                                        'due-on) target
                                              (assq-ref ticket
                                                        'milestone-id))
                (update-ticket-by-project-human-id-and-human-id ticket-repo
                                                                human
                                                                (format #f
                                                                        "~a"
                                                                        (assq-ref
                                                                         ticket
                                                                         'human-id))
                                                                `((updated-at
                                                                   unquote
                                                                   (current-time))))
                (record-ticket-event! rc
                                      #:ticket-id ticket-id
                                      #:project-id project-id
                                      #:actor-id actor-id
                                      #:action (if target
                                                   "sprint-added"
                                                   "sprint-removed")
                                      #:field "sprint"
                                      #:old-value (and (assq-ref ticket
                                                                 'sprint-id)
                                                       (format #f
                                                               "~a"
                                                               (assq-ref ticket
                                                                         'sprint-id)))
                                      #:new-value (if target sprint-id
                                                      (format #f "~a"
                                                              (assq-ref ticket
                                                                        'sprint-id))))))
            (redirect-to rc (url-of (sprint-url human sprint-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-assign
                           #:unhappy-flow back-to-login)))

;; ===== A project's milestones =====

(get "/projects/:project/milestones"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (milestone-repo (make-sqlite-milestone-repository rc))
              (ticket-repo (make-sqlite-ticket-repository rc))
              (human (params rc "project"))
              (p (get-project-by-human-id project-repo human)))
         (if (not p)
             (redirect-to rc (url-of (format #f "~a/projects"
                                             (current-myhost)))
                          #:status 302)
             (let ((project-id (assq-ref p
                                         'id)))
               (response-emit (:mime rc
                                     (html-from-milestones (:i18n rc)
                                                           #:projects (get-projects
                                                                       project-repo)
                                                           #:project human
                                                           #:milestones (get-milestones
                                                                         milestone-repo
                                                                         project-id)
                                                           #:tickets (get-tickets
                                                                      ticket-repo
                                                                      human
                                                                      '((human-id
                                                                         asc)))
                                                           #:statuses (effective-ticket-statuses
                                                                       p)
                                                           #:today (today-iso)
                                                           #:editable? (session-has-permission?
                                                                        #:rc rc
                                                                        #:token-name 'x-bewaking-lucidplan
                                                                        #:permission 'lucidplan:ticket-create)
                                                           #:is-logged-in (:cookies-value
                                                                           rc
                                                                           "x-bewaking-lucidplan")))))))))

(post "/projects/:project/milestones"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-create)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (milestone-repo (make-sqlite-milestone-repository rc))
                 (human (params rc "project"))
                 (p (get-project-by-human-id project-repo human))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (assoc-ref body "title"))
                 (description (assoc-ref body "description"))
                 (due-on (parse-iso-date (assoc-ref body "due-on"))))
            (when (and p
                       (string? title)
                       (not (string-null? (string-trim-both title))))
              (insert-milestone milestone-repo
                                (append `((id unquote
                                              (new-id))
                                          (project-id unquote
                                                      (assq-ref p
                                                                'id))
                                          (title unquote
                                                 (html-escape
                                                  (string-trim-both title)))
                                          (created-at unquote
                                                      (current-time)))
                                        (if (and description
                                                 (not (string-null?
                                                       (string-trim-both
                                                        description))))
                                            `((description unquote
                                                           (html-escape
                                                            (string-trim-both
                                                             description))))
                                            '())
                                        (if due-on
                                            `((due-on unquote due-on))
                                            '()))))
            (redirect-to rc (url-of (milestones-url human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-create
                           #:unhappy-flow back-to-login)))

;; Deleting a milestone leaves its tickets where they are -- their rows are
;; untouched, they simply no longer count toward anything -- for the same
;; reason deleting a sprint does: a milestone is a promise about the work, and
;; withdrawing the promise does not withdraw the work.
(post "/projects/:project/milestones/:milestone-id/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((project-repo (make-sqlite-project-repository rc))
                 (milestone-repo (make-sqlite-milestone-repository rc))
                 (ticket-repo (make-sqlite-ticket-repository rc))
                 (human (params rc "project"))
                 (milestone-id (params rc "milestone-id"))
                 (p (get-project-by-human-id project-repo human))
                 (milestone (get-milestone-by-id milestone-repo milestone-id)))
            (when (and p milestone
                       (equal? (format #f "~a" (assq-ref milestone
                                                          'project-id))
                               (format #f "~a" (assq-ref p
                                                         'id))))
              (unassign-milestone ticket-repo milestone-id)
              (soft-delete-milestone milestone-repo milestone-id))
            (redirect-to rc (url-of (milestones-url human))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
