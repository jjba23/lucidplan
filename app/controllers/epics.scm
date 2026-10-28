;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller epics)

(import (app views epics)
        (web uri)
        (ice-9 string-fun)
        (lucidplan auth)
        (kracht prelude)
        (uuid generate)
        (artanis security nss)
        (lucidplan domain)
        (lucidplan domain project)
        (lucidplan domain epic)
        (lucidplan domain ticket)
        (lucidplan sqlite project)
        (lucidplan sqlite epic)
        (lucidplan sqlite ticket))

;;; ===== The epics of a project =====
;;;
;;; An epic is read by everybody who may read the project -- it is a way of
;;; grouping the tickets that are already public on the backlog -- and written
;;; by whoever may write a ticket, because that is what putting one in an epic
;;; is: an edit to the ticket.
;;;
;;; Progress is counted here and nowhere else.  The page is handed the tickets
;;; of the project as well as its epics, and says how far each epic has got by
;;; counting them (see `epic-progress'), so the number on this page and the
;;; number of ticked tickets on the backlog are the same number because they
;;; are the same query.

(define (epics-of rc project-repo human-id)
  "Answer EPICS and TICKETS of the project HUMAN-ID, as two values."
  (let* ((project (get-project-by-human-id project-repo human-id))
         (project-id (and project
                          (assq-ref project
                                    'id)))
         (epic-repo (make-sqlite-epic-repository rc))
         (ticket-repo (make-sqlite-ticket-repository rc)))
    (values (if project-id
                (get-epics epic-repo project-id)
                '())
            ;; `get-tickets' resolves the project it is given -- a human id, the
            ;; name a ticket's address is written with -- to the row id its own
            ;; statement filters on, so it is the human id that is handed over
            ;; here, not the uuid the epics are stored under.
            (if project-id
                (get-tickets ticket-repo human-id
                             '((human-id asc)))
                '())
            (and project
                 (effective-ticket-statuses project)))))

(get "/epics/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((human-id (params rc "project"))
              (project-repo (make-sqlite-project-repository rc))
              (project (get-project-by-human-id project-repo human-id)))
         (if (not project)
             (throw 'artanis-err 404 get "/epics/:project"
                    "no such project")
             (call-with-values (lambda ()
                                 (epics-of rc project-repo human-id))
               (lambda (epics tickets statuses)
                 (response-emit (:mime rc
                                       (epics-page (:i18n rc)
                                                   #:project project
                                                   #:epics epics
                                                   #:tickets tickets
                                                   #:statuses statuses
                                                   #:projects (get-projects
                                                               project-repo)
                                                   #:is-logged-in (:cookies-value
                                                                   rc
                                                                   "x-bewaking-lucidplan")
                                                   #:can-edit? (session-has-permission?
                                                                #:rc rc
                                                                #:token-name 'x-bewaking-lucidplan
                                                                #:permission 'lucidplan:ticket-create))))))))))

;; The form is the same one for a new epic and an old one; what differs is
;; what it opens with and where it posts.
(get "/epics/:project/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-new)
         (let* ((human-id (params rc "project"))
                (project-repo (make-sqlite-project-repository rc))
                (project (get-project-by-human-id project-repo human-id)))
           (response-emit (:mime rc
                                 (epic-form-page (:i18n rc)
                                                 #:project project
                                                 #:projects (get-projects
                                                             project-repo)
                                                 #:is-logged-in (:cookies-value
                                                                 rc
                                                                 "x-bewaking-lucidplan"))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:ticket-create)
                          #:rc rc
                          #:happy-flow get-new
                          #:unhappy-flow back-to-login)))

(get "/epics/:project/id/:human-id/edit"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-edit)
         (let* ((human-id (params rc "project"))
                (project-repo (make-sqlite-project-repository rc))
                (project (get-project-by-human-id project-repo human-id))
                (epic-repo (make-sqlite-epic-repository rc))
                (epic (and project
                           (get-epic-by-project-and-human-id epic-repo
                                                             (assq-ref
                                                              project
                                                              'id)
                                                             (params rc
                                                              "human-id")))))
           (response-emit (:mime rc
                                 (epic-form-page (:i18n rc)
                                                 #:project project
                                                 #:projects (get-projects
                                                             project-repo)
                                                 #:epic epic
                                                 #:title (if epic
                                                             (assq-ref
                                                              epic
                                                              'title)
                                                             "")
                                                 #:human-id (if epic
                                                                (assq-ref
                                                                 epic
                                                                 'human-id)
                                                                "")
                                                 #:description (if epic
                                                                   (or (assq-ref
                                                                        epic
                                                                        'description)
                                                                       "")
                                                                   "")
                                                 #:is-logged-in (:cookies-value
                                                                 rc
                                                                 "x-bewaking-lucidplan"))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:ticket-create)
                          #:rc rc
                          #:happy-flow get-edit
                          #:unhappy-flow back-to-login)))

(post "/epics/:project/new"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-new)
          (let* ((human-id (params rc "project"))
                 (project-repo (make-sqlite-project-repository rc))
                 (project (get-project-by-human-id project-repo human-id))
                 (epic-repo (make-sqlite-epic-repository rc))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (or (assoc-ref body "title") "")))
                 (slug (epic-slug (assoc-ref body "human-id")))
                 (description (html-escape (or (assoc-ref body
                                                          "description")
                                               ""))))
            ;; A title and a slug are what an epic is named by; without either
            ;; there is no row to write, so the form is answered with the list
            ;; rather than with a row no address could reach.
            (when (and project title slug)
              (insert-epic epic-repo
                            `((id unquote
                                  (string-replace-substring
                                   (generate-string-uuid) "\n" ""))
                              (project-id unquote
                                          (assq-ref project
                                                    'id))
                              (human-id unquote slug)
                              (title unquote title)
                              (description unquote description)
                              (created-at unquote
                                          (current-time)))))
            (redirect-to rc
                         (string->uri (format #f "~a/epics/~a"
                                              (current-myhost) human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-new
                           #:unhappy-flow back-to-login)))

(post "/epics/:project/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((human-id (params rc "project"))
                 (project-repo (make-sqlite-project-repository rc))
                 (project (get-project-by-human-id project-repo human-id))
                 (epic-repo (make-sqlite-epic-repository rc))
                 (epic (and project
                            (get-epic-by-project-and-human-id epic-repo
                                                              (assq-ref
                                                               project
                                                               'id)
                                                              (params rc
                                                               "human-id"))))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (or (assoc-ref body "title") "")))
                 (slug (epic-slug (assoc-ref body "human-id")))
                 (description (html-escape (or (assoc-ref body
                                                          "description")
                                               ""))))
            (when (and epic title slug)
              (update-epic-by-id epic-repo
                                 (assq-ref epic
                                           'id)
                                 `((human-id unquote slug)
                                   (title unquote title)
                                   (description unquote description)
                                   (updated-at unquote
                                               (current-time)))))
            (redirect-to rc
                         (string->uri (format #f "~a/epics/~a"
                                              (current-myhost) human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))

;; Removing an epic removes the grouping, not the work: the tickets that
;; pointed at it keep pointing at a row the pages no longer draw, which reads
;; as a ticket with no epic -- exactly where they started -- until somebody
;; gives them another one.
(post "/epics/:project/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((human-id (params rc "project"))
                 (project-repo (make-sqlite-project-repository rc))
                 (epic-repo (make-sqlite-epic-repository rc))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id")))
            (when (and id
                       (get-epic-by-id epic-repo id))
              (soft-delete-epic epic-repo id))
            (redirect-to rc
                         (string->uri (format #f "~a/epics/~a"
                                              (current-myhost) human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:ticket-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
