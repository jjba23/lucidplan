;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller kanban)

(import (app views kanban)
        (web uri)
        (ice-9 string-fun)
        (lucidplan auth)
        (kracht prelude)
        (lucidplan domain)
        (lucidplan sqlite ticket)
        (lucidplan sqlite project)
        (lucidplan domain ticket)
        (lucidplan domain project)
        (kracht prelude)
        (artanis security nss))

(get "/kanban/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan lucidplan-ticket-sort)
     (lambda (rc)
       (define (get)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (proj (get-projects project-repo))
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
                (query-sort (entity-sort-from-query user-sort))
                ;; The columns the board draws are this project's own statuses:
                ;; its list if it kept one, the instance's otherwise.
                (project-details (get-project-by-human-id project-repo
                                                          (params rc "project")))
                (statuses (effective-ticket-statuses project-details))
                (project-tickets (get-tickets ticket-repo
                                              (params rc "project") query-sort))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assq-ref t
                                                           'id) unquote
                                                (assq-ref t
                                                          'human-id)))
                                            project-tickets)))
           (response-emit (:mime rc
                                 (kanban-from-tickets (:i18n rc)
                                                      #:projects proj
                                                      #:project (params rc
                                                                 "project")
                                                      #:tickets
                                                      project-tickets
                                                      #:statuses statuses
                                                      #:current-sort user-sort
                                                      #:ticket-id-to-human-id
                                                      ticket-id-to-human-id
                                                      ;; The board lets a reader
                                                      ;; drag a card into another
                                                      ;; column, which is a change
                                                      ;; to the ticket: the grip is
                                                      ;; offered to the same reader
                                                      ;; the endpoint that writes it
                                                      ;; accepts.
                                                      #:can-drag? (session-has-permission?
                                                                   #:rc rc
                                                                   #:token-name 'x-bewaking-lucidplan
                                                                   #:permission 'lucidplan:ticket-create)
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-lucidplan"))))))
       (get)))
