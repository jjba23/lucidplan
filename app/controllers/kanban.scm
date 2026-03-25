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

(define-artanis-controller kanban)

(import (app views kanban)
        (web uri)
        (ice-9 string-fun)
        (lucidplan auth)
        (kracht prelude)
        (lucidplan domain)
        (kracht prelude)
        (app models ticket)
        (artanis security nss)
        (app models project))

(define (project-id-from-human-id project)
  (let ((p (DB-get-all-rows (DB-query (DB-open rc)
                                      (format #f
                                       "SELECT * FROM project WHERE human_id = '~a' deleted_at IS NULL LIMIT 1"
                                       project)))))
    (cond
      ((null? p)
       "")
      (else (assoc-ref (car p) "id")))))

(get "/kanban/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan lucidplan-ticket-sort)
     (lambda (rc)
       (define (get)
         (let* ((project-id (project-id-from-human-id (params rc "project")))
                (proj (DB-get-all-rows (DB-query (DB-open rc)
                                        "SELECT * FROM project WHERE deleted_at IS NULL ORDER BY human_id ASC")))
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
                (ticket-query (->sql select * from
                                     'ticket
                                     (format #f
                                      " WHERE project_id = '~a' ORDER BY ~a ~a"
                                      project-id
                                      (car query-sort)
                                      (cadr query-sort))))
                (project-tickets (DB-get-all-rows (DB-query (DB-open rc)
                                                            ticket-query)))
                (ticket-id-to-human-id (map (lambda (t)
                                              `(,(assoc-ref t "id") unquote
                                                (assoc-ref t "human_id")))
                                            project-tickets)))
           (response-emit (:mime rc
                                 (kanban-from-tickets (:i18n rc)
                                                      #:projects proj
                                                      #:project (params rc
                                                                 "project")
                                                      #:tickets
                                                      project-tickets
                                                      #:current-sort user-sort
                                                      #:ticket-id-to-human-id
                                                      ticket-id-to-human-id
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-lucidplan"))))))
       (get)))
