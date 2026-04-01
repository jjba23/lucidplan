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
        (lucidplan sqlite ticket)
        (lucidplan sqlite project)
        (kracht prelude)
        (artanis security nss))

(define (project-id-from-human-id rc human-id)
  (let* ((project-repo (make-sqlite-project-repository rc))
         (p (get-active-project-by-human-id project-repo human-id)))
    (cond
      ((null? p)
       "")
      (else (assq-ref p
                      'id)))))

(get "/kanban/:project"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan lucidplan-ticket-sort)
     (lambda (rc)
       (define (get)
         (let* ((project-id (project-id-from-human-id rc
                                                      (params rc "project")))
                (project-repo (make-sqlite-project-repository rc))
                (ticket-repo (make-sqlite-ticket-repository rc))
                (proj (get-active-projects project-repo))
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
                (project-tickets (get-active-tickets ticket-repo project-id
                                                     (format #f "~a ~a"
                                                             (car query-sort)
                                                             (cadr query-sort))))
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
                                                      #:current-sort user-sort
                                                      #:ticket-id-to-human-id
                                                      ticket-id-to-human-id
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-lucidplan"))))))
       (get)))
