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
        (lucidplan uri)
        (lucidplan domain)
        (lucidplan query-escape)
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

(get "/kanban/:project"
     #:mime 'xml
     #:session #t
     #:i18n '(cookie "lang")
     #:cookies '(names sid)
     (lambda (rc)
       (define (get)
         (let* ((project-id (project-id-from-human-id (params rc "project")))
                (proj ($project 'get
                                #:order-by '(human_id asc)))
                (project-tickets ($ticket 'get
                                          #:condition (where #:project_id
                                                             project-id)
                                          #:order-by '(updated_at desc)))
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
                                                      #:ticket-id-to-human-id
                                                      ticket-id-to-human-id
                                                      #:is-logged-in (:cookies-value
                                                                      rc "sid"))))))
       (get)))
