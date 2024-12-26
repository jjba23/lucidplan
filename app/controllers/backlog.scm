;;; index.scm

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

;; lucidplan is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lucidplan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

(define-artanis-controller index)

(import (lucidplan html backlog)
        (lucidplan persistence backlog)
        (web uri)
        (lucidplan uri)
        (lucidplan uuid))

(get "/backlog/:project"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (html-from-backlog (params rc "project")
                                                (read-lucidplan-backlog (params
                                                                         rc
                                                                         "project")))))))

(get "/backlog/:project/tickets/new"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (html-new-ticket-form (params rc "project"))))))

(post "/backlog/:project/tickets/new-submit"
      #:mime 'xml
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (title (uri-decode (car (assoc-ref kv "title"))))
               (ticket-status (uri-decode (car (assoc-ref kv "ticket-status"))))
               (description (uri-decode (car (assoc-ref kv "description"))))
               (backlog (read-lucidplan-backlog (params rc "project")))
               (human-ids (map (lambda (t)
                                 (assoc-ref (cdr t)
                                            'human-id))
                               (assoc-ref backlog
                                          'tickets)))
               (highest-human-id (apply max human-ids))
               (new-uuid (uuidgen))
               (new-ticket `((id unquote new-uuid )
                             (human-id unquote
                                       (+ highest-human-id 1))
                             (title unquote title)
                             (description unquote description)
                             (ticket-status unquote ticket-status)))
               (new-backlog-tickets (assoc-set! (assoc-ref backlog 'tickets) new-uuid new-ticket))
               (new-backlog (assoc-set! backlog 'tickets new-backlog-tickets))
               )
          
          (newline)
          (display "===========")
          (newline)
          (display new-ticket)
          (newline)
          (display new-backlog)
          (newline)
          (display "===========")
          (newline)

          ;;(write-lucidplan-backlog (params rc "project") new-backlog)

          (redirect-to rc
                       (format #f "/backlog/~a"
                               (params rc "project"))))))

