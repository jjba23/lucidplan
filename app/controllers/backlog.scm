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
        (lucidplan persistence backlog))

(get "/backlog/:project"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                       (html-from-backlog (params rc "project") (read-lucidplan-backlog (params rc "project")))))))

(get "/backlog/:project/tickets/new"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                       (html-new-ticket-form (params rc "project"))
                       ))))
