;;; lucidplan - free and open project management for everyone

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

(define-restful-api v1)

(import (app models project)
        (app models ticket))

(get "/api/v1/tickets/:project"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (:mime rc
          ($ticket 'get
                   #:condition (where #:project_id (params rc "project"))
                   #:order-by "human_id ASC")))
       (protected-api-endpoint rc do-get)))

(get "/api/v1/projects"
     #:mime 'json
     #:auth '(table user "username" "password")
     #:session #t
     (lambda (rc)
       (define (do-get)
         (:mime rc
          ($project 'get
                    #:order-by "human_id ASC")))
       (protected-api-endpoint rc do-get)))
