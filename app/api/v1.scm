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

(import (lucidplan persistence projects)
        (lucidplan html projects)
        (lucidplan persistence backlog)
        (lucidplan html backlog))

(get "/api/v1/backlog/:project"
     #:mime 'json
     (lambda (rc)
       (:mime rc
        (read-lucidplan-backlog (params rc "project")))))

(get "/api/v1/projects"
     #:mime 'json
     (lambda (rc)
       (:mime rc (read-lucidplan-projects))))
