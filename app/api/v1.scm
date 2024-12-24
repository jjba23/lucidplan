;; RESTful API v1 definition of lucidplan
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-restful-api v1)
; DO NOT REMOVE THIS LINE!!!

(import (lucidplan backlog))
(import (lucidplan projects))

(get "/api/v1/backlog/:project"
     #:mime 'json
     (lambda (rc)
       (:mime rc
        (get-backlog (params rc "project")))))

(get "/api/v1/projects"
     #:mime 'json
     (lambda (rc)
       (:mime rc
        (get-projects))))
