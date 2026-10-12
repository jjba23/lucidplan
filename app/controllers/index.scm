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

(define-artanis-controller index)

(import (lucidplan html)
        (web uri)
        (ice-9 string-fun)
        (lucidplan sqlite project)
        (lucidplan domain project)
        (app views index)
        (app views error))

;; The pages the server answers a refusal with are not routes -- an unmatched
;; path and a handler that threw never reach one -- so they are registered
;; with the framework here, while the application is being loaded.  See
;; `app/views/error.scm' for the pages themselves.
(register-error-pages!)

(get "/"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (proj (get-projects project-repo)))
         (response-emit (:mime rc
                               (welcome-page (:i18n rc)
                                             #:projects proj
                                             #:is-logged-in (:cookies-value rc
                                                             "x-bewaking-lucidplan")))))))

(get "/robots.txt" static-page-emitter)
(get "/favicon.ico" static-page-emitter)
