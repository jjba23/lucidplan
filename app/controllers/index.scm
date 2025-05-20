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
        (app views index))

(get "/"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names sid)
     (lambda (rc)
       (response-emit (:mime rc
                             (welcome-page (:i18n rc)
                                           #:is-logged-in (:cookies-value rc
                                                           "sid"))))))
