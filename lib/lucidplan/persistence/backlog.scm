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

(define-module (lucidplan persistence backlog)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 threads)
  #:use-module (lucidplan html dsl)
  #:use-module (lucidplan html backlog)
  #:use-module (artanis route))

(define lucidplan-joe-backlog-location
  "/home/joe/Ontwikkeling/Persoonlijk/lucidplan/db/joe-backlog.scm")

(define-public (read-lucidplan-backlog project)
  (let* ((file (open-input-file lucidplan-joe-backlog-location))
         (backlog-data (eval-string (get-string-all file))))
    backlog-data))

(define-public (write-lucidplan-backlog project b)
  (with-output-to-file lucidplan-joe-backlog-location
    (lambda ()
      (display b))))

