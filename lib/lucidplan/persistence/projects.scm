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

(define-module (lucidplan persistence projects)
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
  #:use-module (lucidplan html projects)
  #:use-module (artanis route))

(define-public (lucidplan-projects)
  `((projects ("e8351c99-22a0-4e94-bd55-f6e04fe0e46c"
               (id . "e8351c99-22a0-4e94-bd55-f6e04fe0e46c")
               (human-id . "JOE")
               (title . "Joe's Projects"))
              ("0e9f28c2-788f-49f3-811f-d463b79fbd8d"
               (id . "0e9f28c2-788f-49f3-811f-d463b79fbd8d")
               (human-id . "ET")
               (title . "Engagement Team")))))
