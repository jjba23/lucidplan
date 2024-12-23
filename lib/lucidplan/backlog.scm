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

(define-module (lucidplan backlog)
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
  #:use-module (ice-9 threads)
  #:use-module (artanis route))

(define t
  `((title . "Manual for SSS - keybindings, tips")))

(define tt
  `((title . "Bootstrap script for SSS")))

(define (some-backlog project)
  `((project . ,project)
    (tickets . ,#(("7b954ba2-dd82-46a7-9b2f-04d64bd72366" . t)
                  ("727dfeed-6364-4f6c-a9c6-4990621039a0" . t)
                  )
             )
    (sorting . ,#("7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0"))
    ))

(define-public (get-backlog project)
  (some-backlog project)
  )


;; ("" . ((title . "")))
