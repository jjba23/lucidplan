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
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 threads)
  #:use-module (artanis route))

(define (some-backlog project)
  `((project . ,project)
    (tickets . (("7b954ba2-dd82-46a7-9b2f-04d64bd72366" . ((title . "Manual for SSS - info, tips, keybindings")
                                                           (description . "* Manual for SSS\ngenerate manual, documentation and cheatsheet for SSS easily")))
                ("727dfeed-6364-4f6c-a9c6-4990621039a0" . ((title . "Bootstrap script for SSS - easy onboarding")
                                                           (description . "create simple bootstrap script for first-setup to help adoption of sss")))
                ("01e35983-c88d-49ab-aebd-1bb466158170" . ((title . "User factory for SSS - profiles")
                                                           (description . "**** create a user factory which is by default configured to joe's settings but is easily tweaked and with no duplication it creates new users.")))
                ))
    (highest-priority-tickets . ,#("7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0"))
    (closed-tickets . ,#())
    (in-progress-tickets . ,#("7b954ba2-dd82-46a7-9b2f-04d64bd72366" ))
    (in-review-tickets . ,#())
    (by-priority-sorting . ,#("7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0" "01e35983-c88d-49ab-aebd-1bb466158170"))
    (user-sorting . ,#("01e35983-c88d-49ab-aebd-1bb466158170" "727dfeed-6364-4f6c-a9c6-4990621039a0" "7b954ba2-dd82-46a7-9b2f-04d64bd72366"))
    ))

(define-public (get-backlog project)
  (some-backlog project)
  )



