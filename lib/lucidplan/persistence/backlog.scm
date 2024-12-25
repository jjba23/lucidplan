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

(define joe-backlog
  `((project unquote "JOE")
    (tickets ("e263a0f1-77fa-45d6-bfdf-1d63c359b9bb"
              (title . "LucidPlan proof-of-concept development")
              (description . "Develop a nice and hackable project management tool that is hackable and free and open for everyone.")
              (human-id . 4)
              (ticket-status . work-in-progress)
              (id . "e263a0f1-77fa-45d6-bfdf-1d63c359b9bb"))
             ("7b954ba2-dd82-46a7-9b2f-04d64bd72366"
              (title . "Manual for SSS - info, tips, keybindings")
              (description . "* Manual for SSS\ngenerate manual, documentation and cheatsheet for SSS easily")
              (human-id . 1)
              (ticket-status . work-in-progress)
              (id . "7b954ba2-dd82-46a7-9b2f-04d64bd72366"))
             ("727dfeed-6364-4f6c-a9c6-4990621039a0"
              (title . "Bootstrap script for SSS - easy onboarding")
              (description . "* bootstrap script for first-setup to help adoption of sss")
              (human-id . 2)
              (ticket-status . todo)
              (id . "727dfeed-6364-4f6c-a9c6-4990621039a0"))
             ("01e35983-c88d-49ab-aebd-1bb466158170"
              (title . "User factory for SSS - profiles")
              (description . "** user factory which is by default configured to joe's settings but is easily tweaked and with no duplication it creates new users.")
              (human-id . 3)
              (ticket-status . todo)
              (id . "01e35983-c88d-49ab-aebd-1bb466158170")))
    (highest-priority-tickets unquote
                              #("e263a0f1-77fa-45d6-bfdf-1d63c359b9bb" "7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0"))
    (closed-tickets unquote #())
    (in-progress-tickets unquote #("7b954ba2-dd82-46a7-9b2f-04d64bd72366"))
    (in-review-tickets unquote #())
    (by-priority-sorting unquote
                         #("e263a0f1-77fa-45d6-bfdf-1d63c359b9bb" "7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0" "01e35983-c88d-49ab-aebd-1bb466158170"))
    (user-sorting unquote
                  #("e263a0f1-77fa-45d6-bfdf-1d63c359b9bb" "01e35983-c88d-49ab-aebd-1bb466158170" "727dfeed-6364-4f6c-a9c6-4990621039a0" "7b954ba2-dd82-46a7-9b2f-04d64bd72366"))))

(define-public (lucidplan-backlog project) joe-backlog)


