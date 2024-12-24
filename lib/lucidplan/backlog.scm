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
  `((project unquote project)
    (tickets ("7b954ba2-dd82-46a7-9b2f-04d64bd72366"
              (title . "Manual for SSS - info, tips, keybindings")
              (description . "* Manual for SSS\ngenerate manual, documentation and cheatsheet for SSS easily")
              (human-id . "SSS-1")
              (id . "7b954ba2-dd82-46a7-9b2f-04d64bd72366"))
             ("727dfeed-6364-4f6c-a9c6-4990621039a0"
              (title . "Bootstrap script for SSS - easy onboarding")
              (description . "* bootstrap script for first-setup to help adoption of sss")
              (human-id . "SSS-2")
              (id . "727dfeed-6364-4f6c-a9c6-4990621039a0"))
             ("01e35983-c88d-49ab-aebd-1bb466158170"
              (title . "User factory for SSS - profiles")
              (description . "** user factory which is by default configured to joe's settings but is easily tweaked and with no duplication it creates new users.")
              (human-id . "SSS-3")
              (id . "01e35983-c88d-49ab-aebd-1bb466158170")))
    (highest-priority-tickets unquote
                              #("7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0"))
    (closed-tickets unquote #())
    (in-progress-tickets unquote #("7b954ba2-dd82-46a7-9b2f-04d64bd72366"))
    (in-review-tickets unquote #())
    (by-priority-sorting unquote
                         #("7b954ba2-dd82-46a7-9b2f-04d64bd72366" "727dfeed-6364-4f6c-a9c6-4990621039a0" "01e35983-c88d-49ab-aebd-1bb466158170"))
    (user-sorting unquote
                  #("01e35983-c88d-49ab-aebd-1bb466158170" "727dfeed-6364-4f6c-a9c6-4990621039a0" "7b954ba2-dd82-46a7-9b2f-04d64bd72366"))))

(define-public (get-backlog project)
  (some-backlog project))

(define-public (page-footer)
  `((div (@ (class "block text-center font-mono"))
         (div (@ (class
                  "mt-4 flex flex-row flex-wrap gap-4 justify-center align-center"))
              (p (@ (class "text-sm text-stone-600"))
                 "find the source code here:")
              (a (@ (class "text-orange-700 font-bold cursor-pointer text-sm")
                    (href "https://codeberg.org/jjba23/lucidplan"))
                 "https://codeberg.org/jjba23/lucidplan"))
         (p (@ (class "mt-4"))
            (em (@ (class "text-md text-stone-600"))
                "Copyright 2025 - Josep Bigorra"))
         (p (@ (class "mt-4"))
            (span (@ (class "text-sm text-stone-600"))
                  "free and open project management for everyone")))))

(define (render-ticket ticket)
  `((div (@ (class "flex flex-col gap-2 p-3 bg-stone-100 rounded-xl"))
         (h4 (@ (class "font-mono text-stone-700 text-lg"))
             ,(assoc-ref ticket
                         'title))
         (p (@ (class "font-mono text-stone-600 text-md max-w-prose"))
            ,(assoc-ref ticket
                        'description))
         (small (@ (class "font-mono text-stone-500 text-xs"))
                ,(assoc-ref ticket
                            'id)))))

(define ticket-listitem-reveal-details-js-template
  "document.getElementById('details-~a').toggleAttribute('hidden')")

(define (render-ticket-listitem ticket)
  `((div (@(draggable "true") (ondragstart "dragBacklogItem(event)") (id ,(format #f "listitem-~a"
                                                                                  (assoc-ref ticket
                                                                                             'id)))
          (class
            "flex flex-row flex-wrap gap-4 items-center justify-left bg-stone-300 border-stone-200 py-1 px-3 cursor-pointer rounded-xl")
          (onclick ,(format #f ticket-listitem-reveal-details-js-template
                            (assoc-ref ticket
                                       'id))))
         (small (@ (class "font-mono text-stone-500 text-sm"))
                ,(assoc-ref ticket
                            'human-id))
         (h5 (@ (class "font-mono text-stone-700 text-lg"))
             ,(assoc-ref ticket
                         'title)))
    (div (@ (id ,(format #f "details-~a"
                         (assoc-ref ticket
                                    'id)))
            (class "")
            (hidden ""))
         ,(render-ticket ticket))

    ))


;; <div id="div1" ondrop="drop(event)" ondragover="allowDrop(event)"></div>

;; <img id="drag1" src="img_logo.gif" draggable="true" ondragstart="drag(event)" width="336" height="69">


(define (html-from-backlog backlog)
  `(html (head (title "LucidPlan - My Backlog")
               (link (@ (rel "preconnect")
                        (href "https://fonts.googleapis.com")))
               (link (@ (rel "preconnect")
                        (href "https://fonts.gstatic.com")
                        (crossorigin "")))
               (link (@ (rel "stylesheet")
                        (href "https://fonts.cdnfonts.com/css/liberation-sans")))
               (link (@ (rel "stylesheet")
                        (href "/css/fonts.css")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1.0")))
               (script (@ (src "https://cdn.tailwindcss.com")) "")
               (script (@ (src "/js/tailwind.config.js")) ())
               (script (@ (src "/js/backlog.js")) ()))
         (body (@ (class "bg-stone-200 p-2"))
               (div (@ (class
                         "my-6 flex flex-row flex-wrap justify-center gap-2"))
                    (div (@(class "flex flex-col gap-2 p-2"))
                         (a (@(class "text-sm uppercase text-stone-600 font-bold")(href "/board/JOE")) "Board")
                         (a (@(class "text-sm uppercase text-stone-600 font-bold")(href "/backlog/JOE")) "Backlog")
                         )
                    (div (@(ondrop "dropBacklogItem(event)")(ondragover "allowDrop(event)")(class "flex flex-col gap-2 container"))
                         ,(map (lambda (t)
                                 (render-ticket-listitem (cdr t)))
                               (assoc-ref backlog
                                          'tickets)))
                    )
               (footer ,(page-footer)))))

(define-public (get-backlog-html project)
  (html-from-backlog (some-backlog project)))

