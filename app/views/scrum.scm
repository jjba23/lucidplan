;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; The pages of the plan: a project's sprints, one sprint's own page with its
;;; burndown, and the milestones its work counts toward.
;;;
;;; Nothing here computes a number that is not drawn.  A sprint's progress, the
;;; points it committed to and the line they should have fallen along all come
;;; from `(lucidplan domain)' -- `burndown-series' replays the audit trail and
;;; answers one row per day -- so the chart, the totals above it and any other
;;; reader of the same question cannot disagree.  This file decides how a
;;; number is drawn, never what the number is.

(define-module (app views scrum)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (lucidplan html)
  #:use-module (lucidplan domain)
  #:use-module (hygguile hygguile)
  #:export (html-from-sprints html-sprint-board html-from-milestones
                              render-burndown))

;; =============================================================================
;;  Small pieces the plan pages share
;; =============================================================================

(define %panel-face
  (mk-class (append '(flex flex-col gap-2 px-4 py-4 rounded-xl border-2
                           border-stone-300/75 dark:border-onyx-100/75
                           shadow-md) frosted-tint-classes)))

(define %muted-face
  (mk-class '(text-sm text-onyx-500 dark:text-stone-500)))

(define (sprint-status-label G_ status)
  "Answer the words a reader is shown for a sprint STATUS."
  (cond
    ((equal? status "planned")
     (G_ "planned"))
    ((equal? status "active")
     (G_ "active"))
    ((equal? status "closed")
     (G_ "closed"))
    (else status)))

;; A sprint's state is drawn the way its dates are read: an active sprint is
;; the one a team is in and is marked out, a closed one is what it was, and a
;; planned one is waiting.  The three are one function so the list, the board
;; and the menu that changes it all say the same colours.
(define (sprint-status-face status)
  (mk-class (append '(text-xs px-2 py-1 rounded-lg border-2 font-medium)
                    (cond
                      ((equal? status "active")
                       '(text-asparagus-700 dark:text-asparagus-300
                         border-asparagus-500/75
                         dark:border-asparagus-500/75))
                      ((equal? status "closed")
                       '(text-onyx-500 dark:text-stone-500
                         border-stone-300/75 dark:border-onyx-100/75))
                      (else
                       '(text-olive-700 dark:text-olive-300
                         border-olive-500/75 dark:border-olive-500/75))))))

(define (sprint-dates-label G_ sprint)
  "Answer the range a sprint runs over, or the sentence that it has none."
  (let ((starts (assq-ref sprint
                           'starts-on))
        (ends (assq-ref sprint
                         'ends-on)))
    (cond
      ((and starts ends) (format #f "~a → ~a" starts ends))
      (starts (format #f "~a →" starts))
      (ends (format #f "→ ~a" ends))
      (else (G_ "no dates yet")))))

(define (ticket-points-of ticket)
  "Answer the story points TICKET is estimated at, counting none as zero.

The same reading the burndown makes, said here so a row's number and the
chart's total are the same number."
  (let ((points (assq-ref ticket
                          'estimate-points)))
    (if (number? points) points 0)))

(define (points-of tickets)
  "Answer the points TICKETS add up to."
  (apply + 0
         (map ticket-points-of tickets)))

;; A bar says how much of a scope is done without a reader having to subtract
;; two numbers.  It is a filled div inside a track rather than an element the
;; browser draws, because the two themes want the same bar in different
;; colours and the stylesheet already knows both.
(define (render-progress-bar G_ completed scope)
  (let* ((percent (if (and (number? scope)
                           (> scope 0))
                      (min 100
                           (max 0
                                (round (* 100 (/ completed scope)))))
                      0)))
    `(div (@ (class ,(mk-class '(w-full h-2 rounded-full
                                        border-2 border-stone-300/75
                                        dark:border-onyx-100/75)))
             (role "progressbar")
             (aria-valuenow ,(format #f "~a" percent))
             (aria-valuemin "0")
             (aria-valuemax "100")
             (aria-label ,(G_ "how much of the sprint is done")))
          (div (@ (class ,(mk-class '(h-2 rounded-full
                                             bg-asparagus-500)))
                  (style ,(format #f "width: ~a%" percent)))))))

;; =============================================================================
;;  The burndown
;; =============================================================================

(define (axis-label value)
  "Answer VALUE as the short number an axis is labelled with.

A peak that is not a whole number -- thirteen points whose half is six and
a half -- would otherwise be labelled with the exact fraction it is
computed as, which is a number a reader has to do arithmetic on."
  (let ((n (exact->inexact value)))
    (if (= n (round n))
        (format #f "~a" (inexact->exact (round n)))
        (format #f "~,1f" n))))

;; A burndown is a chart a reader glances at, so it is drawn as one: the points
;; left over the days of the sprint, and the straight line the team committed
;; to, so the gap between them says what a table of the same numbers would take
;; a minute to say.
;;
;; It is drawn as SVG in the page rather than as an image or a script: the
;; numbers are already on the server, a picture cannot be read by a screen
;; reader, and a chart that needs JavaScript is a chart that is missing for
;; somebody.  The colours are `currentColor' over theme classes, so the same
;; markup is legible in both themes, and every coordinate is a number computed
;; here -- nothing about the shape depends on a stylesheet arriving.
(define* (render-burndown G_ rows #:key (width 640) (height 240))
  "Answer the burndown ROWS as a chart, or the sentence that none can be drawn.

Each row is a day of the sprint, as `burndown-series' answers them.  A sprint
with no dates, or one whose first day is still in the future, has no days to
draw -- and a chart of nothing is worse than a sentence saying so."
  (cond
    ((null? rows)
     `((p (@ (class ,%muted-face))
          ,(G_
            "a burndown needs a sprint with dates: give the sprint a start and an end and it is drawn here"))))
    (else (let* ((count (length rows))
                 (peak (max 1
                            (apply max
                                   (append (map (lambda (row)
                                                  (or (assq-ref row
                                                                'scope) 0))
                                                rows)
                                           (map (lambda (row)
                                                  (or (assq-ref row
                                                                'remaining) 0))
                                                rows)
                                           (map (lambda (row)
                                                  (or (assq-ref row
                                                                'ideal) 0))
                                                rows)))))
                 ;; Room for the numbers down the side and the days along the
                 ;; bottom: a chart whose axis labels are cut off is a chart
                 ;; nobody can read a value from.
                 (left 48)
                 (right 16)
                 (top 16)
                 (bottom 44)
                 (plot-width (- width left right))
                 (plot-height (- height top bottom))
                 (x-of (lambda (index)
                         (if (<= count 1) (+ left (/ plot-width 2))
                             (+ left
                                (* index (/ plot-width (- count 1)))))))
                 (y-of (lambda (value)
                         (- (+ top plot-height)
                            (* (/ (max 0 value) peak) plot-height))))
                 (line-points (lambda (key)
                                (string-join (map (lambda (row index)
                                                    (format #f "~,1f,~,1f"
                                                            (exact->inexact
                                                             (x-of index))
                                                            (exact->inexact
                                                             (y-of (or (assq-ref
                                                                        row
                                                                        key)
                                                                       0)))))
                                                  rows (iota count))
                                             " ")))
                 (first-day (assq-ref (car rows)
                                      'day))
                 (last-day (assq-ref (car (reverse rows))
                                     'day)))
            `((svg (@ (viewBox ,(format #f "0 0 ~a ~a" width height))
                       (class ,(mk-class '(w-full)))
                       (role "img")
                       (aria-label ,(format #f "~a: ~a → ~a"
                                           (G_ "burndown")
                                           first-day last-day)))
                   ;; Three grid lines: nothing left, half, and the peak the
                   ;; sprint was given.  More would be a grid rather than a
                   ;; chart, and the peak is the only height that means
                   ;; anything.
                   ,@(map (lambda (fraction)
                            (let* ((value (* peak fraction))
                                   (y (exact->inexact (y-of value))))
                              `(g ()
                                   (line (@ (x1 ,(format #f "~a" left))
                                            (y1 ,(format #f "~,1f" y))
                                            (x2 ,(format #f "~a"
                                                         (- width right)))
                                            (y2 ,(format #f "~,1f" y))
                                            (stroke "currentColor")
                                            (stroke-opacity "0.25")
                                            (stroke-width "1")
                                            (class ,(mk-class '(text-onyx-500
                                                                dark:text-stone-500)))))
                                   (text (@ (x ,(format #f "~a"
                                                        (- left 6)))
                                            (y ,(format #f "~,1f" (+ y 4)))
                                            (text-anchor "end")
                                            (font-size "11")
                                            (fill "currentColor")
                                            (class ,(mk-class '(text-onyx-500
                                                                dark:text-stone-500))))
                                         ,(axis-label value)))))
                          '(0 1/2 1))
                   ;; The committed line, drawn under the measured one: where
                   ;; the team should have been is context, and where it is is
                   ;; the answer.
                   (polyline (@ (points ,(line-points 'ideal))
                                (fill "none")
                                (stroke "currentColor")
                                (stroke-width "2")
                                (stroke-dasharray "6 4")
                                (stroke-linecap "round")
                                (class ,(mk-class '(text-onyx-500
                                                    dark:text-stone-500)))))
                   ;; The measured line: what was left at the end of each day.
                   (polyline (@ (points ,(line-points 'remaining))
                                (fill "none")
                                (stroke "currentColor")
                                (stroke-width "3")
                                (stroke-linecap "round")
                                (stroke-linejoin "round")
                                (class ,(mk-class '(text-asparagus-500)))))
                   ;; A dot per day, so a two-day sprint still reads as two
                   ;; measurements rather than as one long line.
                   ,@(map (lambda (row index)
                            (let ((remaining (or (assq-ref row
                                                           'remaining) 0)))
                              `(circle (@ (cx ,(format #f "~,1f"
                                                       (exact->inexact
                                                        (x-of index))))
                                          (cy ,(format #f "~,1f"
                                                       (exact->inexact
                                                        (y-of remaining))))
                                          (r "3")
                                          (fill "currentColor")
                                          (class ,(mk-class '(text-asparagus-500)))))))
                          rows (iota count))
                   ;; The two ends of the axis: the day it began and the day
                   ;; it is being read on, which is where the chart stops.
                   (text (@ (x ,(format #f "~a" left))
                            (y ,(format #f "~a" (- height 12)))
                            (text-anchor "start")
                            (font-size "11")
                            (fill "currentColor")
                            (class ,(mk-class '(text-onyx-500
                                                dark:text-stone-500))))
                         ,first-day)
                   (text (@ (x ,(format #f "~a" (- width right)))
                            (y ,(format #f "~a" (- height 12)))
                            (text-anchor "end")
                            (font-size "11")
                            (fill "currentColor")
                            (class ,(mk-class '(text-onyx-500
                                                dark:text-stone-500))))
                         ,last-day)))))))

;; The three numbers the chart is read as a whole by, and what the two lines
;; mean.  The legend is markup rather than part of the picture, because a
;; dashed line and a solid one are a difference a colour-blind reader cannot
;; see and a sentence can always say.
(define (render-burndown-summary G_ summary rows)
  (let ((scope (or (assq-ref summary
                             'scope) 0))
        (completed (or (assq-ref summary
                                 'completed) 0))
        (remaining (or (assq-ref summary
                                 'remaining) 0)))
    `((div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-6
                                         items-baseline my-2))))
           (div (@ (class ,(mk-class '(flex flex-col))))
                (small (@ (class ,%muted-face))
                       ,(G_ "committed"))
                (strong (@ (class ,(mk-class '(text-2xl))))
                        ,(format #f "~a" scope)))
           (div (@ (class ,(mk-class '(flex flex-col))))
                (small (@ (class ,%muted-face))
                       ,(G_ "done"))
                (strong (@ (class ,(mk-class '(text-2xl
                                               text-asparagus-700
                                               dark:text-asparagus-300))))
                        ,(format #f "~a" completed)))
           (div (@ (class ,(mk-class '(flex flex-col))))
                (small (@ (class ,%muted-face))
                       ,(G_ "left"))
                (strong (@ (class ,(mk-class '(text-2xl))))
                        ,(format #f "~a" remaining))))
      ,(if (pair? rows)
           (render-progress-bar G_ completed scope) '())
      (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4 my-1))))
           (span (@ (class ,%muted-face))
                 ,(G_ "— what is left"))
           (span (@ (class ,%muted-face))
                 ,(G_ "- - the line the team committed to"))))))

;; =============================================================================
;;  The pages
;; =============================================================================

(define (render-empty G_ text)
  `((p (@ (class ,%muted-face))
       ,text)))

;; One sprint as the list shows it: its name and state, the stretch of days it
;; runs over, how much of it is done, and the controls that move it along.
;; Every control is a form that posts, so a sprint is started or closed without
;; a script having to run, and each is offered only to a reader the endpoint
;; behind it would accept.
(define* (render-sprint-row G_
                            #:key
                            project
                            sprint
                            tickets
                            (statuses (default-ticket-statuses))
                            (today #f)
                            (editable? #f))
  (let* ((sprint-id (format #f "~a"
                            (assq-ref sprint
                                      'id)))
         (status (or (assq-ref sprint
                               'sprint-status) "planned"))
         (mine (filter (lambda (t)
                         (equal? (format #f "~a"
                                         (assq-ref t
                                                   'sprint-id))
                                 sprint-id))
                       tickets))
         (scope (points-of mine))
         (done (points-of (filter (lambda (t)
                                    (ticket-status-in-bucket?
                                     statuses (assq-ref t
                                                        'ticket-status)
                                     "done"))
                                  mine))))
    `(div (@ (class ,%panel-face))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            items-baseline justify-between))))
               (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                 items-baseline))))
                    (a (@ (href ,(format #f
                                  "/projects/~a/sprints/~a" project
                                  sprint-id))
                          (class ,(mk-class '(text-md font-bold
                                                     no-underline
                                                     text-onyx-700
                                                     dark:text-stone-300))))
                       ,(assq-ref sprint
                                   'title))
                    (span (@ (class ,(sprint-status-face status)))
                          ,(sprint-status-label G_ status)))
               (small (@ (class ,%muted-face))
                      ,(sprint-dates-label G_ sprint)))
          ,@(if (string-null? (let ((goal (assq-ref sprint
                                                    'goal)))
                                (if (string? goal) goal "")))
                '()
                `((p (@ (class ,(mk-class '(text-sm max-w-prose))))
                     ,(assq-ref sprint
                                 'goal))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4
                                            items-baseline))))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "tickets") (length mine)))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "points") scope))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "done") done)))
          ,(render-progress-bar G_ done scope)
          ,@(if editable?
                `((div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                    my-1))))
                        ,@(if (equal? status "planned")
                              `((form (@ (enctype
                                          "application/x-www-form-urlencoded")
                                         (method POST)
                                         (action ,(format #f
                                                   "/projects/~a/sprints/~a/status"
                                                   project sprint-id)))
                                      (input (@ (hidden "")
                                                (name "sprint-status")
                                                (value "active")))
                                      ,(submit-button (G_ "start"))))
                              '())
                        ,@(if (equal? status "active")
                              `((form (@ (enctype
                                          "application/x-www-form-urlencoded")
                                         (method POST)
                                         (action ,(format #f
                                                   "/projects/~a/sprints/~a/status"
                                                   project sprint-id)))
                                      (input (@ (hidden "")
                                                (name "sprint-status")
                                                (value "closed")))
                                      ,(submit-button (G_ "close"))))
                              '())
                        ,@(if (equal? status "closed")
                              `((form (@ (enctype
                                          "application/x-www-form-urlencoded")
                                         (method POST)
                                         (action ,(format #f
                                                   "/projects/~a/sprints/~a/status"
                                                   project sprint-id)))
                                      (input (@ (hidden "")
                                                (name "sprint-status")
                                                (value "planned")))
                                      ,(submit-button (G_ "reopen"))))
                              '())
                        (form (@ (enctype "application/x-www-form-urlencoded")
                                 (method POST)
                                 (action ,(format #f
                                           "/projects/~a/sprints/~a/delete"
                                           project sprint-id)))
                              ,(danger-submit-button (G_ "delete")))))
                '()))))

(define* (html-from-sprints G_
                            #:key
                            (projects '())
                            project
                            (sprints '())
                            (tickets '())
                            (statuses (default-ticket-statuses))
                            (today #f)
                            (editable? #f)
                            (is-logged-in #f))
  (html-template G_
                 #:title (G_ "sprints")
                 #:canonical-path (format #f "/projects/~a/sprints" project)
                 #:is-logged-in is-logged-in
                 #:proj projects
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4 flex flex-col
                                                           gap-3))))
                              (a (@ (href ,(format #f "/tickets/~a" project))
                                    (class ,(mk-class '(text-md px-2
                                                               no-underline))))
                                 ,(format #f "← back to backlog: ~a" project))
                              (h2 (@ (class ,(mk-class '(text-lg font-bold))))
                                  ,(G_ "sprints"))
                              (p (@ (class ,%muted-face))
                                 ,(G_
                                   "a sprint is a stretch of work a team commits to. The kanban board of a sprint is the board of its tickets, and the burndown of a sprint is drawn from the audit trail the tickets already leave."))
                              ;; The create form is offered to the same reader
                              ;; the endpoint behind it accepts: holding the
                              ;; permission, not merely being signed in.
                              ,@(if editable?
                                    `((div (@ (class ,%panel-face))
                                           (form (@ (enctype
                                                     "application/x-www-form-urlencoded")
                                                    (method POST)
                                                    (action ,(format #f
                                                              "/projects/~a/sprints"
                                                              project))
                                                    (class ,(mk-class '(flex
                                                                        flex-col
                                                                        gap-2))))
                                                 ,(input #:label (G_ "what it is called")
                                                         #:name "title"
                                                         #:required #t)
                                                 ,(textarea #:label (G_ "what it is for")
                                                            #:name "goal"
                                                            #:rows 2
                                                            #:textarea-css '(leading-7
                                                                             p-3
                                                                             rounded-xl
                                                                             border-stone-700
                                                                             dark:border-onyx-100
                                                                             dark:text-stone-300
                                                                             dark:bg-onyx-700/75
                                                                             bg-stone-300/75
                                                                             border-2
                                                                             min-h-24
                                                                             w-full
                                                                             max-w-prose
                                                                             shadow-md))
                                                 (div (@ (class ,(mk-class '(flex
                                                                             flex-row
                                                                             flex-wrap
                                                                             gap-2
                                                                             items-end))))
                                                      (div ,(date-input (G_
                                                                         "starts on")
                                                                        "starts-on"))
                                                      (div ,(date-input (G_
                                                                         "ends on")
                                                                        "ends-on")))
                                                 ,(submit-button (G_
                                                                  "plan a sprint")))))
                                    '())
                              (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                                   ,@(if (pair? sprints)
                                         (map (lambda (sprint)
                                                (render-sprint-row G_
                                                                   #:project
                                                                   project
                                                                   #:sprint
                                                                   sprint
                                                                   #:tickets
                                                                   tickets
                                                                   #:statuses
                                                                   statuses
                                                                   #:today today
                                                                   #:editable?
                                                                   editable?))
                                              sprints)
                                         (render-empty G_
                                                       (G_
                                                        "no sprints yet: a project between sprints is a backlog with nowhere to pour it")))))))

;; The board of one sprint: what is in it, what is left, and the line it should
;; be on.  The backlog beside it is what makes the page useful rather than a
;; report -- a sprint nobody can add work to is a page about the past.
;;
;; The statuses a ticket is drawn with are the project's own ladder, so a
;; project that renamed or added a status sees its own words here, and a
;; ticket is counted as done by the same rule the backlog counts it.
;; One row of a ticket list on the board, with whatever control comes with it:
;; a ticket in the sprint can be taken out of it, and one in the backlog can be
;; put in.  Both lists draw their rows the same way, so a ticket looks the same
;; whichever side of the page it is on -- and the row is a procedure rather than
;; two nearly-identical quasiquotes, which is what keeps them from drifting
;; apart (and what keeps this body shallow enough to read).
(define %board-row-face
  (mk-class '(flex flex-row flex-wrap gap-2 items-center justify-between
                   border-2 border-stone-300/50 dark:border-onyx-100/50
                   rounded-lg px-3 py-2 my-1)))

(define %board-row-main-face
  (mk-class '(flex flex-row flex-wrap gap-2 items-center)))

(define* (render-board-row G_
                           #:key
                           project
                           ticket
                           statuses
                           (actions '()))
  (let ((human-id (assq-ref ticket
                            'human-id)))
    `(div (@ (class ,%board-row-face))
          (div (@ (class ,%board-row-main-face))
               (a (@ (href ,(format #f "/tickets/~a/id/~a" project human-id))
                     (class ,(mk-class '(text-sm no-underline))))
                  ,(assq-ref ticket
                             'title))
               (span (@ (class ,%muted-face))
                     ,(status-label G_ statuses
                                    (or (assq-ref ticket
                                                  'ticket-status) "")))
               (span (@ (class ,%muted-face))
                     ,(format #f "~a: ~a" (G_ "points")
                              (ticket-points-of ticket))))
          ,@actions)))

;; Putting a ticket in or taking it out is one form with the direction in it,
;; because the endpoint behind it is one endpoint.  The ticket is named by its
;; own id rather than by its number, since a number only means something inside
;; one project.
(define* (render-board-ticket-form G_
                                   #:key
                                   project
                                   sprint-id
                                   ticket
                                   direction
                                   label
                                   danger?)
  `(form (@ (enctype "application/x-www-form-urlencoded")
             (method POST)
             (action ,(format #f "/projects/~a/sprints/~a/tickets" project
                              sprint-id)))
         (input (@ (hidden "")
                   (name "ticket")
                   (value ,(assq-ref ticket
                                     'id))))
         (input (@ (hidden "")
                   (name "action")
                   (value ,direction)))
         ,(if danger?
              (danger-submit-button (G_ label))
              (submit-button (G_ label)))))

;; The board of one sprint: what is in it, what is left, and the line it should
;; be on.  The backlog beside it is what makes the page useful rather than a
;; report -- a sprint nobody can add work to is a page about the past.
;;
;; The statuses a ticket is drawn with are the project's own ladder, so a
;; project that renamed or added a status sees its own words here, and a ticket
;; is counted as done by the same rule the backlog counts it.
(define* (html-sprint-board G_
                            #:key
                            (projects '())
                            project
                            sprint
                            (tickets '())
                            (backlog '())
                            (statuses (default-ticket-statuses))
                            (rows '())
                            summary
                            (today #f)
                            (editable? #f)
                            (is-logged-in #f))
  (let* ((sprint-id (format #f "~a"
                            (assq-ref sprint
                                      'id)))
         (status (or (assq-ref sprint
                               'sprint-status) "planned"))
         (goal (let ((text (assq-ref sprint
                                     'goal)))
                 (if (string? text) text "")))
         (ticket-row (lambda (ticket)
                       (render-board-row G_
                                         #:project project
                                         #:ticket ticket
                                         #:statuses statuses
                                         #:actions (if editable?
                                                       (list (render-board-ticket-form
                                                              G_
                                                              #:project project
                                                              #:sprint-id
                                                              sprint-id
                                                              #:ticket ticket
                                                              #:direction
                                                              "remove"
                                                              #:label "take out"
                                                              #:danger? #t))
                                                       '()))))
         ;; The three panels, each built on its own and then spliced into the
         ;; page: a body of nested quasiquotes two hundred lines deep is a body
         ;; nobody can count the parentheses of, and this one has the panel
         ;; boundaries to say where each ends.
         (burndown-panel `(div (@ (class ,%panel-face))
                               ,(h3 (G_ "burndown"))
                               ,@(render-burndown-summary G_ summary rows)
                               ,@(render-burndown G_ rows)))
         (tickets-panel `(div (@ (class ,%panel-face))
                              ,(h3 (format #f "~a (~a)"
                                           (G_ "in this sprint")
                                           (length tickets)))
                              ,@(if (pair? tickets)
                                    (map ticket-row tickets)
                                    (render-empty G_
                                                  (G_
                                                   "nothing in this sprint yet: add a ticket from the backlog below")))))
         (backlog-panel (if (and editable?
                                 (pair? backlog))
                            `(div (@ (class ,%panel-face))
                                  ,(h3 (format #f "~a (~a)"
                                               (G_ "backlog")
                                               (length backlog)))
                                  ,@(map (lambda (ticket)
                                           (render-board-row G_
                                                             #:project project
                                                             #:ticket ticket
                                                             #:statuses statuses
                                                             #:actions (list (render-board-ticket-form
                                                                              G_
                                                                              #:project
                                                                              project
                                                                              #:sprint-id
                                                                              sprint-id
                                                                              #:ticket
                                                                              ticket
                                                                              #:direction
                                                                              "add"
                                                                              #:label
                                                                              "add"
                                                                              #:danger?
                                                                              #f))))
                                         backlog)
                            '()))))
    (html-template G_
                   #:title (assq-ref sprint
                                     'title)
                   #:canonical-path (format #f "/projects/~a/sprints/~a" project
                                            sprint-id)
                   #:is-logged-in is-logged-in
                   #:proj projects
                   #:body `(div (@ (class ,(mk-class '(px-4 py-4 flex flex-col
                                                             gap-3))))
                                (a (@ (href ,(format #f
                                              "/projects/~a/sprints" project))
                                      (class ,(mk-class '(text-md px-2
                                                                 no-underline))))
                                   ,(format #f "← ~a" (G_ "all sprints")))
                                (div (@ (class ,(mk-class '(flex flex-row
                                                                 flex-wrap
                                                                 gap-2
                                                                 items-baseline))))
                                     (h2 (@ (class ,(mk-class '(text-lg
                                                                font-bold))))
                                         ,(assq-ref sprint
                                                     'title))
                                     (span (@ (class ,(sprint-status-face
                                                       status)))
                                           ,(sprint-status-label G_ status)))
                                (p (@ (class ,%muted-face))
                                   ,(sprint-dates-label G_ sprint))
                                ,@(if (string-null? goal)
                                      '()
                                      (list `(p (@ (class ,(mk-class '(text-sm
                                                                       max-w-prose))))
                                                ,goal)))
                                ,burndown-panel
                                ,tickets-panel
                                ,@(if (pair? backlog-panel)
                                      (list backlog-panel)
                                      '())))))

;; A milestone is a promise made outside the team, so the page is a list of
;; promises and how much of each is kept: the tickets that count toward it,
;; the points they add up to and how many of those are done.  A milestone's own
;; date is drawn the way a deadline is, because it is one.
(define* (render-milestone-row G_
                               #:key
                               project
                               milestone
                               tickets
                               (statuses (default-ticket-statuses))
                               (today #f)
                               (editable? #f))
  (let* ((milestone-id (format #f "~a"
                             (assq-ref milestone
                                       'id)))
         (mine (filter (lambda (t)
                         (equal? (format #f "~a"
                                         (assq-ref t
                                                   'milestone-id))
                                 milestone-id))
                       tickets))
         (scope (points-of mine))
         (done (points-of (filter (lambda (t)
                                    (ticket-status-in-bucket?
                                     statuses (assq-ref t
                                                        'ticket-status)
                                     "done"))
                                  mine))))
    `(div (@ (class ,%panel-face))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            items-baseline justify-between))))
               (strong (@ (class ,(mk-class '(text-md))))
                       ,(assq-ref milestone
                                   'title))
               ,@(render-due-badge G_ (assq-ref milestone
                                                 'due-on) today))
          ,@(if (string-null? (let ((text (assq-ref milestone
                                                     'description)))
                                (if (string? text) text "")))
                '()
                `((p (@ (class ,(mk-class '(text-sm max-w-prose))))
                     ,(assq-ref milestone
                                 'description))))
          (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4
                                            items-baseline))))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "tickets") (length mine)))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "points") scope))
               (small (@ (class ,%muted-face))
                      ,(format #f "~a: ~a" (G_ "done") done)))
          ,(render-progress-bar G_ done scope)
          ,@(if editable?
                `((form (@ (enctype "application/x-www-form-urlencoded")
                           (method POST)
                           (action ,(format #f
                                     "/projects/~a/milestones/~a/delete"
                                     project milestone-id)))
                        ,(danger-submit-button (G_ "delete"))))
                '()))))

(define* (html-from-milestones G_
                               #:key
                               (projects '())
                               project
                               (milestones '())
                               (tickets '())
                               (statuses (default-ticket-statuses))
                               (today #f)
                               (editable? #f)
                               (is-logged-in #f))
  (html-template G_
                 #:title (G_ "milestones")
                 #:canonical-path (format #f "/projects/~a/milestones"
                                          project)
                 #:is-logged-in is-logged-in
                 #:proj projects
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4 flex flex-col
                                                           gap-3))))
                              (a (@ (href ,(format #f "/tickets/~a" project))
                                    (class ,(mk-class '(text-md px-2
                                                               no-underline))))
                                 ,(format #f "← back to backlog: ~a" project))
                              (h2 (@ (class ,(mk-class '(text-lg font-bold))))
                                  ,(G_ "milestones"))
                              (p (@ (class ,%muted-face))
                                 ,(G_
                                   "a milestone is a result the project promised by a date. Tickets count toward it, and a sprint may feed more than one."))
                              ,@(if editable?
                                    `((div (@ (class ,%panel-face))
                                           (form (@ (enctype
                                                     "application/x-www-form-urlencoded")
                                                    (method POST)
                                                    (action ,(format #f
                                                              "/projects/~a/milestones"
                                                              project))
                                                    (class ,(mk-class '(flex
                                                                        flex-col
                                                                        gap-2))))
                                                 ,(input #:label (G_ "what it is")
                                                         #:name "title"
                                                         #:required #t)
                                                 ,(textarea #:label (G_ "what it means")
                                                            #:name "description"
                                                            #:rows 2
                                                            #:textarea-css '(leading-7
                                                                             p-3
                                                                             rounded-xl
                                                                             border-stone-700
                                                                             dark:border-onyx-100
                                                                             dark:text-stone-300
                                                                             dark:bg-onyx-700/75
                                                                             bg-stone-300/75
                                                                             border-2
                                                                             min-h-24
                                                                             w-full
                                                                             max-w-prose
                                                                             shadow-md))
                                                 (div ,(date-input (G_ "due on")
                                                                   "due-on"))
                                                 ,(submit-button (G_
                                                                  "add a milestone")))))
                                    '())
                              (div (@ (class ,(mk-class '(flex flex-col gap-3))))
                                   ,@(if (pair? milestones)
                                         (map (lambda (milestone)
                                                (render-milestone-row G_
                                                                      #:project
                                                                      project
                                                                      #:milestone
                                                                      milestone
                                                                      #:tickets
                                                                      tickets
                                                                      #:statuses
                                                                      statuses
                                                                      #:today today
                                                                      #:editable?
                                                                      editable?))
                                              milestones)
                                         (render-empty G_
                                                       (G_
                                                        "no milestones yet: a date the project promised is what this page keeps")))))))
