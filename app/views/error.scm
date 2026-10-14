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

(define-module (app views error)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (ice-9 i18n)
  #:use-module (hygguile hygguile)
  #:use-module (hygguile sxml)
  #:use-module (kracht prelude)
  #:use-module (lucidplan html)
  #:export (error-page error-page-html register-error-pages!))

;; =============================================================================
;;  The pages a refusal is read on
;; =============================================================================

;; A status is a code a client understands, and a reader is not a client:
;; "404" on its own says nothing to the person who followed a link.  So each
;; refusal is written down here the way it is met -- a glyph to catch the eye,
;; the tone it is drawn in, a headline that says what happened in the reader's
;; own terms, and the sentence or two that says what to do about it.
;;
;; The tones are the four accents the rest of the application is built from,
;; used for what they already mean there: the calm green of a thing that is
;; fine, the blue of a door to come in through, plain ink for nothing at all,
;; and the red the framework already marks an error with.
(define error-notes
  '((400 plugs
     (text-asparagus-700 dark:text-asparagus-300)
     "that request did not arrive whole"
     "Something about the request LucidPlan received did not add up, so it was left unread and nothing was changed. Going back a page and trying again usually settles it.")
    (401 lock-key
     (text-jeans-blue-700 dark:text-jeans-blue-300)
     "this page is for signed-in readers"
     "Projects stay behind a sign-in, and the session this request arrived with is missing, has aged out, or was never recognised. Sign in again and your projects will be waiting.")
    (404 magnifying-glass
     (text-onyx-500 dark:text-stone-300) "there is nothing at this address"
     "The page you asked for is not here. It may have been moved or renamed, or the link that brought you here may be out of date. Everything else is a click away.")
    ;; A request kept so long that the server gave up on it is a refusal the
    ;; framework raises itself, and the page it answers with is empty: a
    ;; reader who waited gets nothing at all unless there is a page here.
    (408 arrow-clockwise
     (text-asparagus-700 dark:text-asparagus-300)
     "that request took too long to arrive"
     "The request was still on its way when LucidPlan stopped waiting for it, so it was left unread and nothing was changed. Sending it again usually settles it.")
    (500 warning
     (text-redwood-700 dark:text-redwood-300)
     "something came loose on our side"
     "LucidPlan hit an error while handling that page and could not finish it. The error has been written to the server's log; nothing you had open was changed. Trying again in a moment often works.")))

(define (error-note status)
  "Answer the @code{(glyph tone headline explanation)} a @var{status} is read
with.

The table is not a list of the statuses the framework can answer with -- it
is the list of refusals that mean something particular to a reader.  A status
without a line of its own is still rendered as the class of refusal it belongs
to rather than as a blank page, so a page asked for a 403 or a 503 is written
in the same voice as the ones that are spelled out.  Anything below 400 is not
a refusal at all and is read as a server error, because that is what a page
rendered for it would mean."
  (let ((row (assv status error-notes)))
    (cond
      (row (cdr row))
      ((and (>= status 400)
            (< status 500))
       (cdr (assv 400 error-notes)))
      (else (cdr (assv 500 error-notes))))))

(define* (error-page G_ status)
  "The page a request refused with @var{status} is answered with.

The page is the application's own: the same frosted card, the same header
and footer, the same fonts and scripts as every other page, so a reader who
lands on a refusal has not fallen out of LucidPlan.  What it carries is the
code itself at display size beside its glyph, the headline and explanation
@code{error-note} holds for that status, and two ways back in -- the
projects they can see and the front page, or, when the refusal is a
sign-in that is missing rather than a page that is not there, a way to
sign in."
  (let* ((note (error-note status))
         (glyph (car note))
         (tone (cadr note))
         (headline (caddr note))
         (explanation (cadddr note))
         ;; A 401 is not a wrong turn, it is a locked door: the first thing
         ;; it should offer is the key, not a map.
         (primary (if (= status 401)
                      (list "/login"
                            (G_ "sign in")
                            'sign-in)
                      (list "/projects"
                            (G_ "browse projects")
                            'map-trifold))))
    (html-template G_
                   #:title (format #f "~a" status)
                   #:is-logged-in #f
                   #:proj '()
                   #:body `((div (@ (class ,(mk-class '(flex flex-col
                                                             items-center
                                                             justify-center
                                                             gap-6
                                                             px-3
                                                             sm:px-6
                                                             py-8))))
                                 ,(glass-panel `((div (@ (class ,(mk-class '(flex
                                                                             flex-row
                                                                             items-center
                                                                             gap-4))))
                                                      ,(icon #:type glyph
                                                             #:icon-css (append
                                                                         tone
                                                                         '(text-5xl)))
                                                      (span (@ (class ,(mk-class
                                                                        (append '
                                                                         (font-sans
                                                                          font-black
                                                                          text-5xl
                                                                          leading-none
                                                                          tracking-tight)
                                                                         tone))))
                                                            ,(format #f "~a"
                                                                     status)))
                                                 ,(h1 (G_ headline)
                                                      #:size '(text-2xl)
                                                      #:color '(hg-gradient-text))
                                                 ,(p (G_ explanation)
                                                     #:size '(text-md)
                                                     #:weight '(font-light))
                                                 (div (@ (class ,(mk-class '(flex
                                                                             flex-row
                                                                             flex-wrap
                                                                             gap-3
                                                                             pt-2))))
                                                      (a (@ (href ,(car
                                                                    primary))
                                                            (class ,(mk-class '
                                                                              (no-underline))))
                                                         ,(icon-button #:icon (caddr
                                                                               primary)
                                                                       #:content
                                                                       (cadr
                                                                        primary)))
                                                      ;; The framework's own `light-button' hands
                                                      ;; `button' a bare symbol where a class
                                                      ;; list is wanted, which makes `mk-class'
                                                      ;; fail on an improper list, so the light
                                                      ;; weight is asked for directly.
                                                      (a (@ (href "/")
                                                            (class ,(mk-class '
                                                                              (no-underline))))
                                                         ,(button (G_
                                                                   "back to the top")
                                                                  #:weight '(font-light)))))
                                               #:frost 3
                                               #:padding '(p-6 sm:p-10)
                                               ;; The card is as wide as the measure the
                                               ;; framework reads prose at, and no wider: a
                                               ;; refusal is a few lines, not a page.
                                               #:classes '(flex flex-col gap-5
                                                                w-full
                                                                max-w-prose)))))))

(define (error-page-html status)
  "The whole page for @var{status} as the one string the framework asks a
registered page handler for.

A handler a status is registered with is handed no request, so there is no
cookie to read a language from and no session to draw a name from: the page
is rendered with the framework's own answer to \"no language at all\", which
is the translator `make-i18n-handler' builds under an empty language and
which answers every key with the key itself.  That is the same page an
untranslated LucidPlan shows anywhere else, and the navigation it carries is
the one a signed-out reader sees, which is who is met by a refusal."
  (let ((G_ (parameterize ((current-lang ""))
              (make-i18n-handler))))
    (call-with-output-string (lambda (port)
                               (sxml->xml (error-page G_ status) port)))))

(define (register-error-pages!)
  "Give the four refusals a reader can meet LucidPlan's own page.

Left alone, the framework answers a refusal from an HTML file shipped with
it -- a bare @code{Oh~Fark! 404!} in the case of a path that matches no route
-- or, for a request that timed out, with no body at all.  A status is given
a page by registering a handler for it before the server starts, which is
what this does; the handler is the app's own view layer, so the page looks
like the application that refused the request.

The five statuses registered are the five this server raises on its own: an
unmatched path (404), a route that asked for a sign-in its request did not
carry (401), a request body the framework's own reader refused (400), a
request that took too long to arrive (408), and a handler that threw (500).
A refusal a proxy in front of LucidPlan writes itself -- a 502 while the
service restarts, say -- is the proxy's page to write, since it never reaches
this process at all."
  (for-each (lambda (status)
              (http-status status
                           (lambda ()
                             (error-page-html status))))
            '(400 401 404 408 500)))
