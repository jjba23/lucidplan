;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views login)
  #:use-module (lucidplan html)
  #:use-module (lucidplan config)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (login-page))

;;; The way in, as a window.
;;;
;;; Signing in is the one page a reader meets on the way *into* the
;;; application rather than inside it, so it is drawn as the thing they are
;;; about to be let into: a pane of frosted glass with the chrome a window
;;; wears across the top -- the three lights, the instance's own name -- and the
;;; two doors the page offers inside it.
;;;
;;; The two forms used to be panes of their own, frosted like everything else.
;;; Inside a frosted window that is one frost too many: a blur behind a blur
;;; reads as fog rather than as depth, and it costs a second backdrop pass for
;;; nothing.  They are recessed surfaces now -- a tint a shade off the glass,
;;; with the same hairline the panes wear -- so the hierarchy is legible at a
;;; glance and there is one frost on the page.

(define* (login-page G_
                     #:key (notices '()))
  (let* ((recessed-classes recessed-tint-classes)
         (login-form `(form (@ (action "/login")
                               (class ,(mk-class (append '(w-full rounded-2xl
                                                           p-4
                                                           flex
                                                           flex-row
                                                           flex-wrap
                                                           items-center
                                                           align-middle
                                                           gap-4
                                                           shadow-sm)
                                                         recessed-classes)))
                               (method "POST"))
                            ,(input #:label (G_ "email")
                                    #:name "email"
                                    #:type 'email
                                    #:value ""
                                    #:autocomplete "email"
                                    #:required #t)
                            ,(input #:label (G_ "password")
                                    #:name "password"
                                    #:type 'password
                                    #:value ""
                                    #:autocomplete "current-password"
                                    #:required #t)
                            (div ,(submit-button (G_ "login")))))
         (reset-form `((form (@ (action "/password/reset")
                                (class ,(mk-class (append '(w-full rounded-2xl
                                                            p-4
                                                            flex
                                                            flex-row
                                                            flex-wrap
                                                            items-center
                                                            gap-4
                                                            shadow-sm)
                                                   recessed-classes)))
                                (method POST))
                             ,(input #:label (G_ "account to reset (email)")
                                     #:name "email"
                                     #:type 'email
                                     #:value ""
                                     #:autocomplete "email"
                                     #:required #t)
                             (div ,(icon-button #:icon 'password
                                                #:type 'submit
                                                #:content (G_ "reset")
                                                #:icon-css '(text-md
                                                             text-onyx-700
                                                             dark:text-stone-300)))))))
    (html-template G_
                   #:title (G_ "login")
                   #:canonical-path "/login"
                   #:body `((div (@ (class ,(mk-class '(px-4 pt-4 pb-24))))
                                 ;; What happened on the last attempt -- a
                                 ;; failed sign-in, a reset that went
                                 ;; through or a link that had aged out --
                                 ;; is said here, above the forms, in a live
                                 ;; region, so a reader is told before they
                                 ;; fill anything in again.
                                 ,@(map (lambda (notice)
                                          `(div (@ (class ,(mk-class '(my-2
                                                                       px-4
                                                                       py-2
                                                                       max-w-prose
                                                                       rounded-xl
                                                                       border-2
                                                                       border-jeans-blue-500/50
                                                                       text-onyx-700
                                                                       dark:text-stone-300)))
                                                   (role "status"))
                                                ,notice)) notices)
                                 ,(glass-panel
                                   `(,(window-titlebar
                                       #:title (format #f "~a — ~a"
                                                       (config-ref 'site-name)
                                                       (G_ "login")))
                                     (div (@ (class ,(mk-class '(flex flex-col
                                                                      lg:flex-row
                                                                      gap-10
                                                                      p-6 sm:p-8))))
                                          (div (@ (class ,(mk-class '(flex
                                                                      flex-col
                                                                      gap-8
                                                                      grow
                                                                      min-w-0))))
                                               ,(h2 (G_ "login to lucidplan"))
                                               (div ,login-form))
                                          (div (@ (class ,(mk-class '(flex
                                                                      flex-col
                                                                      gap-8
                                                                      grow
                                                                      min-w-0))))
                                               ,(h2 (G_ "reset password"))
                                               (div ,reset-form))))
                                   ;; The window's own padding is the bar's
                                   ;; business and the columns' business, so the
                                   ;; pane adds none of its own.
                                   #:padding '()
                                   ;; The one page a reader has to get through,
                                   ;; so it frosts deeply and answers the pointer:
                                   ;; it should feel like a place, not a form.
                                   #:frost 4
                                   #:classes '(lp-deep lp-spotlight)
                                   #:content-classes '(flex flex-col)))))))
