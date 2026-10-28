;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views about)
  #:use-module (lucidplan html)
  #:use-module (lucidplan config)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (about-page))

;;; The page that says what this instance is.
;;;
;;; Almost all of it is configuration: the instance names itself, describes
;;; itself and tells its readers how to reach it, and an operator changes any
;;; of that in one place rather than in this view.  What the view adds is the
;;; shape -- a heading, the operator's own account as rendered Org text, and
;;; the doors a reader can go through from here -- so that a fresh checkout
;;; and a heavily customised instance both read as a proper page.

(define (external-link label href icon)
  `(a (@ (class ,(mk-class '(no-underline)))
         (href ,href))
      ,(icon-button #:icon icon
                    #:content label)))

(define* (about-page G_
                     #:key projects is-logged-in)
  (let* ((name (config-ref 'site-name))
         (contact (config-ref 'contact-email))
         (social (config-ref 'social-links))
         (links `(div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                  items-center gap-3 my-4))))
                      (a (@ (class ,(mk-class '(no-underline)))
                            (href "/projects"))
                         ,(icon-button #:icon 'align-bottom
                                       #:content (G_ "browse projects")))
                      ,(external-link (G_ "source code")
                                      (config-ref 'source-url)
                                      'code)
                      ,(external-link (G_ "manual")
                                      (config-ref 'manual-url)
                                      'book-open)))
         (social-row (if (or (null? social)
                             (not (list? social)))
                         '()
                         `(div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                           items-center gap-3
                                                           my-2))))
                               ,@(map (lambda (entry)
                                        `(a (@ (class ,(mk-class '(no-underline)))
                                               (href ,(cdr entry)))
                                            ,(icon-button #:icon 'link
                                                          #:content (car entry))))
                                      social))))
         (contact-row `(div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                        items-center gap-2
                                                        my-2))))
                            ,(em (G_ "write to us at"))
                            (a (@ (class ,(mk-class '(no-underline
                                                      text-jeans-blue-700
                                                      dark:text-jeans-blue-300)))
                                  (href ,(format #f "mailto:~a" contact)))
                               ,contact))))
    (html-template G_
                   #:title (G_ "about")
                   #:description (config-ref 'site-description)
                   #:canonical-path "/about"
                   #:proj projects
                   #:is-logged-in is-logged-in
                   #:body `(div (@ (class ,(mk-class '(px-3 md:px-6 py-6
                                                            max-w-256))))
                                ;; The page a reader decides whether to trust
                                ;; this instance is the page most worth
                                ;; dressing as a window: it is the operator's
                                ;; own account, and the chrome says where it
                                ;; ends.
                                ,(glass-panel `(,(window-titlebar
                                                  #:title (format #f "~a — ~a"
                                                                  name
                                                                  (G_ "about")))
                                                (div (@ (class ,(mk-class '(flex
                                                                            flex-col
                                                                            gap-1
                                                                            p-6 sm:p-10))))
                                                     ,(h1 (G_ "About"))
                                                     ,(sub-h2 name
                                                              #:weight '(font-light))
                                                     ,(em (config-ref 'site-tagline)
                                                          #:size '(text-sm))
                                                     (div (@ (class ,(mk-class '(max-w-prose
                                                                                 my-2))))
                                                          ,(org2html (config-ref 'about-org)))
                                                     ,links
                                                     ,social-row
                                                     ,contact-row))
                                              #:frost 3
                                              #:padding '()
                                              #:classes '(flex flex-col gap-4
                                                               w-full
                                                               lp-deep
                                                               lp-spotlight))))))
