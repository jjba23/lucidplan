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

;;; ===== One place for the things an instance says about itself =====

;;; An instance used to name itself in a scattered way: the page title, the
;;; footer's copyright line, the address a password reset is sent from and the
;;; link to the project's own source each lived, hardcoded, inside a view or a
;;; controller.  That is fine for the one instance these pages were written
;;; for and wrong for every other: a self-hosted LucidPlan is somebody else's
;;; project on somebody else's domain, and every one of those strings is the
;;; operator's to choose.
;;;
;;; This module is the single place those values live.  It holds the defaults
;;; that ship with the software, and it lets a deployment override any of them
;;; without touching code: a value defined in the `(lucidplan env)' module --
;;; the one loaded from `env.scm', which is per-deployment and kept out of
;;; version control -- wins over the default of the same name.  So a fresh
;;; checkout renders as the project's own instance with no configuration at
;;; all, and an operator changes a name by writing one `define-public' in the
;;; one file they already keep private.
;;;
;;; A key is a symbol and the value is whatever the key means: a string for a
;;; name or an address, a list for the keywords of a page or the languages the
;;; interface offers.  `config-ref' answers the effective value.

(define-module (lucidplan config)
  #:use-module (ice-9 format)
  #:use-module (artanis config)
  #:export (config-ref absolute-url default-config))

;;; ===== The values an instance can change =====

;; The defaults are the project's own: the instance the author runs.  They are
;; deliberately the values the pages used to carry inline, so turning them into
;; configuration changes nothing until an operator changes one.
(define default-config
  `(
    ;; -- What the instance is called -------------------------------------
    (site-name . "LucidPlan")
    (site-tagline . "free and open project management for everyone")
    (site-description . "LucidPlan is free and open project management for teams of every shape and size: multi-project workspaces, backlog management, kanban boards and fine-grained, Lisp-based permissions.")
    (site-keywords
     . ("project management" "kanban" "backlog" "agile" "scrum" "issue tracker"
        "free software" "self-hosted" "scheme" "guile" "gnu guix"))
    ;; The canonical base address of the instance, e.g.
    ;; "https://lucidplan.example.org".  Left empty, the pages fall back to the
    ;; host the request arrived on, which is right for a local development
    ;; server and for an instance behind a single public name.
    (site-url . "")
    ;; The image a link preview shows.  It may be written either as an
    ;; absolute URL or as a path on this instance -- "/img/logo-small.jpg",
    ;; the project's own mark, which `maak fetch-assets' keeps beside the
    ;; pages -- and a path is answered on the instance's own address, since a
    ;; preview image has to be absolute for the service that reads it.  Empty
    ;; leaves the tags out rather than pointing them at an image that is not
    ;; there.
    (site-logo . "/img/logo-small.jpg")
    (site-language . "en")

    ;; -- The address this instance's own files are handed out from ---------
    ;; The shell wears a theme this instance serves itself: the Lucida faces,
    ;; the web fonts, the icon face and the stylesheets that describe them all
    ;; live under `pub/' and are refetched by `maak fetch-assets'.  Left
    ;; empty, a page names them by path alone -- "/css/fonts.css" -- and the
    ;; instance that served the page answers for them too, which is what a
    ;; self-hosted deployment wants.  Set it to an absolute base to hand that
    ;; work to a static host or a CDN instead: every asset address is then
    ;; built on this, and nothing else about the pages changes.
    (asset-url . "")

    ;; -- Page analytics ----------------------------------------------------
    ;; The address the instance's own page-view script reports to, and how
    ;; often it reports while a reader stays on the page.  Left empty, the
    ;; script is not put on the page at all: no request leaves the reader's
    ;; browser, and nothing about the instance is told to anybody else.
    (analytics-endpoint
     . "https://api.symbanta.jointhefreeworld.org/v1/events/register")
    (analytics-interval . 4000)

    ;; -- Who to credit and how to reach them -----------------------------
    (copyright-holder . "Josep Bigorra")
    (contact-email . "jjbigorra@gmail.com")
    (source-url . "https://codeberg.org/jjba23/lucidplan")
    (manual-url . "https://jointhefreeworld.org/manuals/lucidplan/")
    (donate-url . "https://bmc.link/jjbigorra")
    ;; A list of (label . url) pairs shown on the about page.  Empty by
    ;; default; an operator advertises their chat or social presence by
    ;; setting it.
    (social-links . ())

    ;; -- What the about page says ----------------------------------------
    ;; Org-mode text, rendered to HTML on the about page.  It is a string so
    ;; an operator can replace it wholesale with their own account of the
    ;; instance, its purpose and its rules.
    (about-org . "#+begin_quote
LucidPlan is project management for teams of all sizes, done the free-software way.
#+end_quote

LucidPlan enhances the use of Agile work methodologies in teams thanks to its fast-paced, no-nonsense workflows and deep customizability -- and thanks to being written in Lisp.

It is the fruit of years of experience using bloated proprietary systems, and of the frustrations they bring.

* What it does

- *Multi-project workspaces*: manage dozens of projects from one dashboard.
- *Backlog management*: a clean, sortable, drag-and-drop list of every task.
- *Kanban boards*: track tickets as they move through =To-do=, =Work in Progress=, =In Review= and beyond.
- *Org-mode everywhere*: write titles and descriptions in Org syntax and see them rendered as rich HTML.
- *Granular permissions*: an ACL written as plain Lisp lists, so exactly what each user may do is explicit and programmatic.
- *Free software*: licensed under the GNU AGPL v3 or newer, hackable end to end in Scheme.

* Who runs this instance

This is a self-hosted deployment. Its operator may have added their own notes, contacts and rules below.

* Contributing

Contributions, issues and ideas are welcome at the project's repository.")

    ;; -- Mail -------------------------------------------------------------
    ;; The name and address a notification is sent from, and the subject
    ;; prefix that marks it as this instance's.
    (mail-sender-name . "No Reply - LucidPlan")
    (mail-sender-address . "noreply@mail.jointhefreeworld.org")
    (mail-subject-prefix . "LucidPlan - ")
    ;; How hard the server tries to hand a message to the mail service before
    ;; it gives up.  `mail-retry-attempts' counts the first try, and the wait
    ;; between tries grows as `mail-retry-delay' doubled each time.
    (mail-retry-attempts . 4)
    (mail-retry-delay . 2)

    ;; -- Accounts and passwords ------------------------------------------
    ;; How long a password-reset link stays good, in seconds (one week).
    (password-reset-validity . ,(* 24 7 3600))
    ;; The strength score at which a new password is accepted.  It is
    ;; bewaking's own scale: 0 very weak .. 4 very strong, so 3 means
    ;; "strong".
    (password-min-score . 3)

    ;; -- The languages the interface offers ------------------------------
    ;; Each entry is (catalog-name . the-language-as-it-names-itself).  The
    ;; catalog must exist under `sys/i18n/locale'.  See `maak i18n-compile'.
    (locales . (("en_US" . "English (US)")
                ("nl_NL" . "Nederlands")
                ("es_ES" . "Español")
                ("ja_JP" . "日本語")))))

;;; ===== The lookup =====

(define (env-override key)
  "Answer the value a deployment set for KEY in the @code{(lucidplan env)}
module, or @code{#f} when the module is absent or KEY is not defined in it.

The env module is loaded from the deployment's own @code{env.scm} at start-up
(see @code{ENTRY}); it may or may not exist, and it need not mention every key,
so both absences are ordinary and answer @code{#f} rather than failing."
  (let ((module (resolve-module '(lucidplan env) #:ensure #f)))
    (and module
         (let ((variable (module-variable module key)))
           (and variable
                (variable-bound? variable)
                (variable-ref variable))))))

(define (config-ref key)
  "Answer the effective value of the configuration KEY (a symbol).

A value the deployment defined in the @code{(lucidplan env)} module wins; with
no override the value @code{default-config} ships is answered.  An unknown key
is a programming mistake rather than a missing setting, so it raises."
  (let ((override (env-override key)))
    (cond
      (override override)
      ((assoc key default-config) => cdr)
      (else (error "config-ref: unknown configuration key" key)))))

(define* (absolute-url path #:optional base-url)
  "Answer PATH as an absolute URL on the instance's own address.

The base is the configured @code{site-url} unless one is passed, and the host
the request arrived on when no base is configured -- which is what a local
development server, or an instance reached by a single public name, wants.
When even that cannot be determined, PATH is answered unchanged rather than
prefixed with a broken address."
  (let* ((configured (or base-url (config-ref 'site-url)))
         (base (cond
                 ((and (string? configured) (not (string-null? configured)))
                  configured)
                 (else
                  (catch #t
                         (lambda () (current-myhost))
                         (lambda _ "")))))
         (trimmed (if (string-suffix? "/" base)
                      (substring base 0 (- (string-length base) 1))
                      base))
         (suffix (if (string-prefix? "/" path) path
                     (string-append "/" path))))
    (if (string-null? trimmed) suffix
        (string-append trimmed suffix))))
