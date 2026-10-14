;;; lucidplan

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

(define-module (maak)
  #:use-module (maak maak)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:declarative? #t)

;; ====== Maak constants ======

;; The TCP port on which the web server will listen.
(define http-port
  50025)

(define deploy-dir
  "/srv/http/jointhefreeworld.org")

;; The file where the Lucidplan DB is located.
(define lucidplan-db
  "lucidplan.db")

;; The engine mutastructura is pointed at, and the directory holding the
;; migrations it applies to it.  The migrations are plain SQL files, run in
;; filename order, so the name of a file is its place in the queue: the
;; `YYYY-MM-DD_HH:MM:SS_some-name.sql' shape the project uses sorts the way
;; the schema grew.  A second engine would bring its own directory here.
(define db-engine
  "sqlite3")

(define db-migrations-dir
  "./resources/migrations/sqlite")

;; The main copyright holders of the project.
(define copyright-holders
  '("Josep Bigorra"))

;; The path to the directory where the i18n resources are located.
(define i18n-dir
  "./sys/i18n/locale")

;; The text domain for the i18n translations.
(define i18n-textdomain
  "translate")

(define i18n-locales
  '(en_US nl_NL es_ES ja_JP))

(define hot-reload-file-types
  '(scm css js))

;; The Emacs Lisp script to run for publishing the manual.  The manual is the
;; README: the two documents were merged into one, so the HTML the project's
;; site serves is that document exported, not a second file kept in step with
;; it by hand.
(define emacs-lisp-manual-publish-cmd
  '(progn (find-file "./README.org")
          (org-html-export-to-html)
          (message "finished publishing Lucidplan manual")))

;; ====== Maak i18n abstractions ======

(define (i18n-compile* lang)
  "Compile the translation file (.po) into a machine-readable (.mo) format

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgfmt to compile the .po file into a .mo file for the specified language
The output .mo file will be placed in the LC_MESSAGES directory under the language's folder."
  ($ (list (~ "msgfmt -v -o ~a/~a/LC_MESSAGES/~a.mo" i18n-dir lang
              i18n-textdomain)
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain))))

(define (i18n-init* lang)
  "Initialize a new translation file from a template (.pot) for a specific language.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msginit to generate a .po file from the .pot template for the specified language
The -l option sets the language, and --no-translator means no translator is pre-filled in the .po file
The generated .po file is placed in the LC_MESSAGES directory for the given language"
  (mkdir-p (~ "~a/~a/LC_MESSAGES" i18n-dir lang))
  ($ (list (~ "msginit -l ~a.UTF8 -o" lang)
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
           (~ "-i ~a/~a.pot --no-translator" i18n-dir i18n-textdomain))
     #:verbose? #t))

(define (i18n-merge* lang)
  "Merge translation updates from the template (.pot) into an existing .po file.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgmerge to update the specified language's .po file with new translations from the template (.pot)
The -U flag updates the .po file in place
This ensures the .po file stays up-to-date with the latest changes in the template"
  ($ (list "msgmerge -U"
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
           (~ "~a/~a.pot" i18n-dir i18n-textdomain))))

(define* (i18n-extract* #:key (update? #t))
  "Extract translatable strings from Scheme source files using xgettext.
Args: update? represents whether we should update a template or create a new one."
  ($ (list (~ "xgettext --default-domain=~a" i18n-textdomain)
           (~ "--output=~a/~a.pot" i18n-dir i18n-textdomain)
           "--language=Scheme --keyword=G_"
           "--sort-by-file"
           (if update? "--join-existing" "")
           (~ "--copyright-holder=\"~a\""
              (string-join copyright-holders ","))
           (~ "--package-name=~a" i18n-textdomain)
           "lib/lucidplan/job/*.scm"
           "lib/lucidplan/*.scm"
           "app/controllers/*.scm"
           "app/views/*.scm"
           "app/api/*.scm"
           "ENTRY"
           "--from-code UTF-8")
     #:verbose? #t))

;; ====== Maak vendored theme assets ======

;; LucidPlan wears a theme it serves itself.  The Lucida faces, the two web
;; families the shell names after them and the Phosphor icon face used to be
;; fetched from other people's servers while a page was loading -- the faces
;; from jointhefreeworld.org, the icons from a CDN -- so every visitor of every
;; self-hosted instance depended on hosts the operator does not run, and the
;; instance was slower and less honest about where its bytes came from.
;;
;; They now live under `pub/' as files of this project and are handed out by
;; the instance itself (see `asset-url' in the manual for pointing that at
;; another host).  `fetch-assets' refetches them from the very places they came
;; from, so keeping them current is one command and the provenance of every
;; file is written down here rather than remembered.

;; Where the Lucida faces come from.
(define lucida-font-base
  "https://jointhefreeworld.org/static-assets/fonts")

;; The faces themselves: Lucida Sans and its typewriter companion, four styles
;; each, and the Unicode face the base family falls back to.
(define lucida-fonts
  '("LSANS.woff" "LSANSI.woff" "LSANSD.woff" "LSANSDI.woff"
    "LTYPE.woff" "LTYPEO.woff" "LTYPEB.woff" "LTYPEBO.woff"
    "l_10646.woff"))

;; The web families the shell asks for by name: a sans to fall back to when
;; Lucida is absent and the monospace face.  Their stylesheets are captured
;; with them, so the weight axis and the subset split stay the font service's
;; choice rather than one made here.
(define web-font-families
  '("Intel+One+Mono:ital,wght@0,300..700;1,300..700"
    "Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800"))

;; The icon set the interface draws with.
(define phosphor-version
  "2.1.1")

(define phosphor-font-base
  (~ "https://cdn.jsdelivr.net/npm/@phosphor-icons/web@~a/src/regular"
     phosphor-version))

;; The icon stylesheet names four formats of its one face, weakest last.  Only
;; the first two are taken: a TrueType file and an SVG font are what a browser
;; falls back to when it cannot read woff2 or woff, which no current browser
;; cannot, and the SVG font alone is three megabytes of a format every engine
;; has dropped.  The lines naming them are dropped from the sheet as it is
;; taken, so it points at the files that are here and no others.
(define phosphor-fonts
  '("Phosphor.woff2" "Phosphor.woff"))

;; Each source in the list is separated from the next by a comma *after* it,
;; so dropping a source means taking the comma before the one that follows,
;; and the last source kept keeps its terminating semicolon.
(define phosphor-font-forms-left-out
  '(",\n    url(\"../fonts/Phosphor.ttf\") format(\"truetype\")"
    ",\n    url(\"../fonts/Phosphor.svg#Phosphor\") format(\"svg\")"))

;; The mark a link preview shows when the instance has not been given one of
;; its own: the project's own, kept as a file of this project so that a fresh
;; instance describes itself with an image it serves.
(define site-logo-base
  "https://jointhefreeworld.org/static-assets/lucidplan/img")

(define site-logo-file
  "logo-small.jpg")

;; A font service answers an ordinary browser with woff2 and answers a bare
;; HTTP client with something older, so the request wears a browser's name: the
;; files are taken in the format the pages will actually use.
(define asset-user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36")

(define asset-css-dir
  "pub/css")

(define asset-font-dir
  "pub/fonts")

;; The Lucida faces, as the stylesheet declares them.  This is the block that
;; used to sit in the page pointing at another host, with the addresses changed
;; to the copies beside it: `../fonts/' resolves against the stylesheet's own
;; address, so the faces follow it wherever the instance hands it out from.
(define lucida-font-face-css
  "/* --- Lucida Sans --- */

@font-face {
  font-family: 'Lucida Sans';
  font-style: normal;
  font-weight: 400;
  src: url('../fonts/LSANS.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans';
  font-style: italic;
  font-weight: 400;
  src: url('../fonts/LSANSI.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans';
  font-style: normal;
  font-weight: 600;
  src: url('../fonts/LSANSD.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans';
  font-style: italic;
  font-weight: 600;
  src: url('../fonts/LSANSDI.woff') format('woff');
}


/* --- Lucida Sans Typewriter --- */

@font-face {
  font-family: 'Lucida Sans Typewriter';
  font-style: normal;
  font-weight: 400;
  src: url('../fonts/LTYPE.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans Typewriter';
  font-style: oblique;
  font-weight: 400;
  src: url('../fonts/LTYPEO.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans Typewriter';
  font-style: normal;
  font-weight: 700;
  src: url('../fonts/LTYPEB.woff') format('woff');
}

@font-face {
  font-family: 'Lucida Sans Typewriter';
  font-style: oblique;
  font-weight: 700;
  src: url('../fonts/LTYPEBO.woff') format('woff');
}


/* --- Lucida Sans Unicode --- */

@font-face {
  font-family: 'Lucida Sans Unicode';
  font-style: normal;
  font-weight: 400;
  src: url('../fonts/l_10646.woff') format('woff');
}
")

;; ====== Maak tasks ======

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell '("guile -L ./lib -L ."
                                 "-c '((@ (ares server) run-nrepl-server))'")))

(define (default)
  "Default task to be run, list available tasks."
  ($ '("maak --list")))

(define (build-manual)
  "Render the project's manual."
  (time-machine-manifest-shell (list (~ "emacs -Q --batch --eval '~s'"
                                        emacs-lisp-manual-publish-cmd))
                               #:verbose? #t))

(define (deploy)
  "Deploy the project's documentation."
  (build-manual)
  (mkdir-p (~ "~a/manuals/lucidplan" deploy-dir))
  (delete-file-recursively (~ "~a/manuals/lucidplan/index.html" deploy-dir))
  (mv "README.html"
      (~ "~a/manuals/lucidplan/index.html" deploy-dir)))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")))

(define (test)
  "Run the test suite with veritas.

The tests live under `test/veritas/': `unit/' for the ones that run on every
change, and `benchmark/' for the ones that measure rather than assert, which
veritas keeps apart because they answer a different question and take a
different amount of time.  The application's own directories are on the load
path, so a test names the module it tests the way the application does."
  ;; The two settings the run is described by are handed over with `env' rather
  ;; than written in front of `guile': what follows the `--' of a guix shell is
  ;; exec'd, not read by a shell, so an assignment written there is taken for
  ;; the name of a program and the run answers `command not found'.  The
  ;; loader's own defaults agree with both of them; they are said anyway, so a
  ;; run from here is the run that was asked for.
  (time-machine-manifest-shell
   (list "env"
         "VERITAS_TEST_TYPE=unit"
         "VERITAS_RUN_BENCHMARKS=true"
         "guile -L . -L lib -L test"
         "-c '((@ (veritas runner) run-tests))'")))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (time-machine-manifest-shell (list "watchexec"
                                     "--clear"
                                     (~ "--exts ~a"
                                        (string-join (map symbol->string
                                                      hot-reload-file-types)
                                                     ","))
                                     "--on-busy-update=restart"
                                     "--shell bash"
                                     "--watch . --"
                                     "maak server")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (kill-server)
  "Kill any running HTTP server in the same port."
  ($ (list (~ "fuser -k ~a/tcp || true" http-port)))
  ($ '("sleep 1")))

(define (server)
  "Target to start the lucidplan server with the default configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work")))

(define (production-server)
  "Target to start the lucidplan server with the production configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work -c conf/production.conf")))

(define (migrate)
  "Bring the database up to the schema the migrations describe.

Every migration in the migrations directory that the ledger has not recorded
yet is applied, in filename order, each inside its own transaction; a
database that is already up to date is left alone, and one that is not is
never half-migrated, because a migration that fails takes its own statements
back with it.  The ledger is what makes that possible -- it is the record of
what has run, and mutastructura is the only thing that writes it -- so the
migrations are applied here and nowhere else, by hand or otherwise."
  (time-machine-manifest-shell
   (list (~ "mutastructura --database ~a --connection ~a --migrations ~a"
            db-engine lucidplan-db db-migrations-dir))))

(define (init-db)
  "Initialize a new database, migrate it, and enable WAL mode for higher
concurrent writes to SQLite 3."
  (delete-file-recursively lucidplan-db)
  (delete-file-recursively (~ "~a-shm" lucidplan-db))
  (delete-file-recursively (~ "~a-wal" lucidplan-db))
  (time-machine-manifest-shell
   (list (~ "sqlite3 ~a 'PRAGMA journal_mode=WAL;'" lucidplan-db)))
  (migrate))

;; ====== Maak i18n tasks ======

(define (i18n-compile)
  "Compile the translation files for all locales."
  (for-each i18n-compile* i18n-locales))

(define (i18n-init-en)
  "Initialize a new translation file for en_US."
  (i18n-init* 'en_US))

(define (i18n-init-nl)
  "Initialize a new translation file for nl_NL."
  (i18n-init* 'nl_NL))

(define (i18n-init-es)
  "Initialize a new translation file for es_ES."
  (i18n-init* 'es_ES))

(define (i18n-init-ja)
  "Initialize a new translation file for ja_JP."
  (i18n-init* 'ja_JP))

(define (i18n-merge)
  "Merge translation updates for all locales."
  (for-each i18n-merge* i18n-locales))

(define (i18n-new-template)
  "Create a new i18n template based on strings extracted from source code."
  (i18n-extract* #:update? #f))

(define (i18n-update-template)
  "Update the i18n template based on strings extracted from source code."
  (i18n-extract* #:update? #t))

(define (olive-css)
  ($ '("curl --output \"./pub/css/lucidplan.olive.min.css\""
       "\"https://codeberg.org/jjba23/olive-css/raw/branch/trunk/resources/css/lucidplan.olive.min.css\"")))

(define (fetch-assets)
  "Refresh the theme files this instance serves itself, from where they came from.

The faces and the icon set live under `pub/' as files of this project, so a
page load asks this server for them and no other host.  The sources are named
in the constants above; this task takes them again, writes the two
stylesheets that describe them, and leaves everything else alone.  It is
idempotent, so running it when nothing has changed rewrites the same bytes."

  (define (file-name url)
    "The local name a fetched file is kept under."
    (let* ((clean (let ((hash (string-contains url "#")))
                    (if hash (substring url 0 hash) url)))
           (slash (string-rindex clean #\/)))
      (if slash (substring clean (+ slash 1) (string-length clean)) clean)))

  (define (web-font-name url)
    "The local name of a file a font service hands out.

They are content-addressed and carry no family name, so they are prefixed to
say where they came from when one is read on its own."
    (~ "gf-~a" (file-name url)))

  (define (urls-in css)
    "Every http(s) URL the stylesheet CSS points at, each named once."
    (delete-duplicates (map (lambda (m)
                              (match:substring m 1))
                            (list-matches (make-regexp
                                           "url\\([\"']?(https://[^\"')]+)")
                                          css))))

  (define (fetch-text url)
    "The body the web server at URL answers with."
    (let* ((port (open-input-pipe (format #f "curl -fsS -A '~a' '~a'"
                                          asset-user-agent url)))
           (body (get-string-all port)))
      (close-pipe port)
      (when (string-null? body)
        (error "fetch-assets: nothing came back from" url))
      body))

  (define (fetch-all! pairs)
    "Take every (URL . FILE) in PAIRS, in a single run of the fetcher."
    (when (null? pairs) (error "fetch-assets: nothing to fetch"))
    ($ (append (list "curl -fsS --fail-early" "-A"
                     (~ "'~a'" asset-user-agent))
               (append-map (lambda (pair)
                             (list "--output" (~ "'~a'" (cdr pair))
                                   (~ "'~a'" (car pair))))
                           pairs))))

  (define (write-asset path text)
    (call-with-output-file path (lambda (port) (display text port)))
    (log-info "wrote ~a" path))

  ;; A family's stylesheet is captured as the font service wrote it -- the
  ;; @font-face blocks and the subset split are its own -- and then rewritten
  ;; so every file it points at is the copy beside it.
  (define (web-family-css family)
    (let* ((css (fetch-text (~ "https://fonts.googleapis.com/css2?family=~a&display=swap"
                               family)))
           (urls (urls-in css)))
      (fetch-all! (map (lambda (url)
                         (cons url (~ "~a/~a" asset-font-dir
                                      (web-font-name url))))
                       urls))
      (fold (lambda (url sheet)
              (string-replace-substring sheet url
                                         (~ "../fonts/~a"
                                            (web-font-name url))))
            css urls)))

  (mkdir-p asset-css-dir)
  (mkdir-p asset-font-dir)

  ;; -- The Lucida faces -----------------------------------------------------
  (log-info "taking the Lucida faces from ~a" lucida-font-base)
  (fetch-all! (map (lambda (f)
                     (cons (~ "~a/~a" lucida-font-base f)
                           (~ "~a/~a" asset-font-dir f)))
                   lucida-fonts))

  ;; -- The two web families ------------------------------------------------
  (log-info "taking the web families from the font service")
  (let ((families (map web-family-css web-font-families)))
    ;; The sheet is assembled rather than formatted: a stylesheet is full of
    ;; `~' -- an attribute selector like `[class~="ph"]' is one -- and a
    ;; format string would read every one of them as a directive.
    (write-asset (~ "~a/fonts.css" asset-css-dir)
                 (string-append
                  (format #f "/* The faces this instance serves itself.

   Written by `maak fetch-assets' -- edit that task, not this file.  The
   Lucida faces are taken from ~a, the families below from the font
   service, and every file is kept beside this sheet in `pub/fonts/'. */

" lucida-font-base)
                  lucida-font-face-css "\n"
                  (string-join families "\n") "\n")))  ;; -- The mark a link preview shows --------------------------------------
  (log-info "taking the project's own mark from ~a" site-logo-base)
  (mkdir-p "pub/img")
  (fetch-all! (list (cons (~ "~a/~a" site-logo-base site-logo-file)
                          (~ "pub/img/~a" site-logo-file))))

  ;; -- The icon face and its stylesheet ------------------------------------
  (log-info "taking the icon set from ~a" phosphor-font-base)
  (fetch-all! (map (lambda (f)
                     (cons (~ "~a/~a" phosphor-font-base f)
                           (~ "~a/~a" asset-font-dir f)))
                   phosphor-fonts))
  (let* ((css (fetch-text (~ "~a/style.css" phosphor-font-base)))
         (local (string-replace-substring css "./Phosphor."
                                          "../fonts/Phosphor."))
         (trimmed (fold (lambda (line sheet)
                          (string-replace-substring sheet line ""))
                        local phosphor-font-forms-left-out)))
    ;; A sheet that still pointed at a file this instance does not serve would
    ;; send a browser after it and land a 404, so the trimming is checked
    ;; rather than trusted.
    (when (string-contains trimmed "Phosphor.ttf")
      (error "fetch-assets: could not drop the TrueType source from the icon sheet"))
    (when (string-contains trimmed "Phosphor.svg")
      (error "fetch-assets: could not drop the SVG source from the icon sheet"))
    ;; A separator left dangling is a broken declaration, so the shape of what
    ;; is left is checked too, not only what went away.
    (when (string-contains trimmed ",;")
      (error "fetch-assets: the icon sheet's src list is malformed"))
    (write-asset (~ "~a/phosphor.css" asset-css-dir)
                 (string-append
                  (format #f "/* The icon face this instance serves itself (Phosphor ~a).

   Written by `maak fetch-assets' -- edit that task, not this file.  The
   face and its stylesheet are taken from ~a and kept as written there,
   except that the TrueType and SVG sources are dropped from the face's own
   src list: they are what a browser falls back to when it cannot read woff2
   or woff, and of the two the SVG font alone is three megabytes. */

"
                         phosphor-version phosphor-font-base)
                  trimmed "\n")))

  (log-info "theme assets refreshed"))

(define (import-production-database)
  "Import production env's database to local workspace via SSH/SCP"
  ($ '("scp wolk-jjba:/etc/byggsteg/job-clone/lucidplan/trunk/lucidplan.db* .")))

(define (import-production-env)
  "Import production env's database to local workspace via SSH/SCP"
  ($ '("scp wolk-jjba:/etc/byggsteg/job-clone/lucidplan/trunk/env.scm .")))

(define (local-login)
  ($ '("http --form POST" "http://localhost:50025/api/v1/login"
       "email=jjbigorra@gmail.com" "password=***")))
