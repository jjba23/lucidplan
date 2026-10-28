;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (app views projects)
  #:use-module (web uri)
  #:use-module (ice-9 format)
  #:use-module (lucidplan html)
  #:use-module (lucidplan avatars)
  #:use-module (artanis artanis)
  #:use-module (kracht prelude)
  #:use-module (hygguile hygguile)
  #:export (render-project render-project-list-item project-avatar
                           html-from-projects
                           html-new-project-form html-edit-project-form
                           html-project-form))

;;; ----------------------------------------------------------------------------
;;; Data Access Helpers
;;; ----------------------------------------------------------------------------

(define (proj-ref p key . default)
  "Safely retrieve KEY from project association list P, falling back to DEFAULT or empty string."
  (let ((val (assq-ref p key)))
    (if val val
        (if (null? default) ""
            (car default)))))

(define (proj-decoded-url p key)
  "Retrieve and URI-decode a URL field from project P."
  (uri-decode (proj-ref p key "")))

;;; ----------------------------------------------------------------------------
;;; UI Helpers & Sub-components
;;; ----------------------------------------------------------------------------

(define (project-links-bar G_ p)
  (let ((human-id (proj-ref p
                            'human-id))
        (url (proj-decoded-url p
                               'url))
        (vcs-url (proj-decoded-url p
                                   'vcs-url)))
    ;; The names here are the names of the *places*, not of the project: the
    ;; card this bar sits in is already the project, so "backlog: LucidPlan"
    ;; said the same word twice in one line.  One line, one gap, and the ink
    ;; does the separating -- see `quiet-link'.
    `(div (@ (class ,(mk-class '(flex flex-wrap flex-row
                                       items-center gap-1
                                       -ml-2))))
          ,(quiet-link #:icon 'scroll
                       #:label (G_ "backlog")
                       #:href (format #f "/tickets/~a" human-id))
          ,(quiet-link #:icon 'kanban
                       #:label (G_ "kanban")
                       #:href (format #f "/kanban/~a" human-id))
          ,(render-if-nestring (lambda (x)
                                 (quiet-link #:icon 'link
                                             #:label (G_ "view live")
                                             #:href x))
                               url)
          ,(render-if-nestring (lambda (x)
                                 (quiet-link #:icon 'code
                                             #:label (G_ "project code")
                                             #:href x))
                               vcs-url))))

(define (project-actions-bar G_ p)
  (let ((id (proj-ref p
                      'id))
        (human-id (proj-ref p
                            'human-id)))
    ;; Changing the project on the left, taking it away on the right, and the
    ;; project's own reference -- the one string here that is not an action --
    ;; pushed to the end where it reads as a fact about the card rather than as
    ;; another thing to press.
    `(div (@ (class ,(mk-class '(flex flex-wrap
                                      flex-row
                                      items-center
                                      gap-1
                                      py-1
                                      -ml-2))))
          ,(quiet-link #:icon 'pencil
                       #:label (G_ "edit")
                       #:href (format #f "/projects/id/~a/edit" human-id))
          (form (@ (enctype "application/x-www-form-urlencoded")
                   (method "POST")
                   (action "/projects/delete"))
                (input (@ (name "id")
                          (hidden "")
                          (value ,id)) "")
                ,(quiet-submit `(span (@ (class ,(mk-class '(items-center
                                                             flex
                                                             flex-row
                                                             gap-1))))
                                      ,(icon #:type 'trash
                                             #:icon-css '(text-md))
                                      ,(G_ "delete"))))
          (small (@ (class ,(mk-class '(font-mono text-onyx-700
                                                  dark:text-stone-500
                                                  text-sm
                                                  ml-auto))))
                 ,human-id))))

;;; ----------------------------------------------------------------------------
;;; Core View Components
;;; ----------------------------------------------------------------------------

;; The body of a project card: what the project is, and what can be done with
;; it.  The card around it -- its surface, and the row that opens it -- belongs
;; to the list item this is drawn in (see `render-project-list-item'), so what
;; this answers is the *inside* of a card: no surface of its own, and no
;; heading of its own either, because the row that opened it is already the
;; heading.
(define (render-project G_ p)
  (let* ((description (proj-ref p
                                'description)))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))

           ;; Description
           ,(render-if-nestring (lambda (x)
                                  `(p (@ (class ,(mk-class '(text-sm
                                                             sm:text-md
                                                             py-1
                                                             sm:py-3
                                                             leading-5
                                                             sm:leading-6
                                                             text-onyx-700
                                                             dark:text-stone-300
                                                             whitespace-pre-wrap
                                                             font-medium
                                                             wrap-break-word))))
                                      ,x))
                                (org2html (html-unescape description)))

           ,(hr #:margin '(my-2 sm:my-3 mx-1 sm:mx-3))

           ;; Links & Actions
           ,(project-links-bar G_ p)
           ,(project-actions-bar G_ p)))))

;;; ===== The picture a project wears =====
;;;
;;; A project that has been given a picture is drawn with it; a project that
;;; has not is drawn with its own initial, in a colour derived from its name.
;;; The two are the same size and the same shape on purpose: a column of
;;; projects then reads as a column of pictures whether anybody has uploaded
;;; anything or not, which is the whole job of a fallback.  A name and a
;;; letter are enough to tell one row from another at a glance, so nothing has
;;; to be uploaded for the list to look finished.
;;;
;;; The picture is fetched from `/projects/id/<human-id>/avatar' -- the
;;; address is stable and its bytes are not, which is what lets it be cached
;;; for ever (see the route that serves it).  The letter is not fetched at
;;; all: it is a letter, set in the page's own type, so it is sharp at any
;;; size and costs nothing to draw.
;;;
;;; Both are decoration beside the project's name, which is why neither is
;;; given a description a reader would hear twice.
(define* (project-avatar p #:key (classes '(w-9 h-9 text-md)))
  "Answer the node a project P is pictured by: its upload, over its letter.

The letter is always drawn and the picture is laid over it, rather than one
being chosen in place of the other.  Three things follow, and all three are
the point.  A picture that is slow to arrive shows the project's letter while
it comes, instead of an empty disc.  A picture that cannot be fetched at all
-- the file went with an old backup, the row was copied without the directory
-- takes itself away and leaves the letter, instead of leaving the broken-image
mark every browser draws.  And a project that has never had a picture is not
a special case at all: it is the same node with nothing laid over it."
  (let* ((human-id (proj-ref p
                             'human-id))
        (title (proj-ref p
                         'title))
        (avatar (proj-ref p
                          'avatar))
        (has-picture? (and (string? avatar)
                           (not (string-null? avatar)))))
    `(span (@ (class ,(mk-class (append '(lp-avatar lp-avatar-letter)
                                        classes)))
              (style ,(format #f "--lp-avatar-hue: ~a"
                              (avatar-hue (if (string? title)
                                              title human-id))))
              (aria-hidden "true"))
           ,(avatar-initial title)
           ,@(if has-picture?
                 `((img (@ (class "lp-avatar-img")
                           ;; The token names the version on show, so a
                           ;; picture can be cached for ever and still be
                           ;; replaced: a new picture is a new address.
                           (src ,(format #f "/projects/id/~a/avatar?v=~a"
                                         human-id avatar))
                           (alt "")
                           (loading "lazy")
                           (decoding "async")
                           (onerror "this.remove()"))))
                 '()))))

;;; One project, as the card it unfolds into.
;;;
;;; The list used to draw a project twice: a narrow row to click, and then --
;;; once it had been clicked -- a separate card dropped into the page below it,
;;; with its own border, its own shadow and its own heading.  Two surfaces for
;;; one thing is what made the list read as a jumble of boxes rather than as a
;;; list.
;;;
;;; It is one card now.  The row is the card's first line and the only line it
;;; shows while it is closed; opening it grows the card (see `lp-disclosure' in
;;; `pub/css/lucidplan-luxe.css' for the reveal itself) and uncovers the body
;;; drawn above.  The mark at the end of the row is what says so, and it is
;;; drawn only where the script that makes it true is running.
(define (render-project-list-item G_ p)
  (let* ((id (proj-ref p
                       'id))
         (title (proj-ref p
                          'title))
         (body-id (format #f "project-body-~a" id)))
    `((div (@ (class ,(mk-class (append '(w-full
                                               rounded-2xl
                                               border-2
                                               border-stone-300/75
                                               dark:border-onyx-100/75
                                               shadow-md
                                               lp-disclosure)
                                            frosted-tint-classes))))

           ;; The row: everything the card says while it is closed, and the
           ;; whole of what a reader clicks, tabs to and presses Enter on.  It
           ;; says the project's name and nothing else -- a line of its
           ;; description here would be the same sentence the body opens with,
           ;; printed twice a centimetre apart.  What a project *is* belongs to
           ;; the card; who it is belongs to the row, and the picture is who it
           ;; is at a glance.
           (div (@ (class ,(mk-class '(lp-disclosure-summary))))
                ,(project-avatar p)
                (h3 (@ (class ,(mk-class '(font-sans text-onyx-700
                                                  dark:text-stone-300
                                                  text-md
                                                  sm:text-lg
                                                  font-bold
                                                  truncate
                                                  grow))))
                    ,title)
                ,(icon #:type 'caret-down
                       #:icon-css '(text-onyx-700 dark:text-stone-300 text-lg
                                    lp-disclosure-chevron)))

           ;; The rest of the card, uncovered rather than inserted.
           (div (@ (class ,(mk-class '(lp-disclosure-body)))
                   (id ,body-id))
                (div (@ (class ,(mk-class '(lp-disclosure-inner))))
                     ,@(render-project G_ p)))))))

(define (projects-header-title G_ search-query)
  `(div (@ (class ,(mk-class '(flex flex-col
                                    sm:flex-row
                                    justify-between
                                    w-full
                                    gap-3
                                    sm:gap-1
                                    items-start
                                    sm:items-center))))
        (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-1
                                         sm:gap-2))))
             ,(icon #:type 'align-bottom
                    #:icon-css '(text-onyx-700 dark:text-stone-300 text-2xl))
             ,(h2 (G_ "Projects"))
             (div (@ (class ,(mk-class '(ml-2))))
                  (a (@ (href "/projects/new"))
                     ,(icon-button #:icon 'plus
                                   #:content (G_ "new project")
                                   #:size '(text-xs sm:text-sm)
                                   #:icon-css '(text-sm text-onyx-700
                                                        dark:text-stone-300)))))
        (div (@ (class ,(mk-class '(w-full sm:w-auto flex justify-start
                                           sm:justify-end))))
             ,(search-box G_
                          #:search-query search-query))))

(define (projects-toolbar G_)
  `(div (@ (class ,(mk-class '(flex flex-col
                                    sm:flex-row
                                    justify-between
                                    items-start
                                    sm:items-center
                                    my-2
                                    gap-2
                                    sm:gap-4))))
        (p (@ (class ,(mk-class '(text-xs sm:text-sm text-onyx-500
                                          dark:text-stone-500 leading-6))))
           ,(G_
             "Tip: Use Tab to navigate projects and Enter/Space to expand or collapse cards."))
        (div (@ (class ,(mk-class '(flex flex-row gap-3 sm:gap-4 pt-1 sm:pt-0))))
             ;; The two controls name what they do and leave the doing to the
             ;; script, which opens or closes every card on the page with the
             ;; same reveal a click gets -- so a page that was opened and then
             ;; folded away again looks like a page, not like a page that was
             ;; reloaded.  They are `js-only' because they *are* the script:
             ;; without it every card is already open, and two controls that
             ;; could do nothing are worse than two that are not offered.
             (button (@ (class ,(mk-class '(js-only
                                            text-sm font-semibold
                                            underline
                                            cursor-pointer
                                            text-onyx-700
                                            dark:text-stone-300
                                            hover:text-onyx-900
                                            dark:hover:text-stone-100)))
                        (type "button")
                        (data-lp-expand-all ""))
                     ,(G_ "Expand All"))
             (button (@ (class ,(mk-class '(js-only
                                            text-sm font-semibold
                                            underline
                                            cursor-pointer
                                            text-onyx-700
                                            dark:text-stone-300
                                            hover:text-onyx-900
                                            dark:hover:text-stone-100)))
                        (type "button")
                        (data-lp-collapse-all ""))
                     ,(G_ "Collapse All")))))

;;; ===== Giving a project a picture =====
;;;
;;; The picture is uploaded on its own, the moment it is chosen, rather than
;;; as part of the form that saves the project's name: a file is not something
;;; a url-encoded body can carry, and a picture that lands only when somebody
;;; remembers to press "save project" is a picture that does not land.  So the
;;; block holds two small forms of its own -- one that hands a file over and
;;; one that takes the picture away again -- above the form that edits the
;;; rest of the project.
;;;
;;; The letter beside the upload is the same avatar the list draws, at twice
;;; the size: the control shows what the row shows, so a reader choosing a
;;; picture knows what they are replacing.
(define (project-picture-form G_ project)
  (let* ((human-id (proj-ref project
                             'human-id))
        (avatar (proj-ref project
                          'avatar))
        (has-picture? (and (string? avatar)
                           (not (string-null? avatar)))))
    `(div (@ (class ,(mk-class (append '(flex flex-row items-center gap-4
                                              p-3 rounded-2xl)
                                       recessed-tint-classes))))
          ,(project-avatar project #:classes '(w-16 h-16 text-2xl))
          (div (@ (class ,(mk-class '(flex flex-col gap-2 min-w-0))))
               (small (@ (class ,(mk-class '(font-sans text-xs font-medium
                                                        uppercase
                                                        tracking-wide
                                                        text-onyx-500
                                                        dark:text-stone-500))))
                      ,(G_ "project picture"))
               (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                 items-center gap-2))))
                    (form (@ (enctype "multipart/form-data")
                             (method "POST")
                             (action ,(format #f
                                       "/projects/id/~a/avatar"
                                       human-id))
                             (class ,(mk-class '(flex flex-row flex-wrap
                                                       items-center gap-2))))
                          (input (@ (type "file")
                                    (name "avatar")
                                    (accept "image/png,image/jpeg,image/gif,image/webp")
                                    (aria-label ,(G_ "choose a picture"))
                                    (class "lp-file")))
                          ,(submit-button (G_ "upload")))
                    ,@(if has-picture?
                          `((form (@ (method "POST")
                                     (action ,(format #f
                                               "/projects/id/~a/avatar/delete"
                                               human-id)))
                                  ,(quiet-submit (G_ "remove picture"))))
                          '()))
               (p (@ (class ,(mk-class '(text-xs
                                        text-onyx-500
                                        dark:text-stone-500))))
                  ,(G_
                    "A PNG, JPEG, GIF or WebP picture. Leave it empty and the project is drawn with the first letter of its name."))))))

(define* (html-from-projects G_
                             #:key projects is-logged-in search-query)
  (html-template G_
                 #:title (G_ "projects")
                 #:canonical-path "/projects"
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(mk-class '(px-3 pb-6))))
                               ((div (@ (class ,(mk-class '(flex flex-col
                                                                 gap-4 grow))))
                                     ,(projects-header-title G_ search-query)
                                     ,(projects-toolbar G_)
                                     (div (@ (id lucidplan-projects-list)
                                             (class ,(mk-class '(flex flex-col
                                                                 gap-2
                                                                 container
                                                                 my-3))))
                                          ,(map (lambda (x)
                                                  (render-project-list-item G_
                                                   x)) projects))))))
                 #:proj projects))

(define* (html-new-project-form G_
                                #:key projects is-logged-in)
  (html-project-form G_
                     #:projects projects
                     #:page-title (G_ "new project")
                     #:canonical-path "/projects/new"
                     #:is-logged-in is-logged-in))

(define* (html-edit-project-form G_
                                 #:key projects project is-logged-in)
  (html-project-form G_
                     #:is-logged-in is-logged-in
                     ;; The picture is its own pair of forms and only a
                     ;; project that exists can have one, so the block is
                     ;; handed to the form rather than built inside it.
                     #:picture (project-picture-form G_ project)
                     #:title (proj-ref project
                                       'title)
                     #:human-id (proj-ref project
                                          'human-id)
                     #:url (proj-ref project
                                     'url)
                     #:vcs-url (proj-ref project
                                         'vcs-url)
                     #:description (html-unescape (proj-ref project
                                                            'description))
                     #:ticket-statuses (proj-ref project
                                                 'ticket-statuses)
                     #:action (format #f "/projects/id/~a/edit"
                                      (proj-ref project
                                                'human-id))
                     #:page-title (G_ "edit project")
                     #:canonical-path "/projects"
                     #:projects projects))

(define* (html-project-form G_
                            #:key (title "")
                            (human-id "")
                            (description "")
                            (ticket-statuses "")
                            (url "")
                            (vcs-url "")
                            (action "/projects/new")
                            (projects '())
                            (page-title "")
                            (picture '())
                            (canonical-path "/projects")
                            (is-logged-in #f))
  (html-template G_
                 #:title (if (string-null? page-title) #f page-title)
                 #:canonical-path canonical-path
                 #:is-logged-in is-logged-in
                 #:body `(div (@ (class ,(mk-class '(px-4 py-4))))
                              ;; The picture first: it is the one thing on
                              ;; this page that is changed without saving
                              ;; anything, so it sits apart from the form
                              ;; rather than inside it.
                              ,@(if (nil? picture) '() `((div ,picture)))
                              (div (form (@ (class ,(mk-class '(flex flex-col
                                                                     gap-4)))
                                            (enctype
                                             "application/x-www-form-urlencoded")
                                            (method "POST")
                                            (action ,action))
                                         ,(input #:label (G_ "title")
                                                 #:name 'title
                                                 #:value (html-unescape title)
                                                 #:required #t)
                                         ,(input #:label (G_ "human id")
                                                 #:name 'human-id
                                                 #:value human-id
                                                 #:required #t)
                                         ,(input #:label (G_ "homepage url")
                                                 #:name 'url
                                                 #:value (uri-decode url)
                                                 #:required #f)
                                         ,(input #:label (G_
                                                          "version control url")
                                                 #:name 'vcs-url
                                                 #:value (uri-decode vcs-url)
                                                 #:required #f)
                                         ,(textarea #:label (G_ "description")
                                                    #:name 'description
                                                    #:value description
                                                    #:required #f)
                                         ;; The statuses this project's own
                                         ;; tickets move through.  Left
                                         ;; empty, the project uses the
                                         ;; instance's statuses, which is
                                         ;; what every project does until it
                                         ;; is given a list here.  The
                                         ;; format is the one the domain
                                         ;; reads back: one status to a
                                         ;; line, its five fields separated
                                         ;; by `|'.
                                         ,(textarea #:label (G_
                                                             "ticket statuses")
                                                    #:name 'ticket-statuses
                                                    #:value ticket-statuses
                                                    #:required #f)
                                         (p (@ (class ,(mk-class '(text-sm
                                                                   text-onyx-500
                                                                   dark:text-stone-500))))
                                            ,(G_
                                              "One status to a line: value | label | icon | glyph | bucket, where bucket is open, done or rejected. Leave empty to use the instance's statuses."))
                                         (div (@ (class ,(mk-class '(flex
                                                                     flex-row
                                                                     gap-3
                                                                     items-center
                                                                     mt-2))))
                                              ,(submit-button (G_
                                                               "save project"))
                                              (a (@ (href "/projects")
                                                    (class ,(mk-class '(text-sm
                                                                        text-onyx-700
                                                                        dark:text-stone-300
                                                                        underline))))
                                                 ,(G_ "cancel"))))))
                 #:proj projects))
