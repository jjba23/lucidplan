;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller projects)

(import (app views projects)
        (artanis security nss)
        (artanis upload)
        (ice-9 string-fun)
        (ice-9 binary-ports)
        (rnrs bytevectors)
        (srfi srfi-1)
        (kracht prelude)
        (lucidplan auth)
        (lucidplan attachments)
        (lucidplan avatars)
        (lucidplan sqlite util)
        (lucidplan sqlite project)
        (lucidplan domain project)
        (uuid generate)
        (web uri)
        (web uri))

(get "/projects"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-p)
         (let* ((is-logged-in (:cookies-value rc "x-bewaking-lucidplan"))
                (user-search (cond
                               ((equal? #f
                                        (get-from-qstr rc "search"))
                                #f)
                               (else (html-escape (get-from-qstr rc "search")))))
                (project-repo (make-sqlite-project-repository rc))
                (proj (cond
                        ((nil? user-search)
                         (get-projects project-repo))
                        (else (search-projects project-repo
                                               (html-escape user-search))))))
           (response-emit (:mime rc
                                 (html-from-projects (:i18n rc)
                                                     #:projects proj
                                                     #:search-query
                                                     user-search
                                                     #:is-logged-in
                                                     is-logged-in)))))
       (get-p)))

;; The page that offers to create a project asks for the same permission the
;; write behind it does, so a reader who may not create a project is turned
;; away before they fill in a form that could only ever refuse them.
(get "/projects/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-new)
         (let* ((project-repo (make-sqlite-project-repository rc)))
           (response-emit (:mime rc
                                 (html-new-project-form (:i18n rc)
                                                        #:is-logged-in (:cookies-value
                                                                        rc
                                                                        "x-bewaking-lucidplan")
                                                        #:projects (get-projects
                                                                    project-repo))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:project-create)
                          #:rc rc
                          #:happy-flow get-new
                          #:unhappy-flow back-to-login)))

(post "/projects/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id"))
                 (execute-write (db-write (DB-open rc))))
            (execute-write "soft-delete-project"
                           (sql-update 'project
                                       `((deleted-at unquote
                                                     (current-time)))
                                       #:where (list (cons 'id id))))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))

(post "/projects/new"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-new)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (url (uri-encode (assoc-ref body "url")))
                 (vcs-url (uri-encode (assoc-ref body "vcs-url")))
                 (human-id (assoc-ref body "human-id"))
                 ;; The statuses this project's own tickets move through, as
                 ;; the operator wrote them -- the same field the edit page
                 ;; offers, so a project can be created with its own ladder
                 ;; rather than only given one afterwards.  Nothing here
                 ;; means the project uses the instance's statuses.
                 (ticket-statuses (assoc-ref body "ticket-statuses"))
                 (new-uuid (string-replace-substring (generate-string-uuid)
                                                     "\n" ""))
                 (project-repo (make-sqlite-project-repository rc)))
            
            (insert-project project-repo
                            `((id unquote new-uuid)
                              (human-id unquote human-id)
                              (title unquote title)
                              (description unquote description)
                              (url unquote url)
                              (vcs-url unquote vcs-url)
                              (ticket-statuses unquote
                                               (if ticket-statuses
                                                   ticket-statuses ""))
                              (created-at unquote
                                          (current-time))))
            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-new
                           #:unhappy-flow back-to-login)))

;; Editing a project is guarded with the permission that writes it -- there
;; is no separate project-edit permission -- so the form and the endpoint it
;; posts to answer to exactly the same rule.
(get "/projects/id/:human-id/edit"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan)
     (lambda (rc)
       (define (get-edit)
         (let* ((project-repo (make-sqlite-project-repository rc))
                (p (get-project-by-human-id project-repo
                                            (params rc "human-id"))))
           (response-emit (:mime rc
                                 (html-edit-project-form (:i18n rc)
                                                         #:is-logged-in (:cookies-value
                                                                         rc
                                                                         "x-bewaking-lucidplan")
                                                         #:projects (get-projects
                                                                     project-repo)
                                                         #:project p
                                                         ;; Why the last
                                                         ;; upload did not
                                                         ;; land, as the route
                                                         ;; that refused it
                                                         ;; named it on the way
                                                         ;; back here.
                                                         #:avatar-error (get-from-qstr
                                                                        rc
                                                                        "avatar-error"))))))
       (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                          #:needed-permissions '(lucidplan:project-create)
                          #:rc rc
                          #:happy-flow get-edit
                          #:unhappy-flow back-to-login)))

(post "/projects/id/:human-id/edit"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-edit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (title (html-escape (assoc-ref body "title")))
                 (description (html-escape (assoc-ref body "description")))
                 (url (uri-encode (assoc-ref body "url")))
                 (vcs-url (uri-encode (assoc-ref body "vcs-url")))
                 (human-id (assoc-ref body "human-id"))
                 ;; The statuses this project's own tickets move through, as
                 ;; the operator wrote them, or nothing -- which is the row a
                 ;; project that uses the instance's statuses keeps.  The
                 ;; text is stored as it was typed, so the editor round-trips
                 ;; exactly what was entered, and it is the domain that reads
                 ;; it back (see `parse-ticket-statuses').
                 (ticket-statuses (assoc-ref body "ticket-statuses"))
                 (project-repo (make-sqlite-project-repository rc)))
            (update-project-by-human-id project-repo
                                        (params rc "human-id")
                                        `((human-id unquote human-id)
                                          (url unquote url)
                                          (vcs-url unquote vcs-url)
                                          (description unquote description)
                                          (title unquote title)
                                          (ticket-statuses unquote
                                                           (if ticket-statuses
                                                            ticket-statuses ""))
                                          (updated-at unquote
                                                      (current-time))))

            (redirect-to rc
                         (string->uri (format #f "~a/projects"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-edit
                           #:unhappy-flow back-to-login)))

;; ===== The picture a project wears =====
;;
;; Three endpoints, and each of them is one of the three things a picture in a
;; list has to be able to do: be given, be taken away, and be fetched.
;;
;; They answer the permission the edit page answers -- `lucidplan:project-
;; create', which is the permission that writes a project, since there is no
;; separate one for editing -- except the last of them, which is public: the
;; list of projects is public (it is on the front page), so the pictures in it
;; cannot be behind a sign-in.
;;
;; The bytes go straight into the project's own row, read out of the request
;; body rather than out of a file: nothing is written to a directory on the
;; way, so nothing is left behind by an upload that failed halfway, and a
;; picture travels with a copy of the database.  What is *not* kept is what the
;; upload claimed: the type is checked against the four this instance keeps and
;; the bytes are filed under that one, so neither the upload's filename nor its
;; claimed type ever becomes a path or a header.
;;
;; Like the ticket's files, every way this can end is a redirect back to the
;; edit page, with the reason in the query string when there is one.  A route
;; that raises is answered with the framework's error page, which is written
;; without a length and without closing the connection, and a browser still
;; sending the file waits for an end that never comes -- an upload that hangs
;; having already been stored, or not stored at all.
(post "/projects/id/:human-id/avatar"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (back . error)
          (string->uri (format #f "~a/projects/id/~a/edit~a"
                               (current-myhost)
                               (params rc "human-id")
                               (if (null? error)
                                   ""
                                   (format #f "?avatar-error=~a"
                                           (car error))))))
        (define (do-upload)
          (catch #t
            (lambda ()
              (let* ((human-id (params rc "human-id"))
                     (project-repo (make-sqlite-project-repository rc))
                     (p (get-project-by-human-id project-repo human-id))
                     ;; #f rather than a throw when the framework did not read
                     ;; a body at all: see `uploaded-file-of'.
                     (file (and p (uploaded-file-of rc)))
                     (type (and file (avatar-canonical-type (mfd-type file))))
                     (bytes (and file type (uploaded-file-bytes rc file)))
                     (size (and bytes (bytevector-length bytes))))
                (cond
                  ;; Nothing to put it on: the project is not there, or is
                  ;; gone.  Answered, rather than left without a response.
                  ((not p)
                   (redirect-to rc
                                (string->uri (format #f "~a/projects"
                                                     (current-myhost)))
                                #:status 302))
                  ((not file)
                   (redirect-to rc (back "no-file") #:status 302))
                  ;; What came back is not a picture this instance keeps.  It
                  ;; is refused rather than kept under a type it is not.
                  ((not type)
                   (redirect-to rc (back "type") #:status 302))
                  ((not (number? size))
                   (redirect-to rc (back "failed") #:status 302))
                  ((<= size 0)
                   (redirect-to rc (back "empty") #:status 302))
                  ((> size (avatar-max-bytes))
                   (redirect-to rc (back "size") #:status 302))
                  (else
                   (update-project-by-human-id project-repo human-id
                                               `((avatar unquote
                                                         (avatar-token))
                                                 (avatar-data unquote
                                                              (bytes->base64
                                                               bytes))
                                                 (avatar-content-type unquote
                                                                      type)
                                                 (updated-at unquote
                                                             (current-time))))
                   (redirect-to rc (back) #:status 302)))))
            (lambda (key . args)
              ;; Said on the error port -- an upload that fails without a word
              ;; is one nobody can fix -- and answered with the page the
              ;; reader came from.
              (format (current-error-port)
                      "picture upload failed for project ~a: ~a ~a~%"
                      (params rc "human-id") key args)
              (redirect-to rc (back "failed") #:status 302))))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-upload
                           #:unhappy-flow back-to-login)))


(post "/projects/id/:human-id/avatar/delete"
      #:mime 'xml
      #:cookies '(names x-bewaking-lucidplan)
      (lambda (rc)
        (define (do-remove)
          (let* ((human-id (params rc "human-id"))
                 (project-repo (make-sqlite-project-repository rc)))
            (when (get-project-by-human-id project-repo human-id)
              ;; The row is emptied outright rather than given a value that
              ;; stands for nothing: a project with no picture is a project
              ;; whose row says nothing about one, and the letter the pages
              ;; draw is generated from the name rather than kept here.
              ((db-write (DB-open rc)) "clear-project-avatar"
               (sql-parameterize "UPDATE project SET avatar = NULL, avatar_data = NULL, avatar_content_type = NULL, updated_at = ? WHERE human_id = ?"
                                 (list (current-time) human-id))))
            (redirect-to rc
                         (string->uri (format #f "~a/projects/id/~a/edit"
                                              (current-myhost) human-id))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-lucidplan
                           #:needed-permissions '(lucidplan:project-create)
                           #:rc rc
                           #:happy-flow do-remove
                           #:unhappy-flow back-to-login)))

;; The picture itself.
;;
;; It is served for ever -- `immutable' -- and that is only safe because the
;; address a page names carries the token of the version it is showing (see
;; `project-avatar'): a new picture is a new token is a new address, and a
;; reader holding an old page asks for the old one and gets what it always
;; meant.  A project with no picture is not an error here -- it is drawn as its
;; letter avatar and nothing asks for this address at all -- but a request that
;; lands on one anyway is answered as what it is: a picture that is not there.
(get "/projects/id/:human-id/avatar"
     #:mime 'xml
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (picture (get-project-avatar project-repo
                                           (params rc "human-id")))
              (data (and picture
                         (assq-ref picture
                                   'avatar-data)))
              (type (and picture
                         (assq-ref picture
                                   'avatar-content-type))))
         (if (and (string? data)
                  (not (string-null? data))
                  (string? type))
             ;; A header's value is a *shape* here, not a line of text: Guile
             ;; validates each one against the grammar it knows, and
             ;; `cache-control' is a list of directives -- bare symbols for the
             ;; ones without a value, pairs for the ones with -- rather than
             ;; the text `public, max-age=...'.  Written as text it is refused
             ;; with "Bad value for header", the same lesson the attachment
             ;; route learned about `content-type'.
             (response-emit (base64->bytes data)
                            #:headers `((content-type
                                         ,(string->symbol type))
                                        (cache-control
                                         public
                                         (max-age . 31536000)
                                         immutable)))
             (response-emit ""
                            #:status 404)))))

