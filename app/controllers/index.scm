;; lucidplan - free and open project management for everyone
;;
;; SPDX-LICENSE-IDENTIFIER: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-artanis-controller index)

(import (lucidplan html)
        (lucidplan config)
        (web uri)
        (ice-9 string-fun)
        (lucidplan sqlite project)
        (lucidplan domain project)
        (app views index)
        (app views about)
        (app views error))

;; The pages the server answers a refusal with are not routes -- an unmatched
;; path and a handler that threw never reach one -- so they are registered
;; with the framework here, while the application is being loaded.  See
;; `app/views/error.scm' for the pages themselves.
(register-error-pages!)

;; The route reads the `lang' cookie for two things: as the translator it
;; renders with -- `#:i18n' -- and as the row the language control shows,
;; which is the language this very page came out in.
(get "/"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan lang)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (proj (get-projects project-repo)))
         (response-emit (:mime rc
                               (welcome-page (:i18n rc)
                                             #:projects proj
                                             #:selected-locale (:cookies-value
                                                                rc "lang")
                                             #:is-logged-in (:cookies-value rc
                                                             "x-bewaking-lucidplan")))))))

;; The about page names the instance and says what it is.  Like the front
;; page it is public: a reader deciding whether to trust an instance should
;; be able to read what it is before signing in.
(get "/about"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-lucidplan lang)
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (proj (get-projects project-repo)))
         (response-emit (:mime rc
                               (about-page (:i18n rc)
                                           #:projects proj
                                           #:is-logged-in (:cookies-value rc
                                                           "x-bewaking-lucidplan")))))))

;; A sitemap is how a search engine finds the pages a reader could have
;; linked to: the front page, the about page, the project list, and each
;; project's backlog and board.  Everything that needs a sign-in is left
;; out -- it is not a page a crawler can read.
(get "/sitemap.xml"
     #:mime 'xml
     (lambda (rc)
       (let* ((project-repo (make-sqlite-project-repository rc))
              (projects (get-projects project-repo))
              (paths (append '("/" "/about" "/projects")
                             (map (lambda (p)
                                    (format #f "/tickets/~a"
                                            (assq-ref p
                                                      'human-id))) projects)
                             (map (lambda (p)
                                    (format #f "/kanban/~a"
                                            (assq-ref p
                                                      'human-id))) projects))))
         ;; The document is serialized here and sent with its own XML
         ;; content-type rather than through `:mime': a sitemap is XML, and
         ;; the HTML rendering the route would otherwise carry would be a
         ;; document a crawler is entitled to reject.
         (response-emit (string-append
                         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                         (render-page `(urlset (@ (xmlns
                                                   "http://www.sitemaps.org/schemas/sitemap/0.9"))
                                               ,@(map (lambda (path)
                                                        `(url (loc ,(absolute-url
                                                                     path))))
                                                      paths))))
                        #:headers '((content-type application/xml))))))

;; The robots file is built rather than shipped as a file so that the sitemap
;; it points at is the instance's own absolute address, and so the paths that
;; only make sense to a signed-in reader stay out of the index.
(get "/robots.txt"
     #:mime 'xml
     (lambda (rc)
       (response-emit (string-append "User-agent: *\n"
                                     "Allow: /\n"
                                     "Disallow: /users\n"
                                     "Disallow: /me\n"
                                     "Disallow: /password\n"
                                     "Disallow: /logout\n\n"
                                     "Sitemap: "
                                     (absolute-url "/sitemap.xml")
                                     "\n")
                      #:headers '((content-type text/plain)))))

(get "/favicon.ico" static-page-emitter)
