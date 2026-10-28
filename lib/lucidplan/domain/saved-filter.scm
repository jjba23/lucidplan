;; lucidplan - free and open project management for everyone
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; A name for a narrowing of the backlog.
;;;
;;; The backlog is already narrowable -- by epic, by label, by a search, by
;;; which buckets are shown -- and every one of those narrowings lives in the
;;; query string, which is to say in the address, which is to say it is gone
;;; the moment the reader goes anywhere else.  A saved filter is a name that
;;; makes one of them stay.
;;;
;;; It holds the query as *text* rather than as a parsed tree, and that is the
;;; whole design: the text is exactly what the backlog already understands, so
;;; a saved filter can never mean something the page that reads it cannot ask
;;; for.  What that costs is that the text is checked before it is kept (see
;;; `filter-query-ok?') -- a query string is somebody's typing, not a promise.

(define-module (lucidplan domain saved-filter)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (lucidplan domain repo)
  #:export (get-saved-filters get-saved-filter-by-id
                              insert-saved-filter soft-delete-saved-filter))

(define-record-with-kw (<saved-filter-repository>
                        %make-saved-filter-repository
                        saved-filter-repository?)
                       mk-saved-filter-repository
                       (get-saved-filters-proc repo-get-saved-filters)
                       (get-saved-filter-by-id-proc
                        repo-get-saved-filter-by-id)
                       (insert-saved-filter-proc repo-insert-saved-filter)
                       (soft-delete-saved-filter-proc
                        repo-soft-delete-saved-filter))

(define-repo-method get-saved-filters repo-get-saved-filters
 "Retrieves the saved filters of a project from REPO, by name.

They belong to the project rather than to the person who saved one, because a
narrowing of a backlog is a fact about the backlog -- `everything blocked on
ops' is as true for the next reader as for the one who first asked it.")

(define-repo-method get-saved-filter-by-id repo-get-saved-filter-by-id
 "Retrieves one saved filter by ID from REPO, or @code{#f}.")

(define-repo-method insert-saved-filter repo-insert-saved-filter
 "Inserts a new saved filter using ATTRS into REPO.")

(define-repo-method soft-delete-saved-filter repo-soft-delete-saved-filter
 "Marks the saved filter ID as removed in REPO.")
