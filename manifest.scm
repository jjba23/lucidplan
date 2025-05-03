(use-modules (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (guix build-system gnu)
             (gnu packages guile)
             (gnu packages curl)
             (gnu packages bash)
             (gnu packages nss)
             (gnu packages gnupg)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (guix build-system guile)
             (gnu packages linux)
             (gnu packages perl)
             (gnu packages compression)
             (gnu packages sqlite)
             (gnu packages texinfo)
             (gnu packages autotools)
             (guix packages)
             ((guix licenses)
              #:prefix license:))

(define-public guile-uuid
  (let ((commit "64002d74025f577e1eeea7bc51218a2c7929631f")
        (revision "0"))
    (package
      (name "guile-uuid")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/elb/guile-uuid.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q6dqm2hzq75aa5mrrwgqdml864pdrxc98j7pyj1y0827phnzjfj"))))
      (build-system guile-build-system)
      (native-inputs (list guile-3.0 guile-gcrypt))
      (home-page "https://codeberg.org/elb/guile-uuid")
      (synopsis
       "Guile-UUID is a UUID generation and manipulation module for GNU Guile.")
      (description
       "This package implements RFC 9562 UUIDs, and can generate versions 1 and 3–8 from that specification.
        It provides parsing for UUIDs in standard hex-and-dash format of any variant and version.
        It can also query the variant and version of UUIDs from the RFC.
        Simple routines for converting between binary and hex-and-dash string UUIDs are included.")
      (license license:gpl3+))))

(define-public guile-hygguile
  (let ((commit "a5e617a6e37d54ad83c480045f9791a0e0f1f334")
        (base32-sha-signature
         "14pha7pn5p22gchkf4f2krgwkszizl9vyblwxwn8g5k97mmw8faq")
        (revision "0"))
    (package
      (name "guile-hygguile")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/jjba23/hygguile.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 base32-sha-signature))))
      (build-system guile-build-system)
      (native-inputs (list guile-3.0))
      (arguments
       (list
        #:source-directory "src"))
      (home-page "https://codeberg.org/jjba23/hygguile")
      (synopsis
       "SXML and TailwindCSS UI component library for Lisp (Guile Scheme) software projects")
      (description
       "Cozy and professional user-interfaces for everyone.
        SXML and TailwindCSS UI component library for Lisp (Guile Scheme) software projects.
        hygge + guile = hygguile")
      (license license:lgpl3+))))

(define-public guile-kracht
  (let ((commit "428062660a31f7235125751b8e9343afdea9d9e1")
        (base32-sha-signature
         "0m9flnvhal62ik7k2sm2mhbrikzwvgdjpyr6s558z6d2ax41ikjd")
        (revision "0"))
    (package
     (name "guile-kracht")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/kracht.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 base32-sha-signature))))
     (build-system guile-build-system)
     (native-inputs (list guile-3.0))
     (arguments
      (list
       #:source-directory "src"))
     (home-page "https://codeberg.org/jjba23/kracht")
     (synopsis
      "Good prelude for Guile Scheme, an opt-in enhancement to the standard library")
     (description
      "Good prelude for Guile Scheme, an opt-in enhancement to the standard library. kracht means power/strength in the Dutch language, and is also a recursive acronym which stands for: KRACHT Reliable And Consistent Helper Tools")
     (license license:lgpl3+))))


(define-public guile-orgwebalchemy
  (let ((commit "e97ee7bfe85d6dfde9e126899f9800cd36a25c2d")
        (base32-sha-signature
         "0m9flnvhal62ik7k2sm2mhbrikzwvgdjpyr6s558z6d2ax41ikjd")
        (revision "0"))
    (package
     (name "guile-kracht")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/kracht.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 base32-sha-signature))))
     (build-system guile-build-system)
     (native-inputs (list guile-3.0))
     (arguments
      (list
       #:source-directory "src"))
     (home-page "https://codeberg.org/jjba23/kracht")
     (synopsis
      "Good prelude for Guile Scheme, an opt-in enhancement to the standard library")
     (description
      "Good prelude for Guile Scheme, an opt-in enhancement to the standard library. kracht means power/strength in the Dutch language, and is also a recursive acronym which stands for: KRACHT Reliable And Consistent Helper Tools")
     (license license:lgpl3+))))



;;
;; define manifest
;;

(packages->manifest (list (specification->package "make")
                          (specification->package "gettext")
                          guile-next
                          guile-ares-rs
                          artanis
                          guile-hygguile
                          guile-kracht
                          guile-uuid
                          guile-dbi
                          guile-orgwebalchemy
                          sqlite
                          guile-dbd-sqlite3))

