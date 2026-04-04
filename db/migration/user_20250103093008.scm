(create-artanis-migration user_20250103093008)

(use-modules (artanis artanis)
             (artanis security nss))

(define* (add-test-user! #:key name
                         email
                         password
                         salt
                         permissions)
  (let ((mt (map-table-from-DB (get-conn-from-pool!)))
        (real-pass (string->sha-256 (string-append password salt))))
    (mt 'set
        'user
        #:username name
        #:email email
        #:password real-pass
        #:salt salt
        #:permissions permissions)))

(migrate-up (display "Adding user...")
            (add-test-user! #:name "Joe"
             #:email "jjbigorra@gmail.com"
             #:password ""
             #:salt ""
             ;; note: the ` is important due to Lisp quasi-quotation
             ;; so as to have it interpreted as a list of symbols
             #:permissions
             "`(lucidplan:modify-permissions\n                                             lucidplan:ticket-create lucidplan:ticket-delete\n                                             lucidplan:project-create lucidplan:project-delete\n                                             lucidplan:user-create lucidplan:user-delete)")
            (display "done.\n"))

