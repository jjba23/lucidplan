(import (artanis mvc model))

(create-artanis-model ticket_clones
                      (:deps ticket)
                      (parent_id char-field
                                 (#:not-null #:maxlen 36))
                      (clone_id char-field
                                (#:not-null #:maxlen 36)))

