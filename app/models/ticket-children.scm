(import (artanis mvc model))

(create-artanis-model ticket_children
                      (:deps ticket)
                      (parent_id char-field
                                 (#:not-null #:maxlen 36))
                      (child_id char-field
                                (#:not-null #:maxlen 36)))

