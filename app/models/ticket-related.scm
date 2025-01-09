(import (artanis mvc model))

(create-artanis-model ticket_related
                      (:deps ticket)
                      (parent_id char-field
                                 (#:not-null #:maxlen 36))
                      (related_id char-field
                                  (#:not-null #:maxlen 36)))

