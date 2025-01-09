(import (artanis mvc model))

(create-artanis-model ticket_blockers
                      (:deps ticket)
                      (parent_id char-field
                                 (#:not-null #:maxlen 36))
                      (blocker_id char-field
                                  (#:not-null #:maxlen 36)))

