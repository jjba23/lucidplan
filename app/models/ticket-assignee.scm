(import (artanis mvc model))

(create-artanis-model ticket_assignee
                      (:deps ticket)
                      (ticket_id char-field
                                 (#:not-null #:maxlen 36))
                      (user_id char-field
                                 (#:not-null #:maxlen 36)))

