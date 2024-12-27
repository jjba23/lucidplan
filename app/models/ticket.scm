(import (artanis mvc model))

(create-artanis-model ticket
                      (:deps project)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (project-id char-field
                                  (#:not-null #:maxlen 36))
                      (human-id small-integer
                                (#:not-null))
                      (title char-field
                             (#:not-null #:maxlen 512))
                      (description char-field
                                   (#:not-null #:maxlen 4096))
                      (ticket-status char-field
                                     (#:not-null #:maxlen 256))
                      (ticket-type char-field
                                   (#:not-null #:maxlen 256)))

