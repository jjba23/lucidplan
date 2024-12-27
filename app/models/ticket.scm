(import (artanis mvc model))

(create-artanis-model ticket
                      (:deps project)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (project_id char-field
                                  (#:not-null #:maxlen 36 #:unique))
                      (human_id small-integer
                                (#:not-null #:unique))
                      (title char-field
                             (#:not-null #:maxlen 512 #:unique))
                      (description char-field
                                   (#:not-null #:maxlen 4096))
                      (ticket_status char-field
                                     (#:not-null #:maxlen 256))
                      (ticket_type char-field
                                   (#:not-null #:maxlen 256))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field))

