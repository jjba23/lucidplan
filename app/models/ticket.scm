(import (artanis mvc model))

(create-artanis-model ticket
                      (:deps project)
                      (human_id integer (#:primary-key #:auto-increment))
                      (id char-field
                          (#:unique #:not-null #:maxlen 36))
                      (project_id char-field
                                  (#:not-null #:maxlen 36))
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
                      (updated_at date-field )
                      (child_of char-field (#:maxlen 8192))
                      (blocked_by char-field (#:maxlen 8192))
                      (splits_from char-field (#:maxlen 8192))
                      (clone_of char-field (#:maxlen 8192))
                      (blocks char-field (#:maxlen 8192))
                      (related_to char-field (#:maxlen 8192)))

