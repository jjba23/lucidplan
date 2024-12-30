(import (artanis mvc model))

(create-artanis-model project
                      (:deps)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (human_id char-field
                                (#:not-null #:maxlen 32
                                 #:unique))
                      (title char-field
                             (#:not-null #:maxlen 256
                              #:unique))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field))

