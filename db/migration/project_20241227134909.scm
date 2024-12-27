(create-artanis-migration project_20241227134909)

(migrate-up (create-table 'project
                          '(id char-field
                               (#:primary-key #:maxlen 36))
                          '(human_id char-field
                                     (#:not-null #:maxlen 32 #:unique))
                          '(title char-field
                                  (#:not-null #:maxlen 256 #:unique))
                          '(created_at date-field
                                       (#:not-null))
                          '(updated_at date-field)))

(migrate-down (drop-table 'project))
