(create-artanis-migration project_20241227134909)

(migrate-up
 (create-table
  'project
  '(id char-field
       (#:primary-key #:maxlen 36))
  '(human_id char-field
             (#:not-null #:maxlen 32))
  '(title char-field
          (#:not-null #:maxlen 256))
  ))

(migrate-down
 (drop-table 'project))
