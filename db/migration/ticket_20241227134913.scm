(create-artanis-migration ticket_20241227134913)

(migrate-up
 (create-table
  'ticket
  '(id char-field
       (#:primary-key #:maxlen 36))
  '(project_id char-field
               (#:not-null #:maxlen 36))
  '(human_id small-integer
             (#:not-null))
  '(title char-field
          (#:not-null #:maxlen 512))
  '(description char-field
                (#:not-null #:maxlen 4096))
  '(ticket_status char-field
                  (#:not-null #:maxlen 256))
  '(ticket_type char-field
                (#:not-null #:maxlen 256))
  '(created_at date-field
               (#:not-null))
  '(updated_at date-field)))

(migrate-down
 (drop-table 'ticket))

