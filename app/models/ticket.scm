(import (artanis mvc model))

(create-artanis-model
 ticket
 (:deps)
 (id char-field (#:primary-key #:maxlen 36))
 (human-id auto)
 (title char-field (#:not-null #:maxlen 256))
 (description char-field (#:maxlen 4096))
 )

