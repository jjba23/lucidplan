(import (artanis mvc model))

(create-artanis-model ticket_relations
                      (:deps ticket)
                      (parent_id char-field
                                 (#:not-null #:maxlen 36))
                      (relation_id char-field
                                   (#:not-null #:maxlen 36))
                      ;; relation type is:
                      ;; - blockers
                      ;; - child
                      ;; - clone
                      ;; - related
                      (relation_type char-field (#:not-null #:maxlen 1024)))


