PRAGMA foreign_keys=off;

ALTER TABLE tickets RENAME TO old_tickets;

CREATE TABLE "ticket" (
	"human_id"	integer,
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"title"	varchar(512) NOT NULL UNIQUE,
	"description"	varchar(4096),
	"ticket_status"	varchar(256) NOT NULL,
	"ticket_type"	varchar(256) NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"priority"	INTEGER,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY(id)
);

INSERT INTO tickets SELECT * FROM old_tickets;

PRAGMA foreign_keys=on;
