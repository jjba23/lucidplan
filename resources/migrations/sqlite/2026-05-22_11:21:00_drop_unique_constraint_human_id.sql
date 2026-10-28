-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

PRAGMA foreign_keys=off;

ALTER TABLE ticket RENAME TO old_ticket;

CREATE TABLE "ticket" (
	"human_id"	integer,
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"title"	TEXT NOT NULL,
	"description"	TEXT,
	"ticket_status"	varchar(256) NOT NULL,
	"ticket_type"	varchar(256) NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"priority"	INTEGER,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY(id)
);

INSERT INTO ticket SELECT * FROM old_ticket;

PRAGMA foreign_keys=on;
