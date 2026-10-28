-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- Epics: the work a ticket is a piece of.
--
-- A backlog says what has to be done and a board says where each piece stands,
-- but neither says what the pieces are *for*.  An epic is that: one body of
-- work -- "sign-in", "the import", "everything before the launch" -- that a
-- ticket belongs to and that is finished when its tickets are.  It is the
-- level above the ticket and the level below the project, which is the one a
-- plan is usually argued about.
--
-- An epic is not a ticket and does not share its table: it has no status of
-- its own, no assignee and no estimate, because all of those are what its
-- tickets are for.  What it has is a name, an optional account of itself, and
-- the tickets that point at it -- so its progress is counted rather than kept,
-- and cannot disagree with the tickets that make it up.
CREATE TABLE "epic" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"human_id"	varchar(64) NOT NULL,
	"title"	varchar(256) NOT NULL,
	"description"	varchar(4096) DEFAULT '',
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY("id")
);

-- An epic is named by a slug inside its project, the way a ticket is named by
-- a number inside its project: two projects may each have a "sign-in", and
-- neither may have two.
CREATE UNIQUE INDEX "epic_project_human_idx" ON "epic" ("project_id", "human_id");
CREATE INDEX "epic_project_idx" ON "epic" ("project_id");

-- The epic a ticket belongs to, or nothing at all -- which is the ordinary
-- ticket rather than a special state.  The empty string rather than NULL is
-- deliberate: it is the value a form that offers "no epic" submits, it is what
-- a row that has never had one carries, and it means every write can go
-- through the same builders as every other column instead of needing a
-- statement of its own to say "nothing".
ALTER TABLE "ticket" ADD COLUMN "epic_id" varchar(36) DEFAULT '';
