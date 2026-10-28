-- lucidplan - free and open project management for everyone
--
-- SPDX-LICENSE-IDENTIFIER: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- A ticket is estimated twice over: in story points, which the team burns
-- down, and in t-shirt sizes, which is what a first pass through a backlog
-- is usually written in.  Both are optional -- an unestimated ticket is a
-- ticket nobody has looked at yet, not a ticket worth zero -- so both are
-- nullable and a missing value stays NULL rather than becoming 0 or ''.
ALTER TABLE "ticket" ADD COLUMN "estimate_points" INTEGER;

ALTER TABLE "ticket" ADD COLUMN "estimate_size" varchar(16);

-- Threaded comments: PARENT_ID names the comment a reply answers, and is
-- NULL for a comment on the ticket itself.  A comment is never rewritten by
-- the application; deleting one is a soft delete so the reply it anchors
-- keeps its place in the thread.
CREATE TABLE "ticket_comment" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,
	"parent_id"	varchar(36),
	"author_id"	varchar(36) NOT NULL,
	"body"	TEXT NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "ticket_comment_ticket_idx" ON "ticket_comment" ("ticket_id");

-- A file one ticket carries.  FILENAME is what the uploader called it and
-- what a download is named; STORED_NAME is the generated name the bytes live
-- under, so no submitted name ever reaches the filesystem as a path.
CREATE TABLE "ticket_attachment" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,
	"uploader_id"	varchar(36) NOT NULL,
	"filename"	TEXT NOT NULL,
	"stored_name"	varchar(128) NOT NULL,
	"content_type"	varchar(256),
	"size"	INTEGER,
	"created_at"	date NOT NULL,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "ticket_attachment_ticket_idx" ON "ticket_attachment" ("ticket_id");

-- The audit trail: one row per thing that happened to a ticket.  FIELD,
-- OLD_VALUE and NEW_VALUE describe a change; an event that is not a field
-- change (a comment, an attachment, a move) leaves them out.
CREATE TABLE "ticket_event" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,
	"actor_id"	varchar(36),
	"action"	varchar(64) NOT NULL,
	"field"	varchar(64),
	"old_value"	TEXT,
	"new_value"	TEXT,
	"created_at"	date NOT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "ticket_event_ticket_idx" ON "ticket_event" ("ticket_id", "created_at");

-- A snapshot of a ticket's content at one point in its life.  Versions are
-- numbered per ticket and are never edited: restoring an old version writes
-- a new version with the old content, so the history stays a history.
CREATE TABLE "ticket_revision" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,
	"version"	INTEGER NOT NULL,
	"author_id"	varchar(36),
	"title"	TEXT,
	"description"	TEXT,
	"ticket_status"	varchar(256),
	"ticket_type"	varchar(256),
	"priority"	INTEGER,
	"estimate_points"	INTEGER,
	"estimate_size"	varchar(16),
	"created_at"	date NOT NULL,
	PRIMARY KEY("id")
);

CREATE UNIQUE INDEX "ticket_revision_ticket_version_idx" ON "ticket_revision" ("ticket_id", "version");
