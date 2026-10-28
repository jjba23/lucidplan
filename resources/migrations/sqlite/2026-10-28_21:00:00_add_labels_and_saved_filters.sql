-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- Labels: the words a reader hangs on a ticket that nothing else in the schema
-- has a place for.  `bug', `needs-design', `blocked-on-ops' -- a project's own
-- vocabulary, which no ladder can anticipate and every team invents.  They are
-- kept as the text they were typed as, comma-separated, for the same reason a
-- project's status ladder is: what a reader wrote is what an editor is shown
-- back, and what they mean is decided where they are read (see `parse-labels').
ALTER TABLE "ticket" ADD COLUMN "labels" varchar(512) DEFAULT '';

-- Saved filters: a name for a narrowing of the backlog.
--
-- The backlog can already be narrowed -- by epic, by label, by a search, by
-- which buckets are shown -- and every one of those narrowings lives in the
-- query string.  What it cannot do is remember one.  A saved filter is that: a
-- name a reader gives to a query string, of their own, beside the panel it
-- narrows.  It holds the query as text rather than as a parsed tree because
-- the text is exactly what the backlog already understands, so a filter can
-- never mean something the page that reads it cannot ask for.
CREATE TABLE "saved_filter" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"name"	varchar(128) NOT NULL,
	"query"	varchar(1024) NOT NULL DEFAULT '',
	"created_at"	date NOT NULL,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "saved_filter_project_idx" ON "saved_filter" ("project_id");
