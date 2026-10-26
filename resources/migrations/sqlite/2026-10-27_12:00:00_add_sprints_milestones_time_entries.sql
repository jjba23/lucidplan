-- MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

-- Copyright © Josep Jesus Bigorra Algaba

-- mutastructura is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- mutastructura is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

-- The three things a team plans with, added in one migration because they
-- arrive together: a sprint a ticket is worked in, a milestone it counts
-- toward, and the time somebody actually spent on it.
--
-- One convention is worth stating once here, because every column below
-- follows it.  A calendar date -- the day a sprint starts, the day a
-- ticket is due, the day time was spent -- is stored as the ISO text
-- `2026-10-27', not as a number.  ISO text is what sorts correctly, what
-- compares correctly with an index, and what an `<input type="date">'
-- reads and writes, so no page has to convert on the way in or out.  A
-- timestamp -- when a row was written -- is stored the way every other
-- `created_at' in this schema is: as the number of seconds since the
-- epoch.  The two are deliberately different because they answer
-- different questions; a date is a day, a timestamp is a moment.

-- One stretch of work a team commits to, with the two ends of it.  The
-- ends are nullable because a sprint is often written down before anybody
-- agrees on the dates; `sprint_status' says how far along it is, and is a
-- string rather than an enum because SQLite has none and the ladder is
-- configuration (see `sprint-statuses' in `(lucidplan domain)').
CREATE TABLE "sprint" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"title"	varchar(256) NOT NULL,
	"goal"	TEXT,
	"starts_on"	date,
	"ends_on"	date,
	"sprint_status"	varchar(32) NOT NULL DEFAULT 'planned',
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "sprint_project_idx" ON "sprint" ("project_id", "sprint_status");

-- A point in a project's future that tickets count toward.  A milestone
-- is not a sprint: several sprints may feed one milestone, and a
-- milestone's date is the one that is promised outside the team, which is
-- why it has a date of its own rather than borrowing a sprint's.
CREATE TABLE "milestone" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"title"	varchar(256) NOT NULL,
	"description"	TEXT,
	"due_on"	date,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "milestone_project_idx" ON "milestone" ("project_id", "due_on");

-- What somebody did and how long it took.  The duration is kept in whole
-- minutes rather than as a pair of clock times: a team logs "an hour and a
-- half on the login bug", not "from 10:12 until 11:42", and a duration
-- cannot be wrong about a time zone.  The comment is what the entry is
-- *for* -- a time log that cannot say what the hour bought is a timesheet,
-- not a record of work -- so it is a column of the entry rather than a
-- message somewhere else.
CREATE TABLE "time_entry" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,
	"author_id"	varchar(36),
	"duration_minutes"	INTEGER NOT NULL,
	"spent_on"	date NOT NULL,
	"comment"	TEXT,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE INDEX "time_entry_ticket_idx" ON "time_entry" ("ticket_id", "spent_on");
CREATE INDEX "time_entry_project_idx" ON "time_entry" ("project_id", "spent_on");

-- A ticket's place in that plan.  All three are nullable: a ticket that is
-- not in a sprint, counts toward no milestone and has no deadline is the
-- ordinary ticket, and its columns stay NULL rather than carrying a
-- stand-in value the pages would have to recognise.
ALTER TABLE "ticket" ADD COLUMN "due_on" date;

ALTER TABLE "ticket" ADD COLUMN "sprint_id" varchar(36);

ALTER TABLE "ticket" ADD COLUMN "milestone_id" varchar(36);

CREATE INDEX "ticket_sprint_idx" ON "ticket" ("sprint_id");

CREATE INDEX "ticket_milestone_idx" ON "ticket" ("milestone_id");
