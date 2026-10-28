-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- Planning poker: a table a team sits at to put a number on something.
--
-- The whole section is deliberately detached from the account model.  A
-- planning session happens in a meeting, often with people who are not in
-- this instance -- a product owner, somebody from another team, a guest --
-- and a tool that asks each of them for an account before they may say "five"
-- is a tool the meeting works around.  So a participant here is a *name and a
-- secret*, not a user: `poker_participant.token' is what the browser holds in
-- a cookie and what says which of the seats is yours, and no row in this file
-- points at `user'.
--
-- Three tables, because the three things they keep answer three questions.
-- A *room* is where a team meets ("Sprint 12 planning") and it chooses the
-- deck it estimates with -- the numbers a team burns down, or the t-shirt
-- sizes a first pass through a backlog is written in.  An *item* is one
-- thing that room put a number on, and the round it was asked in: a session
-- is not one question, it is a question, a reveal, a discussion and the next
-- question, so the item carries both what was asked and when it was asked.
-- A *vote* is one participant's card on one item, and the card's face is
-- hidden until the item is revealed -- which is why the reveal is a column
-- of the item, set when the room is ready, rather than anything the voter
-- controls.

-- The room.  TITLE is what the meeting is called ("Sprint 12 planning"),
-- never the thing being estimated: the question asked changes every round
-- and belongs to the round (see `poker_item' below), while the name of the
-- meeting outlives all of them.  DECK is the one thing about a room that
-- does not change from round to round: a team that estimates in t-shirt
-- sizes does so all afternoon, and a room that changed its ladder halfway
-- through would leave its own record of what was agreed unreadable.  DECK is
-- a string rather than an enum because SQLite has none, and because the
-- ladders themselves are configuration (see `ticket-points' and
-- `ticket-sizes' in `(lucidplan domain)') -- this column names which of them,
-- never what is in one.
CREATE TABLE "poker_room" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"title"	varchar(256) NOT NULL,
	"deck"	varchar(32) NOT NULL DEFAULT 'points',
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"deleted_at"	date DEFAULT NULL,
	PRIMARY KEY("id")
);

-- Whatever is not deleted is a table somebody may still sit at, newest
-- first, which is the only order the index is asked for.
CREATE INDEX "poker_room_created_idx" ON "poker_room" ("created_at");

-- One thing the room put a number on, in the round it was asked in.
-- ROUND starts at 1 and counts up; a unique index over the pair keeps a room
-- from holding two first rounds, which would leave "the current item" a
-- question with two answers.  REVEALED_AT is NULL while the cards are face
-- down and holds the moment they were turned over once the room is ready --
-- a timestamp rather than a flag so the room's record says when it was
-- agreed, not merely that it was.
CREATE TABLE "poker_item" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"room_id"	varchar(36) NOT NULL,
	"round"	INTEGER NOT NULL,
	"title"	varchar(256) NOT NULL,
	"revealed_at"	date,
	"created_at"	date NOT NULL,
	PRIMARY KEY("id")
);

CREATE UNIQUE INDEX "poker_item_room_round_idx" ON "poker_item" ("room_id", "round");

-- A seat at a room: a name as it was typed, and the secret the browser
-- carries.  TOKEN is unique across the whole table rather than per room,
-- because it is what a request is resolved to before the room in the URL is
-- even consulted; a token that could name two seats would make every lookup
-- ambiguous.  Nothing here says who the person is, and nothing needs to: the
-- seat is theirs for as long as they hold the token.
CREATE TABLE "poker_participant" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"room_id"	varchar(36) NOT NULL,
	"name"	varchar(64) NOT NULL,
	"token"	varchar(64) NOT NULL UNIQUE,
	"joined_at"	date NOT NULL,
	"last_seen_at"	date,
	PRIMARY KEY("id")
);

CREATE INDEX "poker_participant_room_idx" ON "poker_participant" ("room_id");

-- One card.  VALUE is stored as text in both decks -- "5" and "m" -- because
-- the two ladders are different kinds of thing and a column that held a
-- number for one of them would have to be null for the other, which is how a
-- size becomes unreadable to the very code that wrote it.  The unique index
-- is what makes a vote a *seat's current card* rather than a stack of cards:
-- changing your mind rewrites this row instead of leaving the room to guess
-- which of two cards a person meant.
CREATE TABLE "poker_vote" (
	"id"	varchar(36) NOT NULL UNIQUE,
	"item_id"	varchar(36) NOT NULL,
	"participant_id"	varchar(36) NOT NULL,
	"value"	varchar(16) NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	PRIMARY KEY("id")
);

CREATE UNIQUE INDEX "poker_vote_item_participant_idx" ON "poker_vote" ("item_id", "participant_id");
