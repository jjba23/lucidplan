-- lucidplan - free and open project management for everyone
--
-- SPDX-LICENSE-IDENTIFIER: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- A table is often opened for one thing: somebody looking at a ticket asks the
-- team what it is worth, and the meeting that follows is about that ticket and
-- nothing else.  Which ticket that was is a fact about the *meeting* rather
-- than about any one round of it -- a room may estimate the same ticket over
-- three rounds, going back to it after a discussion -- so it is kept on the
-- room, and every round the table deals is estimated for it.
--
-- What this adds is not a place for a number to land: where a round's answer
-- went is the round's own two columns (see
-- `2026-10-27_14:00:00_add_poker_item_ticket.sql'), and that is a fact about a
-- round that was written down.  These two columns are the earlier and weaker
-- fact -- what the table was opened to estimate -- and they are what a reader
-- is asked for once instead of on every push, and what a round cannot get
-- wrong by having its number mistyped into it.
--
-- The ticket is named the way its own addresses name it, and both columns
-- together name one: a room carrying a project and no number is not about a
-- ticket, and a half-reference is not a reference.
ALTER TABLE "poker_room" ADD COLUMN "ticket_project" varchar(64);

ALTER TABLE "poker_room" ADD COLUMN "ticket_number" INTEGER;
