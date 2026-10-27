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
