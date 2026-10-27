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

-- What a table agreed on is not the same fact as what a ticket is estimated
-- at: the first is what a room said, the second is what the backlog now
-- carries, and either may happen without the other.  So recording that a round
-- was written onto a ticket is a fact about the *round*, kept beside it --
-- which round it was, and which ticket took it -- and not a flag on the ticket
-- and not a copy of the number the ticket now holds (that number is the
-- ticket's own column, and the trail of how it got there is the ticket's own
-- activity).
--
-- The ticket is named the way its own addresses name it: the project it is in
-- and its number in that project, which is what a link needs and what does not
-- change under it.  A ticket's number counts within a project, and moving a
-- ticket to another project gives it the destination's next number, so the
-- pair is a reference and not a copy of anything -- the ticket's id is not
-- kept here because nothing in this section may join to an account or a
-- backlog row: a table stays readable when the instance it estimates for has
-- never heard of the ticket, or has since deleted it.
ALTER TABLE "poker_item" ADD COLUMN "ticket_project" varchar(64);

ALTER TABLE "poker_item" ADD COLUMN "ticket_number" INTEGER;
