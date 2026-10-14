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
-- along with mutastructura. If not, see <https://www.gnu.org/licenses/>.

-- The statuses a project's own tickets move through, as the text its edit
-- page was given -- one status to a line, its fields separated by `|'.  A
-- project with nothing here uses the instance's statuses, which is what
-- every existing project does until somebody edits one.
ALTER TABLE "project" ADD COLUMN "ticket_statuses" varchar(4096);
