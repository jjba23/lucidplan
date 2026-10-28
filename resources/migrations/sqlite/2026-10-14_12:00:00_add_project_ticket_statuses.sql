-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- The statuses a project's own tickets move through, as the text its edit
-- page was given -- one status to a line, its fields separated by `|'.  A
-- project with nothing here uses the instance's statuses, which is what
-- every existing project does until somebody edits one.
ALTER TABLE "project" ADD COLUMN "ticket_statuses" varchar(4096);
