-- lucidplan - free and open project management for everyone
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>

-- The picture a project wears in a list of them: the *stored* name of a file
-- this instance wrote itself -- never the name an upload arrived under -- and
-- nothing at all for a project that has never been given one.  A project with
-- nothing here is drawn with the letter avatar the pages generate from its
-- name, so a new project looks deliberate from the moment it exists rather
-- than looking unfinished until somebody uploads something.
ALTER TABLE "project" ADD COLUMN "avatar" varchar(255);
