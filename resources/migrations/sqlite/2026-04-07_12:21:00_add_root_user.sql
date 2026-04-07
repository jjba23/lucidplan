-- INSERT INTO "user" (
--     username, 
--     email, 
--     password, 
--     salt, 
--     permissions
-- ) VALUES (
--     'Joe',
--     'jjbigorra@gmail.com',
--
-- you should compute a
-- SHA-256 hash of (password + salt)
--
--     'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',

--     'my-personal-salt',

-- here we pass the list of permissions
-- as a backquoted Lisp list of symbols

--     '`(lucidplan:modify-permissions lucidplan:ticket-create lucidplan:ticket-delete lucidplan:project-create lucidplan:project-delete lucidplan:user-create lucidplan:user-delete)'
-- );
