TODO
- [ ] Add .env file
- [ ] Add Postgres
- [ ] Create function to create user from repl
- [ ] Add user authenitcation
- [ ] Create data model

-   user
    -   id (pk)
    -   email
    -   password
    -   created
    -   modified
-   snippet
    -   id (pk)
    -   user_id
    -   title
    -   content
-   snippet_tag
    -   snippet_id
    -   tag_id
-   tag
    -   id
    -   name

[ ] Save snippets in database
