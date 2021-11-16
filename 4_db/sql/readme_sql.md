1. SQLite: 
- no AUTOINCREMENT - it is done automatically on primary key 
- no ALTER TABLE to include foreign key - it must be written in on generation 

2. Run: 
``` 
sqlite3.exe db.sqlite3 -init a4d_wide_sqlite.sql
```
to create a new empty stub 