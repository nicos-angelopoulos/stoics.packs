
:- use_module( library(prosqlite) ).
:- use_module( library(db_facts) ).

/** simple.

==
?- simple.


Using database at: simple.sqlite
create_res(row(0))
i1(Insert into cited_by (pubmed_id,ret_date,citer) values (123, "2012/10/6", 321);)
insert1_res(row(1))
i2(Insert into cited_by (pubmed_id,ret_date,citer) values (120, "2012/10/6", 321);)
insert2_res(row(1))
sel(Select * from cited_by)
sel:row(123,2012/10/6,321)
sel:row(120,2012/10/6,321)
d(Delete From cited_by Where pubmed_id = 120;)
del_row(row(1))
sel:row(123,2012/10/6,321)
true.


?- 
==
*/

simple :-
     catch( delete_file('simple.sqlite'), _, true ),
     sqlite_connect( simple, simple, exists(false) ),
     db_create( simple, cited_by(pubmed_id+bigint, ret_date-date, citer+bigint) ),
     Date = date(2012,10,06),
     write( 'Asserting 2 db fatcs via db_assert/1.' ),  nl,
     db_assert( cited_by(123, Date, 321) ),
     db_assert( cited_by(120, Date, 321) ),
     write( 'Findall cited_by, rows and columns .' ), nl,
     findall( _,  ( db_holds( cited_by(pubmed_id=Pid,ret_date=D1,citer=Cit) ), 
                    write( Pid:D1:Cit ), nl ), _ ),
     write( 'Deleting 1 record via db_retractall>' ), nl,
     db_retractall( cited_by(pubmed_id=120) ),
     write( 'Turning debug messaages on.' ), nl,
     debug( db_facts ),
     write( 'Findall cited_by, rows and columns .' ), nl,
     findall( _,  ( db_holds( cited_by(pubmed_id=Pid,ret_date=D1,citer=Cit) ), 
                    db_date_sql_atom( Da1, D1 ),
                    write( Pid:Da1:Cit ), nl ), _ ),
     sqlite_disconnect( simple ).

