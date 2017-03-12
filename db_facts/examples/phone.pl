
:- ensure_loaded( library(prosqlite) ).
:- ensure_loaded( library(db_facts) ).
:- ensure_loaded( library(debug) ).

phone :-
     Sfile = 'phones.sqlite',
     does_not_exist( Sfile ),
     create_db, 
     interrogate,
     write( 'Deleting database file.' ), nl,
     delete_file( Sfile ),
     nodebug( db_facts ),
     write( 'All done.' ), nl.

create_db :-
     sqlite_connect( phones, phones_db, exists(false) ),
     db_create( phones_db, phones(name+text,telephone-text,address-text) ),
     close_db.

interrogate :-
     write( 'Testing assert + holds.' ), nl,
     sqlite_connect( phones, phones_db ),
     write( 'Inserting 2 records.' ), nl,
     db_assert( phones(naku,"0044",uk) ),
     db_assert( phones(ozzie,`0090`,turkey) ),
     report_phones_db,
     findall( U-T, db_holds(phones(U,T,_)), UTs ), write( uts(UTs) ), nl,
     findall( T, db_holds(phones(naku,T,_)), Ts ), write( naku(Ts) ), nl,
     close_db,

     sqlite_connect( phones, phones_db ),
     write( 'Testing list of pair terms.' ), nl,
     sqlite_connect( phones, phones_db ),
     findall( U-T, db_holds(phones([name=U,telephone=T])), UT2s ),
     write( uts_list(UT2s) ), nl,
     findall( T, db_holds(phones([name=naku,telephone=T])), T2s ),
     write( naku_list(T2s) ), nl,
     close_db, 

     sqlite_connect( phones, phones_db ),
     write( 'Adding 3 more records.' ), nl,
     db_assert( phones('van.de.engel','0030',hellas) ),
     db_assert( phones(panos,'0034',spain) ),
     db_assert( phones("petros",'0082',south_korea) ),
     report_phones_db,

     Mess5='Testing retract_all and debugging information.',
     debug( db_facts ),
     write( Mess5 ), nl,
     write( 'Deleting via phones(panos,_,_).' ), nl,
     db_retractall( phones_db, phones(panos,_,_), Aff1 ),
     write( aff:Aff1 ), nl,
     report_phones_db,
     write( 'Deleting via phones(name=petros).' ), nl,
     db_retractall( phones_db, phones(name=petros), Aff2 ),
     write( aff:Aff2 ), nl,
     report_phones_db,
     sqlite_disconnect( phones_db ),
     write( 'Closed database.' ), nl, nl.
% add test for moudule usage

report_phones_db :-
     write( 'Select * from phones : ' ), nl,
     write( '---' ), nl,
     findall( Row, (sqlite_query(phones_db,'Select * from phones;',Row),write(Row),nl), _ ),
     write( '---' ), nl.

create( C ) :-
     C = 'CREATE TABLE phones (name text, telephone text, address text, Primary Key (name) );'.
     
insert( I ) :-
     I = 'Insert into phones (name, telephone, address) values ("naku","0044","uk");'.
insert( I ) :-
     I = 'Insert into phones (name, telephone, address) values ("ozzy","0090","turkey");'.

close_db :-
     db_disconnect( phones_db ),
     write( 'Closed database, for now.' ), nl, nl.

does_not_exist( Sfile ) :-
     exists_file( Sfile ),
     !,
     write( 'Delete file "' ), 
     write( Sfile ), 
     write( '" to proceed with this example.' ), nl,
     fail.
does_not_exist( _Sfile ).

del :- delete_file('phones.sqlite').
