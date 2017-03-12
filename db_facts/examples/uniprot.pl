
:- use_module( library(db_facts) ).
:- use_module( library(prosqlite) ).


:- nl, nl, nl.
:- write( 'To get the database used in this examples do:' ), nl.
:- write( 'wget http://stoics.org.uk/~nicos/sware/prosqlite/uniprot.sqlite' ), nl.
:- write( 'However, be warned its size is 189 Mb.' ), nl.
:- nl, nl.

/* 
     swipl -l db_facts/examples/uniprot.pl
*/

uniprot :-
     uniprot_arity, 
     write( '    -----   ' ), nl,
     uniprot_pairs.

uniprot_arity :-
     nl, write(' Trying examples with full arity calls.' ), nl, nl,
     UniF = 'uniprot.sqlite',
     sqlite_connect( UniF, uniprot ),
     show_tables( uniprot ),
     show_columns( uniprot ),
     db_current_connection( uniprot, Type ),
     write( by_type(Type) ), nl,
     db_goal_connection( secondary_accessions(_,_), Conn ),
     write( served_by(Conn) ), nl, nl,
     Id = 'P64943',
     findall( S, db_holds(secondary_accessions(S,Id)), SetBef ),
     write( by_findall(Id,SetBef) ), nl,
     write( 'Caution, deleting db entries for'-Id ), nl,
     db_retractall( secondary_accessions(_,Id), Aff ), 
     write( 'Affected rows':Aff ), nl,
     findall( S, db_holds(secondary_accessions(S,Id)), SetAft ),
     write( now(Id,SetAft) ), nl, nl,
     write( 'Not to worry! Adding back the db entries for'-Id ), nl,
     Add1 = 'A0A111',
     db_assert( secondary_accessions(Add1,Id) ), 
     Add2 = 'Q10706',
     db_assert( secondary_accessions(Add2,Id) ), 
     findall( S, db_holds(secondary_accessions(S,Id)), SetFin ),
     write( finally(Id,SetFin) ), nl,
     findall( R, db_query(uniprot,'Select * from secondary_accessions Where primary_accession="P64943";',R), Rows ),
     write( select_rows(Rows) ), nl,
     db_disconnect( uniprot ),
     write( db_closed(uniprot) ), nl, nl.

uniprot_pairs :-
     write(' Trying examples with full pairs notation.' ), nl, nl,
     UniF = 'uniprot.sqlite',
     sqlite_connect( UniF, uniprot ),
     Id = 'P64943',
     findall( S, db_holds(secondary_accessions(secondary_accession=S,primary_accession=Id)), SetBef ),
     write( by_findall(Id,SetBef) ), nl,
     write( 'Caution, deleting db entries for'-Id ), nl,
     db_retractall( secondary_accessions(primary_accession=Id), Aff ), 
     write( 'Affected rows':Aff ), nl,
     findall( S, db_holds(secondary_accessions(secondary_accession=S,primary_accession=Id)), SetAft ),
     write( now(Id,SetAft) ), nl, nl,
     write( 'Not to worry! Adding back the db entries for'-Id ), nl,
     Add1 = 'A0A111',
     db_assert( secondary_accessions(primary_accession=Id,secondary_accession=Add1) ), 
     Add2 = 'Q10706',
     db_assert( secondary_accessions(secondary_accession=Add2,primary_accession=Id) ), 
     findall( S, db_holds(secondary_accessions(primary_accession=Id,secondary_accession=S)), SetFin ),
     write( finally(Id,SetFin) ), nl,
     db_disconnect( uniprot ),
     write( db_closed(uniprot) ), nl, nl.


show_columns( Alias ) :-
     sqlite_table_column( Alias, Table, Col ),
     write( Table/Col ), nl, 
     fail.
show_columns( _Alias ) :-
     nl.

show_tables( Alias ) :-
     sqlite_current_table( Alias, Table ),
     write( t(Table) ), nl,
     fail.
show_tables( _Alias ).
