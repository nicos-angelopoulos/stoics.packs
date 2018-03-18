:- module( db_facts,
               [  db_create/2,              % +Conn, +Fact 
                  db_assert/1,              % +Fact
                  db_assert/2,              % +Fact, -Aff
                  db_assert/3,              % +Conn, +Fact, -Aff
                  db_holds/1,               % +Conn, -Fact
                  db_holds/2,               % +Conn, -Fact
                  db_retractall/1,          % +Fact
                  db_retractall/2,          % +Fact, -Aff
                  db_retractall/3,          % +Conn, +Fact, -Aff
                  db_query/3,               % +Conn, +SQL, -Res
                  db_table/2,               % +Conn, +Table
                  db_table/3,               % +Conn, +Table, -Facet
                  db_table_column/3,        % +Conn, ?Table, ?Col
                  db_table_column/4,        % +Conn, ?Table, ?Col, ?Facet
                  db_enabled_library/1,     % ?Lib
                  db_current_connection/1,  % ?Conn
                  db_current_connection/2,  % ?Conn, -Type
                  db_disconnect/1,          % +Conn
                  db_date_sql_atom/2,       % ?Date, ?SQLatom
                  db_goal_connection/2,     % +Goal, -Conn
                  db_max/4,                 % +Conn, +Table, +Arg, -Max
                  db_min/4,                 % +Conn, +Table, +Arg, -Min
                  db_version/2
               ] ).

/** <module>  a term based interface to the ODBC and SQLite libaries.

This library serves two purposes. First, term structures can be used to 
interact with SQL databases and second, to provide a common abstraction
layer for ODBC and proSQLite libraries of SWI-Prolog.


This library is debug/1 aware: call =|debug(db_facts)|= to see what is sent to 
the SQL engine.

@version 0.5 2018/3/18, fix single quote in db_holds/3, added db_max/4  and db_min/4 and examples/exam1.pl
@version 0.4 + 0.3, 2016/12/22, fix code-list and enable strings as db fact arguments (and wrap of back-end loading)
@version 0.2, 2016/9/18, allow mass asserts in prosqlite interface
@version 0.1.0, 2013/11/1
@license Perl Artistic License
@author Nicos Angelopoulos, 
@see http://stoics.org.uk/~nicos/sware/db_facts/
@see files in examples/ directory
@see also available as a SWI pack http://www.swi-prolog.org/pack/list
@see doc/Releases.txt

@tbd build data structures so we don't interrogate the dbs about column names and the such all the time

*/

:- use_module( library(debug) ).

/* defaults and settings */

/** db_enabled_library( -Lib ).

     Lib is a db backend library enabled in this run. 
     Lib is in {odbc,sqlite}.
*/

:- dynamic( db_enabled_library/1 ).

db_enable( Lib, _LibName, Enabled ) :-
     catch( use_module(library(Lib)), _, fail ),
     !,
     retractall( db_enabled_library(Enabled) ),
     assert( db_enabled_library(Enabled) ).
db_enable( _Lib, LibName, _Enabled ) :-
     write( user_error, LibName ),
     Mess = ' not available. Db_facts through this back-end disabled.',
     write( user_error, Mess ), 
     nl( user_error ),
     nl( user_error ).
     
:- db_enable( prosqlite, 'proSQLite', sqlite ).
:- db_enable( odbc, odbc, odbc ).

/*  Interface predicates  */

/** db_version( -Version, -Date ).

     The current version. Version is a Mj:Mn:Fx term, and date is a date(Y,M,D) term.

==
?- db_version( 0:5:0, date(2018,3,18) ).
true.
==
*/
% db_version( 0:4:0, date(2016,12,22) ).
db_version( 0:5:0, date(2018,3,18) ).

/** db_create( +Conn, +Goal ).

Very simple interface for creating tables via a term representation (Goal).
Goal should share functor name and arity with the table to be creating at the 
database identified by Conn. Arguments of Goal should be either - or + pairs. 
First term of the pair should be the table name and second should be its type.
The column of a + pair is taken to be part of the primary key.
==
     db_create( phones_db, phones(name+text,telephone-text,address-text) )
==
*/
db_create( Conn, Goal ) :-
     Goal =.. [Name|Args],
     create_pairs_atoms_keys( Args, Patoms, Keys ),
     atomic_list_concat( Patoms, ',', Fields ),
     atomic_list_concat( Keys, ',', Primary ),
     Cre = 'Create Table', 
     PrimK  = 'Primary Key',
     atomic_list_concat( [Cre,Name,'(',Fields,',',PrimK,'(',Primary,') );'], ' ', Create ),
     debug( db_facts, 'Create statement: ~w', Create ),
     sqlite_query( Conn, Create, _Res ).

create_pairs_atoms_keys( [], [], [] ).
create_pairs_atoms_keys( [H|T], [A|As], Keys ) :-
     create_pair_atom( H, A ),
     create_key( H, Keys, TKeys ),
     create_pairs_atoms_keys( T, As, TKeys ).

create_pair_atom( H, A ) :-
    ( H=N+T; H=N-T ),
    !,
    create_type_atom( T, Tatom ),
    atomic_list_concat( [N,Tatom], ' ', A ).

% fixme [] for enum() is untested, in addition it doesnt work in sqlite...
create_type_atom( [H|T], Tatom ) :-
    !,
    atomic_list_concat( [H|T], ',', ValsAtom ),
    atomic_list_concat( ['enum(',ValsAtom,')'], Tatom ).
create_type_atom( Atom, Atom ).

create_key( K+_T, [K|TKeys], TKeys ) :- !.
create_key( _, Keys, Keys ).

/** db_assert( +Goal ).

Call db_assert( Conn, Goal, _Aff ) for the
implied connection Conn for table that corresponds 
to the supplied Goal.

*/

db_assert( Goal ) :-
     db_assert( Goal, _Affected ).

/** db_assert( +Goal, -Affected ).

Call db_assert( Conn, Goal, Affected ) for the
implied connection Conn for table that corresponds 
to the supplied Goal.

*/

db_assert( Goal, Affected ) :-
     db_goal_connection( Goal, Conn ),
     db_assert( Conn, Goal, Affected ).

/** db_assert( +Conn, +Goal, -Affected ).

Assert a table row to table matching Goal of db connection Conn.
Affected is the number of rows affected by the operation.

As of db_facts v0.2 Goal can be a list of Goals with 
all goals asserted in a single Instert operation.

*/

db_assert( Conn, Goals, Affected ) :-   
        % fixme: this is is SQLITE specific for now, maybe we can assume ODBC allow multiple value() 
    is_list( Goals ),
    !,
    %  see http://stackoverflow.com/questions/1609637/is-it-possible-to-insert-multiple-rows-at-a-time-in-an-sqlite-database
    db_query(Conn,'BEGIN TRANSACTION', _ ),
    maplist( db_assert(Conn), Goals, Affs ),
    db_query( Conn, 'COMMIT', _ ),
    maplist( arg(1), Affs, AffNs ),
    sumlist( AffNs, AffectedNum ),
    Affected = row(AffectedNum).

db_assert( Conn, Goal, Affected ) :-
     ground( Goal ),

     Goal =.. [Table|Args],
     db_table_columns( Conn, Table, Clms ),
     db_current_connection( Conn, ConT ),
     Ins = 'Insert into ',

     fact_args_term( Args, Clms, Goal, FATerm ),
     fa_value( known, FATerm, KClms, KVals ), 
     maplist( dquote(ConT), KVals, QVals ),
     atomic_list_concat( QVals, ',', CVals ),

     atomic_list_concat( KClms, ',', CClms ),
     atomic_list_concat( [Ins,Table,' (',CClms,') Values ','(',CVals,')'], Insert ),
    debug( db_facts, 'Assert query: ~w', [Insert] ),
     ( db_query(Conn,Insert,Affected) ->
          true
          ;
          db_error( db_assert_failure(Goal,Insert) )
     ).

/** db_holds( +Goal ).

     Call db_holds( Conn, Goal ) for the 
implied connection Conn for table that corresponds 
to the supplied Goal.

*/

db_holds( Fact ) :-
     db_goal_connection( Fact, Conn ),
     db_holds( Conn, Fact ).

/** db_holds( +Conn, +Goal ).

     Goal is partially instantiated at call, returning 
     at backtracing all matching rows from corresponding table 
     belonging to connection Conn.

*/

db_holds( Conn, Goal ) :-
     Goal =..[Table|Args],
     db_table_columns( Conn, Table, Clms ),
     fact_args_term( Args, Clms, Goal, FATerm ),
     fa_value( known, FATerm, KClms, KVals ), 
     fa_value( unown, FATerm, UClms, UVals ), 
     sql_clm_value_pairs_to_where( KClms, KVals, Where ),
     % next line untested
     ( UClms == [] ->  % then we are asking for confiramtion only
          UnC = '*', UnV = KVals  % can only succeed once in dbs.
          ;
          atomic_list_concat( UClms, ',', UnC ),
          UnV = UVals
     ),
     atomic_list_concat( ['Select ',UnC,'From',Table,Where], ' ', Sql ),
     Row =.. [row|UnV],
     db_query( Conn, Sql, Row ).


/** db_retractall( +Goal ).

Call db_retractall(Conn,Goal,_Aff) for the 
implied connection Conn for table that corresponds 
to the supplied Goal.

*/
db_retractall( Goal ) :-
     db_retractall( Goal, _Affected ).

/** db_retractall( +Goal, -Affected ).

Call db_retractall(Conn,Goal,Affected) for the 
implied connection Conn for table that corresponds 
to the supplied Goal.

*/

db_retractall( Goal, Affected ) :-
     db_goal_connection( Goal, Conn ),
     db_retractall( Conn, Goal, Affected ).

/** db_retractall( +Conn, +Goal, -Affected ).

Remove all rows that correspond to the table from SQLite database
identified by Conn and is named by Goal's name. The arity of Goal
should also match the arity of the table to be deleted.
Ground arguments are added to the Where part of the DELETE SQL
statement at their respective column locations.
Affected is the number of rows affected by the operation.

     ==
          db_retractall( uniprot, secondary_accessions(_,'P64943'), A ).
     ==

*/

db_retractall( Conn, Goal, Affected ) :-
     Goal =.. [Table|Args],
     db_table_columns( Conn, Table, Clms ), 
     fact_args_term( Args, Clms, Goal, FATerm ),
     fa_value( known, FATerm, KClms, KVals ), 
     sql_clm_value_pairs_to_where( KClms, KVals, Where ),
     db_retractall_where( Where, Conn, Table, Affected ).

/** db_goal_connection( +Goal, -Conn ).

     Locate connection serving table matching to Goal. 

*/
db_goal_connection( [Goal|_], Conn ) :-
    !,
    db_goal_connection( Goal, Conn ).
db_goal_connection( Goal, Conn ) :-
     functor( Goal, Pname, _Arity ),
     db_current_connection( Conn ),
     db( current_table(Conn,Pname) ),
     !.
db_goal_connection( _Goal, Conn ) :-
     ground( Conn ), 
     \+ db_current_connection( Conn ),
     !,
     db_error( not_a_known_connection(Conn) ).
db_goal_connection( Goal, Conn ) :-
     ground( Conn ), 
     functor( Goal, Pname ),
     \+ db( current_table(Conn,Pname) ),
     !,
     db_error( not_table_in_this_db(Pname,Conn) ).
db_goal_connection( Goal, Conn ) :-
     db_error( goal_connection_mismatch(Goal,Conn) ).

% following not currently used.
db_goal_connection_arity( Goal, Conn ) :-
     functor( Goal, Pname, Arity ),
     db_current_connection( Conn ),
     db( current_table(Conn, Pname, arity(Arity)) ),
     !.

%% db_table( +Conn, -Table ) is nondet.

%% db_table( +Conn, ?Table, -Facet ) is nondet.
%
%
% Table is a table of database Conn. Facet is a property of table.
%
%
db_table( Conn, Table ) :-
     db_current_connection( Conn, Type ),
    db_type_table( Type, Conn, Table ).
    
db_type_table( sqlite, Conn, Table ) :-
    sqlite_current_table( Conn, Table ).

db_type_table( odbc, Conn, Table ) :-
    odbc_current_table( Conn, Table ).

db_table( Conn, Table, Facet ) :-
     db_current_connection( Conn, Type ),
    db_type_table( Type, Conn, Table, Facet ).

db_type_table( sqlite, Conn, Table, Facet ) :-
    sqlite_current_table( Conn, Table, Facet ).
    
db_type_table( odbc, Conn, Table, Facet ) :-
    odbc_current_table( Conn, Table, Facet ).

%% db_table_column( +Conn, -Table, -Column ).

%% db_table_column( +Conn, -Table, -Column, -Facet ).
% 
%   Table is a table in connection. Column is a column of Table and Facet
%   is an aspect of this Column as supported by the underlying
%   connection manager.
%
db_table_column( Conn, Table, Column ) :-
     db_current_connection( Conn, Type ),
    db_type_table_column( Type, Conn, Table, Column ).
    
db_type_table_column( sqlite, Conn, Table, Column ) :-
    sqlite_table_column( Conn, Table, Column ).

db_type_table_column( odbc, Conn, Table, Column ) :-
    odbc_table_column( Conn, Table, Column ).

db_table_column( Conn, Table, Column, Facet ) :-
     db_current_connection( Conn, Type ),
    db_type_table_column( Type, Conn, Table, Column, Facet ).
    
db_type_table_column( sqlite, Conn, Table, Column, Facet ) :-
    sqlite_table_column( Conn, Table, Column, Facet ).

db_type_table_column( odbc, Conn, Table, Column, Facet ) :-
    odbc_table_column( Conn, Table, Column, Facet ).

/** db_max( +Conn, +Table, +ArgOrClm, -Max ).

Find the max value for a Table, at column ArgOrClm (see db_table_column_name/4).

*/
db_max( Conn, Table, ArgPrv, Max ) :-
    db_table_column_name( Conn, Table, ArgPrv, Cnm ),
    atomic_list_concat( ['SELECT MAX(',Cnm,') FROM ',Table,';'], Sql ),
    db_query( Conn, Sql, Row ),
    Row = row(Max),
    !.

/** db_min( +Conn, +Table, +ArgOrClm, -Min ).

Find the min value for a Table, at column ArgOrClm (see db_table_column_name/4).

*/
db_min( Conn, Table, ArgPrv, Min ) :-
    db_table_column_name( Conn, Table, ArgPrv, Cnm ),
    atomic_list_concat( ['SELECT MIN(',Cnm,') FROM ',Table,';'], Sql ),
    db_query( Conn, Sql, Row ),
    Row = row(Min),
    !.

/** db_table_column_name( +Conn, +Table, +ArgPosOrClm, -Clm )
    
    Get the column name, corresponding to the an integer, indicating position of column in Table's arity,
    or a column name.
*/
db_table_column_name( Conn, Table, ArgPrv, Clm ) :-
    ( integer(ArgPrv) -> 
        findall( Clm, db_table_column(Conn,Table,Clm), Clms ),
        nth1(ArgPrv,Clms,Clm)
        ;
        Clm = ArgPrv
    ).


/**  db_query( +Conn, +Sql, -Row ).

     Get Row at a time from quering database handle Conn,
     with Sql statement.

*/
db_query( Conn, Sql, Row ) :-
     db_current_connection( Conn, Type ),
     debug(db_facts,'To connection ~w, of type ~w, sending: ~a',[Conn,Type,Sql]),
     db_type_query( Type, Conn, Sql, Row ).

/**  db_current_connection( ?Conn ).

     Conn is a currently open db connection.
*/
db_current_connection( Conn ) :-
     db_current_connection( Conn, _Type ).

/**  db_current_connection( ?Conn, -Type ).

     True iff Conn is a  current db connection of (db_facts) Type.

*/
db_current_connection( Conn, Type ) :-
     ground( Conn ), 
     !,
     findall( Conn-T, db_current_connection_gen(Conn,T), CTs ),
     ( CTs = [Conn-Type] ->
          true
          ;
          CTs = [Conn-_T1,Conn-_T2|_],
          db_error( multiple_db_handles(CTs) )
     ).

db_current_connection( Conn, Type ) :-
     db_current_connection_gen( Conn, Type ).

db_current_connection_gen( Conn, Type ) :-
     db_enabled_library( odbc ),
     odbc_current_connection( Conn1, _ ),
     Conn = Conn1,
     Type = odbc.
db_current_connection_gen( Conn, Type ) :-
     db_enabled_library( sqlite ),
     sqlite_current_connection( Conn ),
     Type = sqlite.


/** db_disconnect( +Conn ).

Disconnect from an ODBC or proSQLite connection.

*/

db_disconnect( Conn ) :-
     db( disconnect(Conn) ).

/** db_date_sql_atom( Date, Sql ).

Convert between  a Prolog date/3 term and an Sql atom. 
The conversion is bidirectional.
*/
db_date_sql_atom( date(Y,M,D), Sql ) :-
     ground( Sql ), 
     !,
     atomic_list_concat( Consts, '/', Sql ),
     maplist( atom_number, Consts, [Y,M,D] ).
db_date_sql_atom( date(Y,M,D), Sql ) :-
     atomic_list_concat( [Y,M,D], '/', Sql ).

% non-interface predicates
%
     
% take advantage of the fact that {odbc,sqlite}_predname()
% are valid calls in both backends !
%
db( Goal ) :-
     arg( 1, Goal, Conn ),
     db_current_connection( Conn, Type ),
     Goal =.. [Pname|Args],
     atomic_list_concat( [Type,Pname], '_', Gname ),
     TypeGoal =.. [Gname|Args],
     call( TypeGoal ).

fa_value( kcols, kkv_ukv(KClms,_,_,_), KClms ).
fa_value( kvals, kkv_ukv(_,KVals,_,_), KVals ).
fa_value( ucols, kkv_ukv(_,_,UClms,_), UClms ).
fa_value( uvals, kkv_ukv(_,_,_,UVals), UVals ).

fa_value( known, kkv_ukv(KClms,KVals,_,_), KClms, KVals ).
fa_value( unown, kkv_ukv(_,_,UClms,UVals), UClms, UVals ).

fact_args_term( ArgsIn, Clms, _Goal, FATerm ) :-
     ( (ArgsIn=[Args],is_list(Args)) -> true ; Args = ArgsIn ),
     maplist( look_for_pair_nonvar, Args, Keys, Vals ),
     !,
     look_for_pairs_args_term( Keys, Vals, Clms, Kclms, Kvals, Uclms, Uvals ),
     FATerm = kkv_ukv(Kclms,Kvals,Uclms, Uvals ).
fact_args_term( Args, Clms, Goal, FATerm ) :-
     correspond_args_term( Args, Clms, Goal, Kclms, Kvals, Uclms, Uvals ),
     FATerm = kkv_ukv(Kclms,Kvals,Uclms, Uvals ).
     
correspond_args_term( [], Clms, Goal, [], [], [], [] ) :-
     ( Clms == [] -> 
          true
          ;
          db_error( insufficient_args(Goal) )
     ).
correspond_args_term( [A|As], [C|Cs], Goal, Kclms, Kvals, Uclms, Uvals ) :-
     !,
     ( var(A) ->
          TKclms = Kclms,
          TKvals = Kvals,
          Uclms = [C|TUclms],
          Uvals = [A|TUvals]
          ;
          Kclms = [C|TKclms],
          Kvals = [A|TKvals],
          TUclms = Uclms,
          TUvals = Uvals
     ),
     correspond_args_term( As, Cs, Goal, TKclms, TKvals, TUclms, TUvals ).

correspond_args_term( [A|_As], Clms, Goal, _, _, _, _) :-
     ( Clms == [] -> 
          db_error( too_many_args(Goal) )
          ;
          db_error( could_not_parse_arg(A,Goal) )
     ).

look_for_pairs_args_term( [], [], _Clms,  [], [], [], [] ).
look_for_pairs_args_term( [K|Ks], [V|Vs], Clms, Kclms, Kvals, Uclms, Uvals ) :-
     is_one_of_columns( K, Clms ),
     ( var(V) -> 
          TKclms = Kclms,
          TKvals = Kvals,
          Uclms = [K|TUclms],
          Uvals = [V|TUvals]
          ;
          Kclms = [K|TKclms],
          Kvals = [V|TKvals],
          TUclms = Uclms,
          TUvals = Uvals
     ),
     look_for_pairs_args_term( Ks, Vs, Clms, TKclms, TKvals, TUclms, TUvals ).

look_for_pair_nonvar( Var, _, _ ) :- var(Var), !, fail.
look_for_pair_nonvar( Pair, A, B  ) :-
     look_for_pair_silent( Pair, A, B ).

look_for_pair_silent( A=B, A, B ).
look_for_pair_silent( A-B, A, B ).
look_for_pair_silent( A:B, A, B ).


is_one_of_columns( Clm, Columns ) :-
     memberchk( Clm, Columns ), 
     !.
is_one_of_columns( Clm, Columns ) :-
     db_error( unknown_column(Clm,Columns) ).

% this could be an interface pred....
db_table_columns( Conn, Name, Cols ) :-
     findall( Col, db(table_column(Conn,Name,Col)), Cols ),
     ( Cols == [] -> 
          % we can check if connection is valid here,
          % and send a different error if that's the case.
          db_error( failed_to_get_columns(Conn,Name) )
          ;
          true
     ).

db_connection_goal( Conn, Pred, Args, Goal ) :-
     db_current_connection( Conn, Type ), 
     atomic_list_concat( [Type,Pred], '_', dbPred ),
     Goal =.. [dbPred|Args].

db_type_query( sqlite, Conn, Sql, Row ) :-
     sqlite_query( Conn, Sql, Row ).
db_type_query( odbc, Conn, Sql, Row ) :-
     odbc_query( Conn, Sql, Row ).

db_retractall_where( '', Conn, Name, 0 ) :-
     !,
     write( user_error, 'Refusing to delete whole connection/table':Conn/Name ), 
     nl( user_error ),
     fail.
db_retractall_where( Where, Conn, Name, Affected ) :-
     Del = 'Delete from',
     atomic_list_concat( [Del,Name,Where], ' ', Sql ),
     db_query( Conn, Sql, Row ),
     Row = row(Affected).

sql_clm_value_pairs_to_where(Clms, Vals, Where) :-
     sql_clm_value_pairs_to_where_conjunction(Clms, Vals, Conjunction),
     sql_where_conjunction_to_where(Conjunction, Where).

sql_where_conjunction_to_where('', '' ) :- !.
sql_where_conjunction_to_where(Conjunction, Where ) :-
     atom_concat( 'Where ', Conjunction, Where ).

sql_clm_value_pairs_to_where_conjunction([], [],  '').
sql_clm_value_pairs_to_where_conjunction([K|Ks], [V|Vs], Where) :-
     sql_clm_value_pairs_to_where_conjunction( Ks, Vs, InWhere ),
     sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm),
     ( InWhere == '' -> 
          Where = KVAtm
          ;
          atomic_list_concat([KVAtm, ' AND ', InWhere], Where)
     ).

sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm) :-
     ( number(V) -> 
          atom_number(Vatm, V),
          atom_concat('=',Vatm,EqV)
          ;
          atomic_list_concat( Parts, '\'', V ),
          atomic_list_concat( Parts, '\'\'', Vdb ),
          atom_concat(Vdb, '\'', VDsh),
          atom_concat('=\'',VDsh,EqV)
     ),
     atom_concat(K, EqV, KVAtm).

% fixme: date ?
dquote( _, date(Y,M,D), Quoted ) :-
     !,
     atomic_list_concat( ['"',Y,'/',M,'/',D,'"'], Quoted ).
dquote( _, Val, Quoted ) :-
     number( Val ), 
     !,
     Quoted = Val.
dquote( ConT, Val, Quoted ) :-
     atom( Val ),
     !,
     ( ConT == sqlite -> atom_replace( Val, '"', '""', Esc );
                         Esc = Val ),
     atomic_list_concat( ['"',Esc,'"'], Quoted ).
dquote( _ConT, Val, Quoted ) :-
     is_list( Val ),
    !,
     append( [0'"|Val], [0'"], QuotedCs ),
     atom_codes( Quoted, QuotedCs ).
dquote( ConT, Val, Quoted ) :-
    string( Val ),
    atom_string( Atm, Val ),
     ( ConT == sqlite -> atom_replace( Atm, '"', '""', Esc );
                         Esc = Val ),
     atomic_list_concat( ['"',Esc,'"'], Quoted ).

/*
dquote( Val, Quoted ) :-
     number( Val ), 
     !,
     Quoted = Val.
dquote( Val, Quoted ) :-
     atom_replace( Val, '"', '""', Esc ),
     atom_codes( Esc, Codes ),
     CPairs = [0'"-[0'",0'"],946-"beta",947-"gamma"],
     dquote_cs( Codes, CPairs, Qcs ),
     atom_codes( Quoted, [0'"|Qcs] ).
     % atomic_list_concat( ['"',Esc,'"'], Quoted ).

dquote_cs( [], _CBys, [0'"] ).
dquote_cs( [H|T], CBys, Clean ) :-
     ( memberchk(H-By,CBys) -> 
          sew( By, Clean, Tail )
          ;
          Clean = [H|Tail]
     ),
     dquote_cs( T, CBys, Tail ).

sew( [], Tail, Tail ).
sew( [H|T], [H|M], Tail ) :-
     sew( T, M, Tail ).
*/

%% atom_replace( +Atom, +What, +With, -New ).
%
%  Replace all occurances of What in Atom with With to produce New.
%
atom_replace( Atom, What, With, New ) :-
     atom_break_at( Atom, What, Pfx, Psf ),
     !,
     atom_replace( Psf, What, With, NewPsf ),
     atomic_list_concat( [Pfx,NewPsf], With, New ).
atom_replace( New, _What, _With, New ).

%% atom_break_at( +Atom, +Breaks, -Pfx, -Psf ).
%
%  Break an atom at Occurances of Breaks sub atom. 
%  Pfx = Prefix before Break, and Psf = Postfix, after the break.
% 
atom_break_at( Atom, Breaks, Pfx, Psf ) :-
     sub_atom( Atom, Bef, _Len, Aft, Breaks ), 
     sub_atom( Atom,   0, Bef,   _, Pfx ),
     Start is Bef + 1,
     sub_atom( Atom, Start, Aft, _, Psf ).

%-Section error handling.
db_error( Term ) :-
     Type = error,
     print_message( Type, db(Term) ),
     abort.

:- multifile prolog:message//1.

prolog:message(db(Message)) -->
    message(Message).

message( failed_to_get_columns(Conn,Tname) ) -->
     ['Failed to get columns for table ~q on connection ~q.' - [Tname,Conn] ].
message( db_assert_failure(Goal,Insert) ) -->
     ['Failed to assert fact ~q by sql command ~a.' - [Goal,Insert] ].
message( multiple_db_handles([Conn-_|_]) ) -->
     ['Multiple entries for single connection alias ~q.' - [Conn] ].
% not currently used : 
message( operation_needs_all_columns(Op,Cols,Goal) ) -->
     ['Operation ~a needs all columns, instead of ~q, in fact ~q'
               - [Op,Cols,Goal] ].
message( insufficient_args(Goal) ) -->
     ['Insufficient number of arguments in db_fact, ~q.' - [Goal] ].
message( too_many_args(Goal) ) -->
     ['Too many arguments in db_fact, ~q.' - [Goal] ].
message( could_not_parse_arg(A,Goal) ) -->
     ['Could not parse argument ~q in db_fact, ~q.' - [A,Goal] ].
message( unknown_column(Clm,Columns) ) -->
     [ 'Unkown column, ~q expected one in ~q.' - [Clm,Columns] ].
message( not_a_known_connection(Conn) ) -->
     ['Not a known connection:~q.' - Conn ].
message( not_table_in_this_db(Pname,Conn) ) -->
     ['Cannot locate table ~q in database connected at ~q.'- [Pname,Conn] ].
message( goal_connection_mismatch(Goal,_Conn) ) -->
     [ 'Cannot establish parent db for fact, ~q.' - [Goal] ].
