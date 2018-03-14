
:- lib(prosqlite).
:- lib(db_facts).

/**  exam1.

Testing db_facts package.

==
?- debug(db_facts).
?- exam1.
==

@author nicos angelopoulos
@version  0.1 2018/3/14

*/

exam1 :-
    exam1_del,
    exam1_create,
    exam1_list,
    exam1_connect,
    db_max( exam1, exam1, 1, Max ),
    write( max_at_pos_1(Max) ),
    db_disconnect( exam1 ).

exam1_del :-
    exists_file( 'exam1.sqlite' ),
    !,
    delete_file( 'exam1.sqlite' ).
exam1_del.


exam1_create :-
    sqlite_connect( 'exam1.sqlite', exam1, exists(false) ),
    db_create( exam1, exam1(eid+integer,key+text,val-text) ),
    db_assert( exam1(1,a,ten) ),
    db_assert( exam1(1,b,twenty) ),
    db_assert( exam1(2,a,fifteen) ),
    db_disconnect( exam1 ).

exam1_list :-
    exam1_connect,
    exam1( I, K, V ), 
    write( I:K:V ), nl,
    fail.
exam1_list :-
    db_disconnect( exam1 ).

exam1_connect :-
    sqlite_connect( 'exam1.sqlite', exam1, as_predicates(true) ).
