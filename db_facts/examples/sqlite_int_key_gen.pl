
:- use_module(library(debug)).
:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).

:- debug(sqlite_int_key_gen).

/** sqlite_int_key_gen.

Test the issue reported by W. Coene in Feb 2023.

Examples
==
?- working_directory(_,'/tmp').
?- [pack(db_facts/examples/sqlite_int_key_gen)].
?- sqlite_int_key_gen.

==

@author nicos angelopoulos
@version  0.1 2023/02/04

*/

sqlite_int_key_gen :-
     Self = sqlite_int_key_gen,
     sqlite_connect( foo_db, Conn, exists(false) ),
     db_create(Conn, foo(id+integer, value-text)),
     findall( Id, db_assert(Conn, foo(Id, "foo"), _), Ids ),
     debug( Self, 'Got ids: ~w', [Ids] ),
     sqlite_disconnect( Conn ).
