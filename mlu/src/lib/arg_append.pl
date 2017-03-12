
:- lib( stoics_lib:compound/3 ).

/** arg_append( +Term, +App, -NewTerm ).

	Append new arguments to a term or atom. App can be a term, an atomic or a list.

==
arg_append( x(a,b), w(y,z), New ).
New = x(a, b, y, z).

arg_append( x(a,b), [y,z], New ).
New = x(a, b, y, z).

arg_append( x(a,b), y, New ).
New = x(a, b, y).

==

 @author nicos angelopoulos
 @version  0.2 2014/03/28, brought test for list first, as it is also a compound!
 @version  0.3 2015/01/12, allow Term to be an atom

*/
arg_append( Term, AppList, New ) :-
	is_list( AppList ),
	!,
	( compound(Term,Tname,TArgs) ->
		true
		;
		atom(Term), % fixme: atomic? 
		Tname = Term,
		TArgs = []
	),
	% Term =.. [Tname|TArgs],
	append( TArgs, AppList, NArgs ),
	compound( New, Tname, NArgs ).
	% New =.. [Tname|NArgs].
arg_append( Term, AppTerm, New ) :-
	compound( AppTerm ), 
	!,
	% AppTerm =.. [_ATname|ATArgs],
	% Term =.. [Tname|TArgs],
	compound( AppTerm, _ATname, ATArgs ),
	compound( Term, Tname, TArgs ),
	append( TArgs, ATArgs, NArgs ),
	% New =.. [Tname|NArgs].
	compound( New, Tname, NArgs ).
arg_append( Term, AppAtomic, New ) :-
	atomic( AppAtomic ),
	% Term =.. [Tname|TArgs],
	compound( Term, Tname, TArgs ),
	append( TArgs, [AppAtomic], NArgs ),
	% New =.. [Tname|NArgs].
	compound( New, Tname, NArgs ).
