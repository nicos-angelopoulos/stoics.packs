
:- lib( stoics_lib:arity/3 ).

goal_predicate_name( Var, _ ) :-
	var(Var),
	!,
	fail. % throw().
goal_predicate_name( _Mod:Goal, Pname ) :-
	!,
	arity( Goal, Pname, _ ).
goal_predicate_name( Goal, Pname ) :-
	arity( Goal, Pname, _ ).
