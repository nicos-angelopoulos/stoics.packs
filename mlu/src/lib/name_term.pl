
:- lib( stoics_lib:compound/3 ).

%% name_term( ?Name, +Term ).
%
% True iff Name is the name of the Term.
% Suitable for meta-calls on list of terms. 
%
%== 
% partition( name_term(dev), [dev(x),abc(d)], Yes, No ).
% Yes = [dev(x)],
% No = [abc(d)].
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/03/11
% @see term_name/2. It is probably better and argument order is correct.
%
name_term( Name, Term ) :-
	compound( Term, Name, _Arity ),
	!.
name_term( Name, Term ) :-
	atomic( Term ),
	Name = Term.
