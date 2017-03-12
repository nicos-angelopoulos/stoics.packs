
:- lib( stoics_lib:compound/3 ).

%% term_args( Term, Args ).
%
%  A simple predicate for pinching the args of a term.
%
%==
% maplist( term_args, [row(a,b,c),row(e,d,f)], Lists ).
% Lists = [[a, b, c], [e, d, f]].
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014//
%
term_args( Term, Args ) :-
	compound( Term, _Name, Args ).
