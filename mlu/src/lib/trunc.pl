:- lib( float_truncate/3 ).

%% trunc( +P, +Num, -Trunc ).
%
%  Arg rehash of float_truncate/3, with a shorter name too.
trunc( P, Num, Trunc ) :-
	float_truncate( Num, P, Trunc ).
