%% float_truncate( +Num, +P, -Trunc ).
%
% Truncate a float to a maximum of P decimals.
%
% Wrote this to cope with R-3.1.0's inability to read long floats.
%
%==
% ?- L is 0.9344889482650001, assert( l(L) ).
% ?- l(L), float_truncate( L, 10, L10 ).
% L10 = 0.9344889483
%
% ?- l(L), float_truncate( L, 20, L20 ).
% L = L20, L20 = 0.9344889482650001.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/10/28
%
float_truncate( Num, P, Trunc ) :-
 	TenP is 10 ^ P,
	Pow is Num * TenP,
	float_truncate_powered( Pow, Num, TenP, Trunc ).

float_truncate_powered( Pow, Num, _TenP, Trunc ) :-
	Fpow is float_fractional_part( Pow ), 
	0.0 =:= Fpow,     % then we need no truncation, leave the input alone
	!,                % otherwise we might changed it due to floating point
	Trunc is Num.     % arithmetics limitions.
float_truncate_powered( Pow, _Num, TenP, Trunc ) :-
	Trunc is integer(Pow)/TenP.
