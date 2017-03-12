%% max_n_fractional( +N, +Num, -NFract ).
%
% NFract curtails Num, in having at the maximum N digits.
%
%==
% ?- max_n_fractional( 2, 2.213, Round ).
% Round = 2.21.
% ?- max_n_fractional( 2, 2.218, Round ).
% Round = 2.22.
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/6/30
%
max_n_fractional( N, Num, Round ) :-
	Decis is 10 ^ N,
	Round is integer( Num * Decis ) / Decis.

