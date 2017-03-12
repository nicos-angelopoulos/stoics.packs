
:- requires( compound/3 ).

%% nth_arg( +N, +Term, -Nth, -Rem ).
%
%  Nth is Term's argument at position N and Rem is the 
%  remainder of the term. pure_compound/1 constructs are used.
%
% Examples run on Swi 7
%==
% nth_arg( 3, vmf(f1,loc,path,0:0:1), Third, Rem ).
% Third = path,
% Rem = vmf(f1, loc, 0:0:1).
%
% maplist( nth_arg(1), [t(x,y,z),t(1,2,3)], Firsts, Rests ).
% Firsts = [x, 1],
% Rests = [t(y, z), t(2, 3)].
% 
%==
% 
% @author nicos angelopoulos
% @version  0.1 2014/5/19
% @see arg/5 for replacing the nth argument of a term
% @tbd generalise to terms (see some other preds that call this, 
%  and move it to ../term/
%
nth_arg( N, Term, Nth, Rem ) :-
	compound( Term, Name, Args ),
	nth1( N, Args, Nth, RemArgs ),
	compound( Rem, Name, RemArgs ).
