
:- lib(options). %
:- lib(mtx).     %
:- lib(real).    % for infinity code only

%% pl_vector( +VectSpec, -Vect, +Opts ).
%
% True iff VectSpec is a regognisable representation of a vector
% whose canonical representation (a list) is Vect. 
% Through Options you can also control max and min values.
%
% Recognisable represenation are:
% * list
%   which is also the canonical representation
% * Cid
%   when memberchk(mtx(MTx),Opts) and mtx_column( Mtx, Cid, Vect )
%
% Opts 
%  *cnm(Cnm)
%   the column name of the vector (return value)
%  *cnm_def(Def)
%   use Def as Cnm when VectSpec is a prolog list. Leaves free if none is given.
%  *max(Max)
%   curtail values > Max to Max
%  *min(Min) 
%   curtail values < Min to Min
%  *mtx(Mtx)
%   a matrix 
%==
% ?- vector( [1,2,3], V, true ).
% V = [1, 2, 3].
% 
% ?- mtx_data( mtcars, Mc ),
%    pl_vector( 1, Vect, [mtx(Mc),cnm(Cnm)] ),
%    max_list( Vect, Max ).
%
% Mc = [row(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb), row(21.0, 6.0|...],
% Vect = [21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8|...],
% Cnm = mpg,
% Max = 33.9.
% 
% ?- mtx_data( mtcars, Mc ), 
%    pl_vector( 1, Vect, [mtx(Mc),cnm(Cnm),max(30)] ),
%    max_list( Vect, Max ).
% 
% Mc = [row(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb), row(21.0, 6.0|...],
% Vect = [21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8|...],
% Cnm = mpg,
% Max = 30.
%
%==
%
% @see mtx_column/3
% @see pl_vector_curtail/3,4
% @tbd add to real ? but it needs mtx ...
%
pl_vector( VectSpec, Vect, OptS ) :-
	is_list( VectSpec ),
	!,
	en_list( OptS, Opts ),
	( memberchk(cnm_def(Cnm),Opts) -> options_return(cnm(Cnm),Opts); true ),
	pl_vector_curtail( VectSpec, Vect, Opts ).
	% Vect = VectSpec.
pl_vector( Cid, Vect, OptS ) :-
	en_list( OptS, Opts ),
	options( mtx(Mtx), Opts ),
	mtx_column( Mtx, Cid, MtxVect, Cnm, _Cpos ),
	options_return( cnm(Cnm), Opts ),
	pl_vector_curtail( MtxVect, Vect, Opts ).

pl_vector_curtail( VectIn, Vect, Opts ) :-
	( memberchk(max(_),Opts); memberchk(min(_),Opts) ),
	!,
	( memberchk(max(Max),Opts) -> true; 
	                              pl_infinity(Max) ),
	( memberchk(min(Min),Opts) -> true;
	                              pl_infinity(PsvInf),
							Min is - PsvInf
	),
	Min < Max, % else throw error
	pl_vector_curtail( VectIn, Max, Min, Vect ).
pl_vector_curtail( Vect, Vect, _Opts ).

pl_vector_curtail( [], _Max, _Min, [] ).
pl_vector_curtail( [V|Vs], Max, Min, [Val|Tvs] ) :-
	( V > Max -> 
		Val = Max
		;
		( V < Min -> 
			Val = Min
			;
			Val = V
		)
	),
	pl_vector_curtail( Vs, Max, Min, Tvs ).
	

pl_infinity( Inf ) :-
	Inf <- 1 / 0.
