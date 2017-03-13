
:- lib(options). %
:- lib(suggests(mtx)).     %

:- lib( stoics_lib:holds/2 ).
:- lib( stoics_lib:kv_compose/3 ).

%% pl_vector( +VectSpec, -Vect, +Opts ).
%
% True iff VectSpec is a regognisable representation of a vector
% whose canonical representation (a list) is Vect. 
% Through Options you can also control max and min values.
%
% Recognisable represenation are:
%  * list
%     which is also the canonical representation
%  * Cid
%     when memberchk(mtx(MTx),Opts) and mtx_column( Mtx, Cid, Vect )
%
% Opts 
%  * cnm(Cnm)
%     the column name of the vector (return value)
%  * cnm_def(Def)
%     use Def as Cnm when VectSpec is a prolog list. Leaves free if none is given.
%  * k(Kid)
%     return a paired vector where K is taken from Kid column of Mtx (below)- 
%     and V from VectSpec
%  * max(Max)
%     curtail values > Max to Max
%  * min(Min) 
%     curtail values < Min to Min
%  * mtx(Mtx)
%     a matrix 
%  * v(Vid)
%     return a paired vector where V is taken from Vid column of Mtx (below)- 
%     and K from VectSpec. Only used if k(Kid) is not present
%  * where(Cid(Val))
%     restrict matrix to those rows that have in Cid value Val
%
%  Currently, k() and v() are inompatible to max() and min().
%
%==
% ?- pl_vector( [1,2,3], V, true ).
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
% @see pl_vector_curtail/3,4
% @tbd add to real ? but it needs mtx ...
% @author  nicos angelopoulos
% @version 0.2 2016/6/7,  added where() and k(),v() pairs
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
	options( mtx(FullMtx), Opts ),
	pl_vector_where( FullMtx, Mtx, Opts ),
	mtx_column( Mtx, Cid, MtxVect, Cnm, _Cpos ),
	options_return( cnm(Cnm), Opts ),
	pl_vector_pair( AsPair, IsK, Mtx, PairVect, Opts ),
	pl_vector_curtail( MtxVect, AsPair, IsK, PairVect, Vect, Opts ).

pl_vector_where( FullMtx, Mtx, Opts ) :-
	holds( memberchk(where(Where),Opts), HasWhere ),
	pl_vector_has_where( HasWhere, FullMtx, Where, Mtx ).

pl_vector_has_where( true, Full, Where, Mtx ) :-
	Where =.. [Cid,Val],
	mtx_subset( Full, Cid, ==(Val), Mtx ).
pl_vector_has_where( false, Mtx, _Where, Mtx ).

pl_vector_pair( AsPair, IsK, Mtx, PairVect, Opts ) :-
	( memberchk(k(Kid),Opts) ->
		AsPair = true,
		IsK    = false,
		mtx_column( Mtx, Kid, PairVect )
		;
		( memberchk(v(Vid),Opts) ->
			AsPair = true,
			IsK    - true,
			mtx_column( Mtx, Vid, PairVect )
			;
			AsPair = false,
			IsK  = false,
			PairVect = []
		)
	).

pl_vector_curtail( VectIn, _AsPair, _IsK, _PairTo, Vect, Opts ) :-
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
pl_vector_curtail( Vect, AsPair, IsK, PairTo, Pairs, _Opts ) :-
	pl_vector_curtail_pass( AsPair, IsK, Vect, PairTo, Pairs ).

pl_vector_curtail_pass( false, _IsK, Vect, _PairTo, Vect ).
pl_vector_curtail_pass( true, IsK, Vect, PairTo, Pairs ) :-
	pl_vector_curtail_pass_combine( IsK, Vect, PairTo, Pairs ).

pl_vector_curtail_pass_combine( true, Vect, PairTo, Pairs ) :-
	kv_compose( Vect, PairTo, Pairs ).
pl_vector_curtail_pass_combine( false, Vect, PairTo, Pairs ) :-
	kv_compose( PairTo, Vect, Pairs ).

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
	Inf is inf.
/*
pl_infinity( Inf ) :-
	Inf <- 1 / 0.
	*/
