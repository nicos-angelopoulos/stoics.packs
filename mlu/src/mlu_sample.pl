
:- lib( kvs_k_increases_v/3 ).

mlu_sample_defaults( repeats(false) ).

/** mlu_sample( +Goal, +Times, -Yield, -Pairs ).
    mlu_sample( +Goal, +Times, -Yield, -Pairs, +Opts ).

Run Goal Times number of times, at each run observing Yield.
The results are the Yield-Count paired list Pairs.

Currently the predicate: copy_terms Goal and Yield, and requires that 
Yield's copy will be ground after Goal's copy is called.

Opts
  * repeats(R=false)
     if integer(R), the V in Pairs is a list of results (length(V) = R)

==
?- lib(pepl).
?- sload_pe( coin ).
?- mlu_sample( scall(coin(Side)), 100, Side, Freqs ).
Freqs = [head-47, tail-53].

?- mlu_sample( scall(coin(Side)), 100, Side, Freqs ).
Freqs = [head-49, tail-51].

==
@author nicos angelopoulos
@version  0.1 2016/8/31
*/
mlu_sample( GoalPrv, N, Yield, Pairs ) :-
	mlu_sample( GoalPrv, N, Yield, Pairs, [] ).

mlu_sample( GoalPrv, N, Yield, Pairs, Args ) :-
	options_append( mlu_sample, Args, Opts ),
	N > 0,
	( GoalPrv = _:_ -> Goal = GoalPrv; Goal = user:GoalPrv ),
	options( repeats(R), Opts ),
	mlu_sample_repeats( R, N, Goal, Yield, Pairs ).

mlu_sample_repeats( false, N, Goal, Yield, Pairs ) :-
	!,
	mlu_sample_n( N, Goal, Yield, [], Pairs ).
mlu_sample_repeats( R, N, Goal, Yield, Pairs ) :-
	integer( R ),  % fixme add error messages
	R > 0,
	mlu_sample_r_repeats( R, N, Goal, Yield, [], 0, Pairs ).

mlu_sample_r_repeats( 0, _N, _Goal, _Yield, KVs, _NofSeen, Pairs ) :-
	!,
	Pairs = KVs.
mlu_sample_r_repeats( R, N, Goal, Yield, AccKVs, NofSeen, Pairs ) :-
	mlu_sample_n( N, Goal, Yield, [], DisPairs ),
	keysort( DisPairs, Ord ),
	mlu_sample_repeat_fold_in( Ord, AccKVs, NofSeen, NxtKVs ),
	P is R - 1,
	NxtOfSeen is NofSeen + 1,
	mlu_sample_r_repeats( P, N, Goal, Yield, NxtKVs, NxtOfSeen, Pairs ).

mlu_sample_repeat_fold_in( [], KVs, _NofSeen, KVs ) :- !.
mlu_sample_repeat_fold_in( NewKVs, [], _NofSeen, NewKVSs ) :- !,
	findall( K-[V], member(K-V,NewKVs), NewKVSs ).
mlu_sample_repeat_fold_in( [K1-V1|T1], [K2-V2|T2], NofSeen, KVs ) :-
	compare( Op, K1, K2 ),
	mlu_sample_repeat_fold_in_op( Op, K1, V1, T1, K2, V2, T2, NofSeen, KVs ).

mlu_sample_repeat_fold_in_op( =, K1, V1, T1, _K2, V2, T2, NofSeen, [K1-Vs|T] ) :-
	append( V2, [V1], Vs ),
	mlu_sample_repeat_fold_in( T1, T2, NofSeen, T ).
mlu_sample_repeat_fold_in_op( <, K1, V1, T1, K2, V2, T2, NofSeen, [K1-Vs|T] ) :- % unseen value
	findall( 0, between(1,NofSeen,_), Zeros ),
	append( Zeros, [V1], Vs ),
	mlu_sample_repeat_fold_in( T1, [K2-V2|T2], NofSeen, T ).
mlu_sample_repeat_fold_in_op( <, K1, V1, T1, K2, V2, T2, NofSeen, [K2-Vs|T] ) :- % missing value
	append( V2, [0], Vs ),
	mlu_sample_repeat_fold_in( [K1-V1|T1], T2, NofSeen, T ).

mlu_sample_n( 0, _Goal, _Yield, Pairs, Pairs ) :- !.
mlu_sample_n( N, Goal, Yield, Acc, Pairs ) :-
	N > 0,
	debug( mlu(sample), 'Doing N: ~d', N ),
	copy_term( Goal-Yield, G1-Y1 ),
	call( G1 ),
	ground( Y1 ),
	!,
	kvs_k_increases_v( Acc, Y1, Next ),
	M is N - 1,
	mlu_sample_n( M, Goal, Yield, Next, Pairs ).
mlu_sample_n( N, Goal, Yield, Acc, Pairs ) :-  % fixme make behaviour here dependent to an option? 
	N > 0,
	kvs_k_increases_v( Acc, fail, Next ),
	M is N - 1,
	mlu_sample_n( M, Goal, Yield, Next, Pairs ).
