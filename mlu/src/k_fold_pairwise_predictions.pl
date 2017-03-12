:- lib( term_name/2 ).

k_fold_pairwise_predictions_defaults( Args, Defs ) :-
	memberchk( learners(Lrns), Args ),
	( memberchk(pairs(_),Args) -> TDefs = []; 
		TDefs = [pairs(_Pairs)]
	),
	( memberchk(learner_names(_Lames),Args) ->
		TDefs = Defs
		;
		Defs = [learner_names(Lrns)|TDefs]
	).

/** k_fold_pairwise_predictions( +Dat, +Learners, +Predictor, -Models, -Stats, +Opts ).

Run k_fold_pair_predictions/7 on pairs of Learners on a single k_fold segmentation. 
By default all pairwise comparisons are considered.

Opts
  * pairs(Pairs)
	  list or single pair (L1-L2) of predictions to consider

@see options for k_fold_pair_predictions/7
@tbd allow distinct Predictors

*/
k_fold_pairwise_predictions( Dat, Lrs, PrG, Mls, Sts, Args ) :-
	Self = k_fold_pairwise_predictions,
	options_append( k_fold_pairwise_predictions, Args, Opts, extra_arg(learners(Lrs)) ),
	% here( Dat, Lrs, Prs, Mls, Sts ).
	k_fold_learn_list( Lrs, 1, Self, Dat, PrG, _Segs, Mls, Sts, Opts ),
	debug( k_fold_pairwise_predictions, 'done learning', [true] ),
	% k_fold_pairs_predictions( Pairs, Dat, Lrs, Mls, 
	options( learner_names(Lames), Opts ),
	options( pairs(Pairs), Opts ),
	k_fold_pairwise_posts_collect( Opts, Posts, OthOpts ),
	% findall(...member(Post,Posts),term_name(Post,PostName) ... )
	findall( Post-PostOpts, ( member(Post,Posts),term_name(Post,PostName),
                              % Post=..[PostName|_],
                              PostTerm=..[PostName,PostOpt],
	                          findall(PostOpt,member(PostTerm,OthOpts),PostOpts) ), PostPairs ),
	options( statistic_names(StaNames), Opts ),
	maplist( k_fold_postfix_multi_learner, Lrs, MrsNest ),
	flatten( MrsNest, Mrs ),
	k_fold_pairwise_learners_predictions( Mrs, Pairs, Lames, StaNames, Mls, Sts, PostPairs ).

k_fold_postfix_multi_learner( multi(GPsfxs,Goal), Posted ) :-
	!,
	findall( Postfixed, ( member(Psfx,GPsfxs), goal_predicate_name(Goal,Stem),
					  atomic_list_concat( [Stem,Psfx], '_', Postfixed )
	                    ), Posted ).
k_fold_postfix_multi_learner( Posted, Posted ).

k_fold_pairwise_learners_predictions( [], _Pairs, [], _StaNames, [], [], _PostPairs ).
k_fold_pairwise_learners_predictions( [Lr|Lrs], Pairs, [Lm|Lms], StaNames, [Ml|Mls], [St|Sts], PostPairs ) :-
	k_fold_pairwise_predictions_nest( Lrs, Lms, StaNames, Pairs, Lr, Lm, Mls, Ml, Sts, St, PostPairs ),
	k_fold_pairwise_learners_predictions( Lrs, Pairs, Lms, StaNames, Mls, Sts, PostPairs ).

k_fold_pairwise_predictions_nest( [], [], _StaNames, _Pairs, _Lr1, _Lm, [], _Ml1, [], _St1, _PostPairs ).
k_fold_pairwise_predictions_nest( [Lr2|Lrs], [Lm2|Lms], StaNames, Pairs, Lr1, Lm1, [Mls2|TMls], Mls1, [Sts2|TSts], Sts1, PostPairs ) :-
	% options( names(Names), Opts ),
	holds( memberchk(Lr1-Lr2,Pairs), PairFires ),
	k_fold_pairwise_pair_posts( PairFires, Lm1, Lm2, Mls1, Mls2, StaNames, Sts1, Sts2, PostPairs ),
	k_fold_pairwise_predictions_nest( Lrs, Lms, StaNames, Pairs, Lr1, Lm1, TMls, Mls1, TSts, Sts1, PostPairs ).

k_fold_pairwise_pair_posts( true, Lm1, Lm2, Mls1, Mls2, StaNames, Sts1, Sts2, PostPairs ) :-
	write( lm1(Lm1)-lm2(Lm2) ), nl,
	k_fold_pairwise_posts( PostPairs, Lm1, Lm2, Mls1, Mls2, StaNames, Sts1, Sts2 ).
k_fold_pairwise_pair_posts( false, _Lm1, _Lm2, _Mls1, _Mls2, _StaNames, _Sts1, _Sts2, _PostPairs ).

k_fold_pairwise_posts( [], _Lm1, _Lm2, _Mls, _Mls2, _StaNames, _Sts1, _Sts2 ).
k_fold_pairwise_posts( [Post-PostOpts|Posts], Lm1, Lm2, Mls1, Mls2, StaNames, Sts1, Sts2 ) :-
	% findall( PostOpt, member(PostTerm,Opts), PostOpts ),
	trace,
	write( doing( Post, PostOpts, Lm1, Lm2, Mls1, Mls2, StaNames, Sts1,Sts2) ), nl,
	k_fold_comparative_statistic_post_call( Post, [Mls1,Mls2], StaNames, [Sts1,Sts2], [Lm1,Lm2], PostOpts ),
	k_fold_pairwise_posts( Posts, Lm1, Lm2, Mls1, Mls2, StaNames, Sts1, Sts2 ).

k_fold_pairwise_posts_collect( [], [], [] ).
k_fold_pairwise_posts_collect( [H|T], Posts, Other ) :-
	holds( H=post(Post), IsPost ),
	k_fold_pairwise_is_post( IsPost, Post, H, Posts, Other, TPosts, TOther ),
	k_fold_pairwise_posts_collect( T, TPosts, TOther ).

k_fold_pairwise_is_post( true, Post, _H, [Post|Posts], Other, Posts, Other ).
k_fold_pairwise_is_post( false, _Post, H, Posts, [H|Other], Posts, Other ).

/*
	Copts = [ include_stem(model), learner_names(['Age','Clinical']),
			% statistic_names([prediction,auc,harrel,r_square]), 
			segments(Segms),
			rerun_ground(false)
		    ],
	Learners1 = [surv_clm('Agegrp5'),clinical],
	k_fold_comparative_statistic( Data, Learners1, auc(365), Models1, Stats1, Copts ),
	Models1 = [AgeMls,_],
	Stats1  = [AgeSts,_],
	debug( lasso, 'Age x clinical Models: ~w, Stats: ~w', [Models1,Stats1] ),
	% here, Segms is not ground...
	Mopts = [ include_header(true), post(jitter), 
	          include_stem(model), learner_names(['Age','Lasso_min']),
			statistic_names([prediction,auc,harrel,r_square]), segments(Segms),
			rerun_ground(false)
		    ],
	Learners2 = [surv_clm('Agegrp5'),lasso_min],
	k_fold_comparative_statistic( Data, Learners2, auc(365), [AgeMls,LminMls], [AgeSts,LminSts], Mopts ),
	debug( lasso, 'Age x lasso models: ~w, Stats: ~w', [[AgeMls,LminMls],[AgeSts,LminSts]] ).
	*/

k_fold_learn_list( [], _N, _Self, _Dat, _PrG, _Segs, [], [], _Opts ).
% k_fold_learn_list( [LGoalPrv|LGoals], L, Self, Data, PrG, Segs, [Mls|TMls], [Sts|TSts], Opts ) :-
k_fold_learn_list( [LGoalPrv|LGoals], L, Self, Data, PrG, Segs, AMls, ASts, Opts ) :-
	( LGoalPrv = multi(Msfxs,LGoal) -> length(Msfxs,Multi), Add is Multi ; LGoal = LGoalPrv, Multi is 0, Add is 1 ),
	k_fold_comparative_model_stem( LGoal, L, LPstem, Opts ),
	KFOpts = [segments(Segs),model_name_stem(LPstem),predictor(PrG),statistics(Sts)|Opts],
	debug( Self, 'Calling k_fold_learn for ~d-th learner: ~w', [L,LGoal] ),
	k_fold_learn( Data, LGoalPrv, Mls, KFOpts ),
	debug_call( Self, length, models_learnt/Mls ),
	k_fold_learn_list_multi_learned( Multi, Mls, Sts, AMls, ASts, TMls, TSts ),
	M is L + Add,
	k_fold_learn_list( LGoals, M, Self, Data, PrG, Segs, TMls, TSts, Opts ).

k_fold_learn_list_multi_learned( 0, Mls, Sts, [Mls|TMls], [Sts|TSts], TMls, TSts ) :- !.
k_fold_learn_list_multi_learned( M, Mls, Sts, AMls, ASts, TMls, TSts ) :-
	Mls = [HMls|_],
	length( HMls, HLen ),
	( HLen =:= M -> true; throw( multi_learned_mis_match(HMls) ) ),
	findall( IMls, (between(1,M,I),findall(IMl,(member(FMls,Mls),nth1(I,FMls,IMl)),IMls)), NewMls ),
	findall( ISts, (between(1,M,I),findall(ISt,(member(FSts,Sts),nth1(I,FSts,ISt)),ISts)), NewSts ),
	append( NewMls, TMls, AMls ),
	append( NewSts, TSts, ASts ).

	% k_fold_learn_list_multi_learned_m( Mls, M, Sts, AMls, ASts, TMls, TSts ).

k_fold_learn_list_multi_learned_m( [], 0, [], AMls, ASts, AMls, ASts ) .
k_fold_learn_list_multi_learned_m( [Mls|RMls], M, [Sts|RSts], [Mls|MMls], [Sts|MSts], TMls, TSts ) :-
	L is M - 1,
	k_fold_learn_list_multi_learned_m( RMls, L, RSts, MMls, MSts, TMls, TSts ).
