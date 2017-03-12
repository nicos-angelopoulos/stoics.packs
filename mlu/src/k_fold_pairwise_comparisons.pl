
% :- lib( real ).                 % move all real components in their own file
:- ensure_loaded( library(real) ).

:- lib( stoics_lib:holds/2 ).
:- lib( stoics_lib:arity/3 ).
:- lib( stoics_lib:at_con/3 ).

:- lib( onoma/2 ).
:- lib( arg_append/3 ).
:- lib( trunc/3 ).

k_fold_comparative_statistic_defaults( Defs ) :-
    Defs = [  rerun_ground(true), learner_names(_), names(_) ].

:- multifile( k_fold_comparative_statistic/6 ).

/** k_fold_pairwise_comparisons( +Data, +Learners, +Predictors, -Models, -Statistics, +Opts ).

Compare M learners, Goals, over N cross sections of Data. 
On each of the N iterations, the learners are ran on the N-1 sections and tested on
the hold out Nth section. 
If a single Predictor is given, (singleton list or unlisted), then it is used
on all Learerns. Else number should be equal to number of lerners. There
should be at least two Learners, else you better call k_fold_learn directly
(see Predictor and Statistic options there).

Opts
  * learner_names(Lnames) 
     used to distinguish the models for each learner, 
     an l prefixed atom is used by default

  * names(Names)
     Names for Models. Unlike other non post() options, who are passed
     on to Post calls via options, this is passed to explicitly. 
     This not used in main call, and it defaults to model_01...model_NN
     If a variable, the generated names are returned.

  * post(Post)
     Post processing after Models + Statistics are constructed
     Known Post values: 
     * jitter

     * statistic_names(StaNames)
        names for the statistics

Additional Opts for the Post are allowed. For each Post, all options with matching outer term name
are stripped and passed. For instance jitter(accuracy(2,'AUC')) is passed to Post jitter, and 
it signifies that the predictor passes as it is second argument the AUC of the model against the 
leave out segment. The implementation is expected correct at the predictor end, here we just
provide means to pass the information to the plotter.

Also see options for k_fold_learn/4.

Jitter
  * accuracy(N,Name)
      the Nth (>1) position of the statistic is accuracy identified by Name

  * accuracy(Pname)
      predicate name for obtaining the accuracy names, called as call(Pname,N,Name)

  * average_accuracy(Avg=mean)
      R function for obtaining the single accuracy from the k_fold accuracies

  * rerun_ground(Rerun=true)
        set to false to avoid re-runing ground models. Convenient for running 
       comparatives

==
    ?- [pack(mlu/examples/stoic)].

    ?- stoic.  % use ?- stoic_ng.  % if you do not have library(real).

==


*/
k_fold_pairwise_comparisons( Data, Learners, PredictorsPrv, Models, Statistics, Args ) :-
    options_append( k_fold_comparative_statistic, Args, Opts ),
    k_fold_comparative_statistic_predictors( PredictorsPrv, Learners, Predictors, Opts ),
    options( rerun_ground(Rerun), Opts ),
    k_fold_comparative_statistic_call( Learners, Rerun, 0, Data, Predictors, Models, Statistics, Opts ),
    options( names(Names), Opts ),
    options( learner_names(Lames), Opts ),
    k_fold_posts( Opts, Learners, Models, Posts, Names, Lames, OthOpts ),
    % options( statistic_names(StaNames), Opts ),
    % holds( var(StaNames), VarStaNames ),
    % k_fold_statistic_names_ground( VarStaNames, Predictors, StaNames ),
    % here is where the statistic part happens...
    options( statistic_names(StaNames), Opts ),
    k_fold_comparative_statistic_post( Posts, Models, StaNames, Statistics, Lames, OthOpts ).

/*
k_fold_statistic_names_grouk_fold_comparative_statistic-komos-16.01.04.plnd( false, _Predictors, _StaNames ).
k_fold_statistic_names_ground( true, Predictors, StaNames ) :-
    findall( StaName, (member(P,Predictors),arity(P,StaName,_)), StaNames ).
    */
    
k_fold_posts( [], Learners, Models, [], Names, Lames, [] ) :-
    holds( var(Names), VarNames ),
    k_fold_posts_names( VarNames, Learners, Models, Names, Lames ).
k_fold_posts( [H|T], Learners, Models, Posts, Names, Lames, Opts ) :-
    holds( H=post(_), IsPost ),
    k_fold_post( IsPost, H, Posts, TPosts, Opts, TOpts  ),
    k_fold_posts( T, Learners, Models, TPosts, Names, Lames, TOpts ).

k_fold_post( true, post(Post), [Post|Posts], Posts, Opts, Opts  ).
k_fold_post( false, NonPost, Posts, Posts, [NonPost|Opts], Opts  ).

k_fold_post_model_names( false, _Models, Lames, Lames ). % for now
k_fold_post_model_names( true, Models, Lames, Names ) :-
    length( Models, NofMs ),
    NofPs is floor( log10(NofMs) ),
    findall( SubNames, (
                        nth1(N,Models,NModels),
                        nth1(N,Lames,Lame),
                        % k_fold_z_pad(NModels,Zad),
                        findall( Sub, ( nth1(M,NModels,_NMod),
                                     k_fold_z_pad(NofPs,M,Zad),
                                        at_con([Lame,'_m',Zad],'',Sub)
                                      ),  SubNames )
                       ), Names ).

k_fold_z_pad( NofPs, M, Zad ) :-
    NofMPs is floor( log10(M) ),
    NofZs is NofPs - NofMPs,
    findall( 0, between(1,NofZs,_), Zeros ),
    append( Zeros, [M], Parts ),
    at_con( Parts, '', Zad ).

k_fold_posts_names( true, Learners, Models, Names, Lames ) :-
    holds( ground(Lames), Games ),
    k_fold_posts_lerner_names( Games, Learners, Models, Lames ),
    Models = [Hm|_],
    holds( is_list(Hm), IsList ),
    k_fold_post_model_names( IsList, Models, Lames, Names ).
k_fold_posts_names( false, _Learners, _Models, _Names, _Lames ).

k_fold_posts_lerner_names( true, _Learners, _Models, _Lames ).
k_fold_posts_lerner_names( false, Learners, _Models, Lames ) :-
    sort( Learners, Searners ),
    length( Learners, Llen ),
    length( Searners, Slen ),
    holds( Llen=:=Slen, EnoughNames ),
    k_fold_posts_learner_names_complete( EnoughNames, Learners, Lames ).

k_fold_posts_learner_names_complete( true, Learners, Lames ) :-
    findall( Lame, (member(Learner,Learners),goal_predicate_name(Learner,Lame)), Lames ).
k_fold_posts_learner_names_complete( false, Learners, Lames ) :-
    length( Learners, NofLs ),
    MaxNofZs is floor( log10(NofLs) ),
    % findall( '0', between(1,NofLs,_), Zeroes ),
    findall( Lame, (  
                      nth1(I,Learners,Learner),
                   NofZs is floor( log10(I) ),
                   Gap is MaxNofZs - NofZs,
                   findall( 0, between(1,Gap,_), Zs ),
                   goal_predicate_name( Learner, Lname ),
                   at_con([Lname,'_',Zs],Lame)
                ), 
                      Lames ).

    % atomic_list_concat( Zeroes, Pad ),
    % holds( NofMs =:= 0, Termin ),
    % Pfx = lrn,
    % k_fold_posts_default_names( Termin, 1, NofMs, Pfx, Pad, Lames ).

k_fold_posts_default_names( false, I, NofMs, Pfx, Pad, [Name|Names] ) :-
    atomic_list_concat( [Pfx,'_',Pad,I], '', Name ),
    J is I + 1, 
    holds( J > NofMs, Termin ),
    k_fold_posts_default_names( Termin, J, NofMs, Pad, Names ).
k_fold_posts_default_names( true, _I, _NofMs, _Pad, [] ).

k_fold_comparative_statistic_post( [], _Models, _StaNames, _Statistics, _Lames, _Opts ) :-
    debug( mlu, 'Done with post processing statistics.', [] ).
k_fold_comparative_statistic_post( [Post|Posts], Models, StaNames, Statistics, Lames, Opts ) :-
    ( atomic(Post) -> PostName = Post; functor(Post,PostName,_,_) ),
    PostOptWrap =.. [PostName,PostOpt],
    findall( PostOpt, member(PostOptWrap,Opts), PostOpts ),
    k_fold_comparative_statistic_post_call( Post, Models, StaNames, Statistics, Lames, PostOpts ),
    k_fold_comparative_statistic_post( Posts, Models, StaNames, Statistics, Lames, Opts ).

k_fold_comparative_statistic_post_call( cox_cohese, Models, StaNames, Statistics, Lames, _Opts ) :-
    % throw( here(Models,Lames,Opts) ).
    maplist( cox_cohese_summary(StaNames), Models, Statistics, Lames, _Files ).
    % write( wrote_csv_files(Files) ), nl.

% here: extend to jitter(Which).
% k_fold_comparative_statistic_post_call( jitter, _Models, StaNames, Statistics, Lames, Opts ) :-
k_fold_comparative_statistic_post_call( jitter, Models, StaNames, Statistics, Lames, Opts ) :-
    length( StaNames, Len ),
    k_fold_comparative_statistic_post_call( jitter(Len), Models, StaNames, Statistics, Lames, Opts ).

k_fold_comparative_statistic_post_call( jitter(UpTo), _Models, StaNames, Statistics, Lames, Opts ) :-
    % Stats = [[c(A1p1,A1p2,A1p3),c(A2p1,A2p2,A2p3)],[c(B1p1,B1p2,B1p3),c(B2p1,B2p2,B2p3)]]
    % assume for now inners are always compunds, ...
    Defs = [ onoma(true), accuracy_average(mean), postfix(''), pval(true) ],
    /* */
    % Statistics = [[StaTerm|_],[_|_]],  % for now only supporting pairs...
    % functor( StaTerm, _, Arity ),
    % length( StaNames, Arity ),  % sanity check
    ( length(StaNames,UpTo) -> true; throw( fixme(sta_names_of_wrong_length_in_jitter(UpTo,StaNames)) ) ),
    append( Defs, Opts, All ),
    % ExcLast is Arity - 1,
    % ( Lames = ['Age',muts_ose] -> trace; write( no_tracing(Lames) ), nl ),
    k_fold_comparative_statistic_post_call_jitter( UpTo, StaNames, Statistics, Lames, All ).

% this is for single learner, called upstream by maplist/n
%
cox_cohese_summary( StaNames, Models, StatisticsTerm, Lame, File ) :-
    findall( Vars, (member(Mod,Models), VarS <- rownames( summary(Mod)$coef ),en_list(VarS,Vars)), VarsNest ),
    findall( CoeM, (member(Mod,Models), CoeM <- summary(Mod)$coef), CoeMNest ),
    flatten( VarsNest, FlatVars ),
    sort( FlatVars, AllVars ),
    findall( row(Var,Appr,Coef,Z,Pv), 
                ( member(Var,AllVars),
                  findall( 1, (member(VarsA,VarsNest),memberchk(Var,VarsA)), Ones ),
                  sumlist( Ones, Appr ),
                  findall( c(Coef,Z,Pv), ( nth1(VarsAP,VarsNest,VarsA), nth1(VarP,VarsA,Var),
                                        nth1(VarsAP,CoeMNest,CoemA), nth1(VarP,CoemA,Coem),
                                        Coem = [Coef,_,_,Z,Pv]
                                  ),
                                    CoefTerms
                                  ),
                  maplist( arg(1), CoefTerms, Coefs ), Coef <- median(Coefs),
                  maplist( arg(2), CoefTerms, Zs ),    Z    <- median(Zs),
                  maplist( arg(3), CoefTerms, Pvs ),   Pv   <- median(Pvs)
                ),
             Varrows
            ),
    % StaNamesT =.. [row|StaNames],
    % StaMtx    = [StaNamesT|Statistics],
    % Statistics = [StatisticsA|_],
    % StatisticsA = [StatsATerm|_],
    % functor( StatsATerm, _, Arity ),
    % N is Arity - UpTo,
    maplist( remove_last_n_args(1), StatisticsTerm, StatisticsOnly ),
    rows_cohese( Varrows, StatisticsOnly, 5, 4, Csv ),
    atomic_list_concat( [cox,appears,summary,Lame], '_', Stem ),
    os_ext( csv, Stem, File ),
    once( append(StaNamesOnly,[_],StaNames) ),
    Hdr =.. [row,var,appears,coef,z,pval|StaNamesOnly],
    csv_write_file( File, [Hdr|Csv] ).
    % csv_write_file( [row('var',coef,z,pval,)|Csv], File, [match_arity(false)] ).
    
    /*
remove_last_arg_list( Terms, Reds ) :-
    maplist( remove_last_arg, Terms, Reds ).
    */

remove_last_n_args( N, Term, Red ) :-
    Term =.. [Name|Args],
    length( List, N ),
    once( append(Goods,List,Args) ),
    Red =.. [Name|Goods].

rows_cohese( [], [], _N, _M, [] ) :- !.
rows_cohese( [], Posts, N, _M, Rs ) :- !,
    findall( '', between(1,N,_), Elist ),
    Eterm =.. [row|Elist],
    findall( R, (member(P,Posts),arg_append(Eterm,P,R)), Rs ).
rows_cohese( Pres, [], _N, M, Rs ) :- !,
    findall( '', between(1,M,_), Elist ),
    Eterm =.. [row|Elist],
    findall( R, (member(P,Pres),arg_append(P,Eterm,R)), Rs ).
rows_cohese( [Pre|Pres], [Post|Posts], N, M, [R|Rs] ) :-
    arg_append( Pre, Post, R ),
    rows_cohese( Pres, Posts, N, M, Rs ).

/* the following is a "wrong" stub
     if we jitter a multi predictor output, 
     its term arguments should 
     1st  
*/
k_fold_comparative_statistic_post_call_jitter( 0, _StaNames, _Stats, _LNames, _Opts ) :- !.
k_fold_comparative_statistic_post_call_jitter( 1, StaNames, [StsA,StsB], [LA,LB], Opts ) :- !,
    maplist( arg(1), StsA, NtsA ),
    maplist( arg(1), StsB, NtsB ),
    % nth1( 1, StaNames, NStaName ),
    StaNames = [NStaName],
    jitter_plot( NtsA, NtsB, LA, LB, NStaName, [], Opts ).
k_fold_comparative_statistic_post_call_jitter( N, StaNames, [StsA,StsB], [LrnA,LrnB], Opts ) :-
    maplist( arg(1), StsA, NtsA ),
    maplist( arg(1), StsB, NtsB ),
    StaNames = [StName|_],
    options( accuracy_average(AccFunc), Opts ),
    holds( 2>N, Termin ),
    k_fold_comparative_statistic_jitter_accuracies( Termin, 2, N, StsA, StsB, StaNames, AccFunc, AccPairs ),
    ( jitter_plot( NtsA, NtsB, LrnA, LrnB, StName, AccPairs, Opts ) -> true
            ; write( jitter_plot_failure(NtsA,NtsB,LrnA,LrnB,StName,AccPairs,Opts) ), nl ).

k_fold_comparative_statistic_jitter_accuracies( true, _O, _N, _StsA, _StsB, _StaNames, _AccFunc, [] ).
k_fold_comparative_statistic_jitter_accuracies( false, L, N, StsA, StsB, SttNames, AccFunc, [SttName=c(SttAvgA,SttAvgB)|TPairs] ) :-
    maplist( arg(L), StsA, NtsA ),
    maplist( arg(L), StsB, NtsB ),
    nth1( L, SttNames, SttName ),
    AvgARall =.. [AccFunc,NtsA],
    AvgBRall =.. [AccFunc,NtsB],
    SttAvgA <- AvgARall,
    SttAvgB <- AvgBRall,
    M is L + 1,
    holds( M > N, Termin ),
    k_fold_comparative_statistic_jitter_accuracies( Termin, M, N, StsA, StsB, SttNames, AccFunc, TPairs ).
    

jitter_plot( NtsA, NtsB, LrnA, LrnB, StName, AccPairs, Opts ) :- % fixme, fish odir() here
    length( NtsA, Len ),
    length( NtsB, Len ),
    options( onoma(Onoma), Opts ),
    % fixme throw error if not == 
    at_con( [StName,LrnA,vs,LrnB], '_', Base ),
    options( postfix(Psf), Opts ),
    os_ext( pdf, Base, PdfFPrv ),
    os_postfix( Psf, PdfFPrv, PdfF ),
    append( NtsA, NtsB, Nts ),
    % <- plot(jitter(rep(1:2,each=Len)), Nts, ylim=c(0.5,1), axes='FALSE', ylab="Harrel's C", xlab="", pch=20, asp=3)
    ( Onoma == true -> maplist(onoma,[StName,LrnA,LrnB],[Ylab,OnmA,OnmB])
                       ; 
                    Ylab=StName, OnmA=LrnA, OnmB=LrnB 
    ),
    ( AccPairs = [_|_] -> Xlim=c(0.5,3),Width=9; Xlim=c(1,2), Width=7 ),
    <- pdf( +PdfF, width=Width, height=7 ),
    <- plot(jitter(rep(1:2,each=Len)), Nts, ylim=c(0.5,1), xlim=Xlim, axes='FALSE', ylab=+Ylab, xlab="", pch=20, asp=3),
    % <- axis(side=1, at=1:2, labels = c(+OnmA,+OnmB), col.axis=c("red","green") ), % col.lab=c("#4B5320","#21ABCD") ),
    ColA = "#177245", % dark spring green
    ColB = "#21ABCD", % ball blue
    <- axis(side=1, at=1, labels = c(+OnmA), 'col.axis'=ColA ),
    % <- axis(side=1, at=1, labels = c(+OnmA), col.axis=c("#03C03C") ), % 
    % <- axis(side=1, at=1, labels = c(+OnmA), col.axis=c("#006B3C") ), % 
    % <- axis(side=1, at=1, labels = c(+OnmA), col.axis=c("#21CDAB") ), % 
    <- axis(side=1, at=2, labels = c(+OnmB), 'col.axis'=ColB ),
    <- axis(side=1, at=1:2, labels= c("","") ), % col.lab=c("#4B5320","#21ABCD") ),
    <- axis(side=2, at=(5:10)/10),
    at_con( [Ylab,for,LrnA,vs,LrnB,models], ' ', Main ),
    % <- title("Harrel's C for baseline vs full model"),
    <- title( +Main ),
    MeanA <- mean(NtsA),
    MeanB <- mean(NtsB),
    Hw is 0.1, MinXA is 1 - Hw, MaxXA is 1+Hw, MinXB is 2 - Hw, MaxXB is 2 + Hw,
    <- segments( c(MinXA,MinXB), c(MeanA,MeanB), c(MaxXA,MaxXB), c(MeanA,MeanB) ),
    ( options(pval(true),Opts) -> 
        Ttest <- 't.test'( NtsA, NtsB, paired='TRUE' ),
        memberchk( 'p.value'=TtPval, Ttest ),
        atom_number( TtPvalAt, TtPval ),
        once( sub_atom(TtPvalAt,0,6,_,TtPvalAtPfx) ),
        <- text( 1.5, 1.0, +TtPvalAtPfx, col="#E25822" ) % flame
        ;
        true
    ),
    % <- text( 1.5, 1.0, +TtPvalAtPfx, col="#87A96B" ),
    % <- text( 1.5, 1.0, +TtPvalAtPfx, col="#CC4E5C" ),
    % <- text( 1.5, 1.0, +TtPvalAtPfx, col="#E9692C" ), % deep carrot orange

    % flighing low...
    jitter_plot_averaged_statistics( AccPairs, 1, 0.03, ColA, ColB ),
    <- 'dev.off'(),
    debug_call( mlu(k_fold_comparative_statistic), wrote, PdfF ).

jitter_plot_averaged_statistics( [], _Y, _Step, _ColA, _ColB ).
jitter_plot_averaged_statistics( [SttName=c(SttA,SttB)|T], Y, Step, ColA, ColB ) :-
    <- text( 3, Y, +SttName, pos=2 ),
    maplist( trunc(6), [SttA,SttB], [TruA,TruB] ),
    <- text( 3, Y-Step, TruA, col=ColA, pos=2 ),
    <- text( 3, Y-(2*Step), TruB, col=ColB, pos=2 ),
    Z is Y - (4*Step),
    jitter_plot_averaged_statistics( T, Z, Step, ColA, ColB ).

k_fold_comparative_statistic_call( [], _ReR, _L, _Data, [], [], [], _Opts ).
k_fold_comparative_statistic_call( [LGoal|Learners], ReR, L, Data, [PGoal|Prcts], [LMs|Ms], [LSts|Sts], Opts ) :-
    holds( (ReR==false,ground(LMs)), DontRun ),
    Self = mlu(k_fold_comparative_statistic),
    k_fold_comparative_statistic_call_rerun( DontRun, LGoal, L, Data, PGoal, Self, LMs, LSts, Opts ),
    % en_list( LearnedMs, LearnedMsL ),
    % en_list( LearnedSts, LearnedStsL ),
    % k_fold_comparative_statistic_stitch( LearnedMsL, LearnedStsL, Ms, Sts, TMs, TSts ),
    M is L + 1,
    k_fold_comparative_statistic_call( Learners, ReR, M, Data, Prcts, Ms, Sts, Opts ).

k_fold_comparative_statistic_call_rerun( true, LG, _L, _D, _P, Self, Ms, _Sts, _Opts ) :-
    Mess = 'Skipping k_fold_learn for learner:~w with ground Models: ~w',
    debug( Self, Mess, [LG,Ms] ).
k_fold_comparative_statistic_call_rerun( false, LGoal, L, Data, PGoal, Self, Ms, Sts, Opts ) :-
    k_fold_comparative_model_stem( LGoal, L, LPstem, Opts ),
    KFOpts = [model_name_stem(LPstem),predictor(PGoal)|Opts],
    debug( Self, 'Calling k_fold_learn for learner: ~w', LGoal ),
    k_fold_learn( Data, LGoal, Ms, Sts, KFOpts ),
    debug( Self, 'Learnt odels: ~w', [Ms] ),
    debug( Self, 'Stats of learned: ~w', [Sts] ),
    debug_call( Self, length, models_learnt/Ms ).

k_fold_comparative_model_stem( _LGoal, L, LPstem, Opts ) :-
    memberchk( learner_names(Lnames), Opts ),
    nth1( L, Lnames, Lname ),
    ground( Lname ),
    !,
    downcase_atom( Lname, LPstem ). % fixme, also replace ' ' by _
k_fold_comparative_model_stem( LGoal, L, LPstem, _Opts ) :-
    goal_predicate_name( LGoal, LPname ),
    % shall we also check for groundness of LM here ?
    LetterCode is 0'A + L,
    atom_codes( LetterAtom, [LetterCode] ),
    downcase_atom( LPname, LPdown ),
    at_con( [LPdown,LetterAtom], '_', LPstem ).

k_fold_comparative_statistic_stitch( [], [], TMs, TSts, TMs, TSts ).
k_fold_comparative_statistic_stitch( [M|Ms], [St|Sts], [M|MMs], [St|MSts], TMs, TSts ) :-
    k_fold_comparative_statistic_stitch( Ms, Sts, MMs, MSts, TMs, TSts ).

k_fold_comparative_statistic_predictors( PredictorsPrv, Learners, Predictors, _Opts ) :-
    en_list( PredictorsPrv, PredictorsL ),
    length( PredictorsL, Plen ),
    length( Learners, Llen ),
    holds( Plen=:=1, Singleton ),
    k_fold_comparative_statistic_predictors_complement( Singleton, Plen, Llen, PredictorsL, Predictors ).

k_fold_comparative_statistic_predictors_complement( true, 1, Llen, [PredictorInv], Predictors ) :-
    findall( Predictor, (between(1,Llen,_),copy_term(PredictorInv,Predictor)), Predictors ).
k_fold_comparative_statistic_predictors_complement( false, Plen, Llen, Predictors, Predictors ) :-
    holds( Plen=:=Llen, EqLen ),
    k_fold_comparative_statistic_predictors_complement_equal_length( EqLen, Plen, Llen ),

k_fold_comparative_statistic_predictors_complement_equal_length( true, _Plen, _Llen ).
k_fold_comparative_statistic_predictors_complement_equal_length( false, Plen,  Llen ) :-
    % throw( pack_error(mlu,true,,learners_predictors_unequal_length(Llen,Plen)) ).
    Pred = k_fold_comparative_statistic/6,
    throw( pack_error(mlu,Pred,lengths_mismatch(learners,predictors,Llen,Plen)) ).
