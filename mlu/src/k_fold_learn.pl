
:- use_module( library(random) ).  % random_permutation/2.

:- lib(options).

:- lib( stoics_lib:holds/2 ).
:- lib( stoics_lib:compare/4 ).
:- lib( stoics_lib:n_breaks/4 ).
:- lib( stoics_lib:n_digits_min/3 ).

:- lib( goal_predicate_name/2 ).
:- lib( mlu_mtx_header_rows/4 ).

k_fold_learn_defaults( Defs ) :-
    Defs = [ has_header(true),
             folds(10),        repeats(1),
             predictor(false), include_header(false), 
             include_stem(arity),
             call_options(false),
             segments(_)
            ].

/** k_fold_learn( +Data, +Goal, ?Models, +Opts ).

    Split data to N segments and run Goal on each one to learn
    a Model or a sequence of models. Data should be a list.
    Normally it will be a list of data rows, such as one read by 
    csv_read_file/3. The predicate can use existing partitions of Data 
    (see option segments(Segms)).

    Opts 
     * call_options(Copts=false)
       if anything else than false, then Copts is added as last argument to Goal
       (after it is passed through en_list/2). The option fold(K) is also added
       where K is the number of fold.

     * folds(Folds=10)
       number of segments to split the data to 
       (currently only exhaustive and mutual exclusive splits are supported)

     * include_header(IncHdr=false)
       include header to segments before applying to learner, if HasHdr==true and IncHdr==false false/n
       is added as a header, with each argument being c!, ... cN (N equals to the arity of the first term in Data)

     * include_stem(Stem=arity)
       include automatically generated stem to call. The stem looks like,
       <learner>_<LSeq>_<FSeq>[_<RSeq>. eg truism_A_01_r1
       * arity
         include if there is definition for Goal+1 (accounts for Copts too)

       * false
         does not add Stem no matter what

       * model
         Stem replaces to model argument, 

       * true
         adds it no matter what

     * is_matrix(IsM=true/false)
       whether to pass the data through mtx/2. Default depends on whether mtx/2 
       is in memory (true) or not (false)

     * model_name_stem(Stem)
       Stem for model name

      * predictor(Predictor=false)
        if non false, Predictor is called after calling a learner.
       the Predictor takes Model and (hold out) Data as input and returns Statistic

     * repeats(R=1)
       how many times to run Goal on each fold. If 1, then Models is a list
       of models as returned by goal, else, it is a list of lists of models-
       each sublist corresponding the one repeat run on a segment

     * segments(Segms)
       returns the data segments as passed to the learner. if Segs is ground
       these segments are used rather than a new partition

     * statistics(Statistics)
       one statistic for each application for the Predictor (nested as per Models)

Options term (Opts) is passed to k_fold_segments/3 which does the split.

==
?- k_fold_learn( [a,b,c], true, Mods, [is_matrix(false)] ).
ERROR: pack(mlu): Insufficient length of data (2) as 10 folds are requierd

?- assert(true(_,true)), assert(second(_,false)).

?- debug( mlu(k_fold_learn)), numlist( 1, 33, Tthree ),
   k_fold_learn( Tthree, true, Mods, [is_matrix(false),segments(Segms),repeats(10)]),
   length( Mods, LenMods ), 
   flatten( Mods, AllMods ), 
   length( AllMods, LenAllMods ).

% Data fold partition, segment lengths: [3,3,3,3,4,3,3,3,3,4]
LenMods = 10,
LenAllMods = 100.

==
@author nicos angelopoulos
@version  0.1 2015/12/17
@see  example in pack(mlu/examples/stoic.pl)
@see  k_fold_segments/3
@see  mlu_mtx_header_rows/4

*/
k_fold_learn( MtxF, GoalPrv, Models, Stats, Args ) :-
    ( GoalPrv = multi(GPsfxs,Goal) -> length(GPsfxs,GenM); Goal = GoalPrv, GenM is 0 ),
    options_append( k_fold_learn, Args, Opts ),
    memberchk( segments(Segms), Opts ),
    holds( var(Segms), VarSegms ),
    options( folds(K), Opts ),
    k_fold_cross_learn_segments( VarSegms, MtxF, K, Hdr, Segms, Opts ),
    options( call_options(CoptsOpt), Opts ),
    compare( meta, CoptsOptIsFalse, CoptsOpt, ==(false) ),
    k_fold_call_options( CoptsOptIsFalse, CoptsOpt, HasCopts, Copts ),
    options( repeats(R), Opts ),
    compare( meta, SingR, R, =:=(1) ),
    holds( 1 > K, ApplyTerm ),
    options( include_header(IncHdr), Opts ),
    options( predictor(PredPrv), Opts ),
    holds( PredPrv\==false, HasPred ),
    k_fold_moduled( PredPrv, Pred ),
    options( include_stem(IncStemPrv), Opts ),
    k_fold_include_stem( IncStemPrv, HasCopts, Goal, IncStem ),
    k_fold_moduled( Goal, Moal ),
    number_codes( K, Codes ),
    length( Codes, LenK ),

    ( memberchk(stem(Stem),Opts) -> true;
            ( var(GPsfxs) ->
                  goal_predicate_name(Moal,Stem) 
                  ;
                  goal_predicate_name(Moal,GStem),
                  findall( AStem, (member(Spfx,GPsfxs),atomic_list_concat([GStem,Spfx],'_',AStem)), Stem )
            )
    ),
    k_fold_apply( ApplyTerm, 1, K, LenK, Segms, IncHdr, Hdr, SingR, R, HasPred, Pred, Moal, IncStem/HasCopts, Stem, Copts, GenM, Models, Stats, _Folds ).


k_fold_include_stem( arity, HasCopts, Goal, Exists ) :-
    k_fold_include_stem_has_call_options_arity_increase( HasCopts, HasCoptsNum ),
    ( Goal = Mod:Noal -> true; Noal = Goal, Mod = user ),
    Noal =.. [Pname|Args],
    length( Args, Nrgs ),
    Sum is Nrgs + HasCoptsNum + 2 + 1,
    holds( current_predicate( Mod:Pname/Sum ), Exists ).
k_fold_include_stem( true, _HasCopts, _Goal, true ).
k_fold_include_stem( false, _HasCopts, _Goal, false ).
k_fold_include_stem( model, _HasCopts, _Goal, model ).

k_fold_include_stem_has_call_options_arity_increase(  true, 1 ).
k_fold_include_stem_has_call_options_arity_increase( false, 0 ).

k_fold_cross_learn_segments( true, Data, _K, Hdr, Segms, Opts ) :-
    % we could be passing K again in Opts, but that's where it come for
    k_fold_segments( Data, Hdr, Segms, Opts ).
k_fold_cross_learn_segments( false, MtxF, _K, Hdr, _Segms, Opts ) :-
    mlu_mtx_header_rows( MtxF, Hdr, _Rows, Opts ).

k_fold_apply( true, _I, _K, _LenK, _Segms, _IncH, _Hdr, _SingR, _R, _HasP, _Pctr, _Goal, _HasC, _StemIn, _Copts, _GenM, [], [], [] ).
k_fold_apply( false, I, K, LenK, Segms, IncH, Hdr, SingR, R, HasP, Pctr, Goal, HasCopts, StemIn, Copts, GenM, [SegMs|Ms], [SegStats|Stats], [Data-HoldOut|TFolds] ) :-
    nth1( I, Segms, Seg, Remainder ),
    flatter( Remainder, DataPrv ),
    k_fold_include_header( IncH, Hdr, DataPrv, Data ),
    k_fold_include_header( IncH, Hdr, Seg, HoldOut ),
    n_digits_min( LenK, I, Ipad ),
    ( is_list(StemIn) ->
        findall( AStem, (member(AStemIn,StemIn),at_con([AStemIn,'_m',Ipad],'',AStem)), Stem )
        ;
        at_con( [StemIn,'_m',Ipad], '', Stem )
    ),
    k_fold_apply_segment( SingR, R, Goal, Data, HoldOut, HasCopts, Stem, Copts, HasP, Pctr, GenM, SegMs, SegStats ),
    J is I + 1,
    holds( J > K, Termin ),
    k_fold_apply( Termin, J, K, LenK, Segms, IncH, Hdr, SingR, R, HasP, Pctr, Goal, HasCopts, StemIn, Copts, GenM, Ms, Stats, TFolds ).

k_fold_include_header( true, Hdr, Rows, [Hdr|Rows] ).
k_fold_include_header( false, _Hdr, Rows, Rows ).

k_fold_apply_segment( =, 1, Goal, Seg, HoldOut, HasCopts, Stem, Copts, HasP, Pctr, GenM, SegMs, SegStats ) :-
    k_fold_call( HasCopts, Goal, Seg, HoldOut, Stem, Copts, HasP, Pctr, GenM, SegMs, SegStats ).
k_fold_apply_segment( <>, R, Goal, Seg, HoldOut, HasCopts, Stem, Copts, HasP, Pctr, GenM, SegMs, SegStats ) :-
    compare( meta, Op, R, =:=(0) ),
    k_fold_apply_segment_repeats( Op, R, Goal, Seg, HoldOut, HasCopts, Stem, Copts, HasP, Pctr, GenM, SegMs, SegStats ).

k_fold_apply_segment_repeats( =, 0, _Goal, _Seg, _Hout, _HasCopts, _Stem, _Copts, _HasP, _Pctr, _GenM, [], [] ).
k_fold_apply_segment_repeats( <>, R, Goal, Seg, Hout, HasCopts, StemIn, Copts, HasP, Pctr, GenM, [Mod|Mods], [St|Sts] ) :-
    at_con( [StemIn,'_',r,R], '', Stem ),
    k_fold_call( HasCopts, Goal, Seg, Hout, Stem, Copts, HasP, Pctr, GenM, Mod, St ),
    Q is R - 1,
    compare( meta, Op, Q, =:=(0) ),
    k_fold_apply_segment_repeats( Op, Q, Goal, Seg, Hout, HasCopts, Stem, Copts, HasP, Pctr, GenM, Mods, Sts ).

% k_fold_call( IncStem/IncOpts, Copts, Goal, Segm, Hout, HasP, Pctr, Model, Statistic ) :-
k_fold_call( true/true, Goal, Segm, Hout, Stem, Copts, HasP, Pctr, GenM, Model, Statistic ) :-
    !,
    call( Goal, Segm, Stem, Model, Copts ),
    k_fold_call_predictor( HasP, Pctr, Model, Hout, GenM, Statistic ).
k_fold_call( true/false, Goal, Segm, Hout, Stem, _Copts , HasP, Pctr, GenM, Model, Statistic ) :-
    !,
    call( Goal, Segm, Stem, Model ),
    k_fold_call_predictor( HasP, Pctr, Model, Hout, GenM, Statistic ).
k_fold_call( false/true, Goal, Segm, Hout, _Stem, Copts, HasP, Pctr, GenM, Model, Statistic ) :-
    !,
    call( Goal, Segm, Model, Copts ),
    k_fold_call_predictor( HasP, Pctr, Model, Hout, GenM, Statistic ).
k_fold_call( false/false, Goal, Segm, Hout, _Stem, _Copts, HasP, Pctr, GenM, Model, Statistic ) :-
    !,
    call( Goal, Segm, Model ),
    k_fold_call_predictor( HasP, Pctr, Model, Hout, GenM, Statistic ).
k_fold_call( model/true, Goal, Segm, Hout, Stem, Copts, HasP, Pctr, Stem, GenM, Statistic ) :-
    !,
    call( Goal, Segm, Stem, Copts ),
    k_fold_call_predictor( HasP, Pctr, Stem, Hout, GenM, Statistic ).
k_fold_call( model/false, Goal, Segm, Hout, Stem, _Copts, HasP, Pctr, Stem, GenM, Statistic ) :-
    !,
    call( Goal, Segm, Stem ),
    k_fold_call_predictor( HasP, Pctr, Stem, Hout, GenM,  Statistic ).

k_fold_call_predictor( true, Predictor, Model, Hout, GenM, Statistic ) :-
    k_fold_call_predictor_multi( GenM, Predictor, Model, Hout, Statistic ).
k_fold_call_predictor( false, _Predictor, _Model, _Hout, _GenM, false ). % fixme when GenM generate Genm false list

k_fold_call_predictor_multi( 0, Predictor, Model, Hout, Statistic ) :-
    !,
    call( Predictor, Model, Hout, Statistic ).
k_fold_call_predictor_multi( M, Predictor, Models, Hout, Statistics ) :-
    length( Models, LenMs ),
    ( LenMs =:= M -> true;  throw( missmatch(number_of_models_generated(M,LenMs)) ) ),
    findall( Statistic, (member(Model,Models), call(Predictor,Model,Hout,Statistic) ), Statistics ).

k_fold_call_options( <>, CoptsOpt, true, Copts ) :-
    en_list( CoptsOpt, Copts ).
k_fold_call_options( =, false, false, false ).



flatter( [], [] ).
flatter( [H|T], Data ) :-
    append( H, Var, Data ),
    flatter( T, Var ).

/*
flatter_dl( [H|T], Var ) :-
    append( H, V

*/
debug_segment_lengths( true, Segms ) :-
    maplist( length, Segms, Lens ),
    debug( mlu(k_fold_learn), 'Data fold partition, segment lengths: ~w', [Lens] ).
debug_segment_lengths( false, _Segms ).

k_fold_moduled( Goal, Moduled ) :-
    compare( meta, HasModule, Goal, =(_:_) ),
    k_fold_goal( HasModule, Goal, Moduled ).

k_fold_goal( =, Goal, Goal ).
k_fold_goal( <>, Goal, user:Goal ).
