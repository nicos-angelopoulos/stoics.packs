
:- ensure_loaded( library(requires) ).
:- lib( mlu ).

/** stoic_ng.

Use this version if you don't have library(real), 
@see http://eu.swi-prolog.org/pack/list?p=real
to avoid producing the jitter comparative plot.

@see stoic/0

*/
stoic_ng :-
	numlist( 0, 10, Ten ),       % 0 is the "header", datapoints: 1,...,10
	Ls = [falsehood,truism],
	Rnd = bias,
	Opts = is_matrix(false),
	k_fold_pairwise_comparisons( Ten, Ls, Rnd, Ms, Sts, Opts ),
	write( at_end(models(Ms) ) ), nl,
	write( at_end(stats(Sts) ) ), nl.

/** stoic.

Runs a comparative 10 fold cross validation between 2 learners and plots a jitter 
plot of the results (needs library(real)).

The model learners are defined in truism/2 and falsehood/2. They always learn model 
'true' and 'false' respectively. The models are passed through predictor bias/2 which gives
a different bias in its 4 prediction values according to whether the tested model
is 'true' or 'false'.

@author  nicos angelopoulos
@version 0.1   2016/1/30
@see http://eu.swi-prolog.org/pack/list?p=real

*/
stoic :-
	numlist( 0, 10, Ten ),      % 0 is the "header", datapoints: 1,...,10
	Ls = [falsehood,truism],
	Rnd = bias,
	debug( mlu(k_fold_learn) ),
	Opts = [is_matrix(false),post(jitter),
		   statistic_names(['1 year AUC','AUC(1Y)','Harrel\'s C','R square']),
	        jitter(postfix(abuc)),jitter(pval(false))],
	k_fold_pairwise_comparisons( Ten, Ls, Rnd, Ms, Sts, Opts ),
	write( at_end(models(Ms) ) ), nl,
	write( at_end(stats(Sts) ) ), nl,
    write( 'Look at local directory for generated pdf.' ), nl.

% model builders
%
truism(_,true).
falsehood(_,false).	

% predictors
%
bias( false, _Data, c(Rnd,Rnd,Rnd2,Rnd3) ) :-
	random( 0.5, 1, Rnd ),
	random( 0.65, 1, Rnd2 ),
	random( 0.7, 1, Rnd3 ).
bias( true, _Data, c(Rnd,Rnd,Rnd2,Rnd3) ) :-
	random( 0.6, 1, Rnd ),
	random( 0.75, 1, Rnd2 ),
	random( 0.8, 1, Rnd3 ).

/*

singlo :-
	numlist( 0, 10, Ten ),
	Ls = [falsehood,truism],
	Rnd = bias_predictor,
	Opts = [is_matrix(false), post(jitter), statistic_names(['1 year AUC','AUC(1Y)','Harrel\'s C','R square']),
	        jitter(postfix(single))
		   ],
	k_fold_comparative_statistic( Ten, Ls, Rnd, _Ms, _Sts, Opts ).

bias_predictor( true, _, c(Rnd,Est,Bst) ) :-
	random( 0.5, 1, Rnd ),
	Est is Rnd + ( (1 - Rnd) /2 ),
	Bst is Est / (2*Rnd).
bias_predictor( false, _, c(Rnd,Est,Bst) ) :-
	random( 0.6, 1, Rnd ),
	Est is Rnd + ( (1 - Rnd) /2 ),
	Bst is Est / (2*Rnd).

kf :-
	numlist( 1, 10, Data ),
	Learners = [falsehood,truism],
	Predictor = bias, 
	Opts = [is_matrix(false),post(jitter),statistic_names([prediction,auc,buc]),
	        jitter(postfix(abuc)),jitter(pval(false))],

	k_fold_learn( here ).
	*/
