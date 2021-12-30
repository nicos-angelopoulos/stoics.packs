
% :- use_module( library(real) ).
:- lib(real).
:- lib(suggests(mtx)).
:- lib(options).

:- lib(pl_vector/3).
:- lib(max_n_fractional/3).

lm_plot_defaults( [max_frac(4)] ).

/** lm_plot( +Vect1, +Vect2, +Opts ).

Create a scatter plot for vectors Vect1 and Vect2 (pl_vector/2) along with 
fitting a linear model.

Pval is the anova p.value of Clm2 ~ Clm1, Coef is the coefficient of the
regression, R2 is R squared (coefficient of determination). See options.

Opts
  * Ropt = Rval
    pairs are passed to the plot call (via r_call/2) (defaults for main, xlab, ylab and col provided)
  * coef(Coef) 
    return value of the coefficient
  * max_frac(MxFrac=4)
    maximum number of fractional part
  * mtx(Mtx)
    used if Vect1,2 are not lists, see pl_vector/2
  * outputs(Outs)
    outputs to produce, see r_call/2
  * pval(Pval) 
    return value of the p value
  * r2(R2) 
    return value of the R2 
  * stem(Stem)
    stem for any output files (see r_call/2)
  * RcallOpts
    options supported by r_call/2

==
?- mtx_data( mtcars, Mt ), 
   lm_plot( mpg, disp, mtx(Mt) ).

?- assert( lm_mtx( [row(a,b,c),row(1,2,3),row(2,3,4),row(3,4,5)] ) ).
?- lm_mtx( Mtx ), lm_plot( a, b, C, R2, P, mtx(Mtx) ).
C = 1.0,
P = 0.0.

?- lm_mtx(Mtx),
   lm_plot(Mtx,a,b,C,R2,P,true,).

C = 1.0,
P = 0.0.

?- ls.  
what.png
?- use_module( library(by_unix) ).
?- @ eog(what.png).
==

@author nicos angelopoulos
@version  0.1 2014/6/30
@version  0.1 2014/1/23, mtx updates + R2
*/
lm_plot( Cid1, Cid2, OptsIn ) :-
	options_append( lm_plot, OptsIn, Opts ),
	pl_vector( Cid1, Clm1, [cnm(Cnm1)|Opts] ),
	pl_vector( Cid2, Clm2, [cnm(Cnm2)|Opts] ),
	% fixme: make better provisions for Cnm1 & Cnm2 when Cid1, and Cid2 are lists.
	( var(Cnm1) -> Cnm1 = x; true ),
	( var(Cnm2) -> Cnm2 = y; true ),
	lmr <- lm( Clm2 ~ Clm1 ), % fixme
	Coef <- 'coef(lmr)[2]',
	lma <- anova( lmr ),
     Pval <- 'lma[5][1,1]',
	R2 <- summary(lmr)$r.squared,
	options( max_frac(MxFrac), Opts ),
	maplist( max_n_fractional(MxFrac), [Coef,R2,Pval], [CoefR,R2R,PvalR] ),
	Ls = [Cnm2,'~',Cnm1,' \n coef=',CoefR,', r^2=', R2R, ', pval=',PvalR],
	write( ls(Ls) ), nl,
	atomic_list_concat( Ls, Main ),
	Abline = (<- abline( lmr, col="darkgreen" ) ),
	% fixme: below, be more considerate to user's post_call()
	append( [post_call(Abline)|Opts], [main=+Main, xlab=+Cnm1, ylab=+Cnm2, col="darkorange"], ROpts ),
	r_call( plot(Clm1,Clm2), ROpts ),
	Rets = [coef(Coef),pval(Pval),r2(R2)],
	options_return( Rets, Opts ).
