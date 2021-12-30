
:- use_module( library(real) ).
% :- lib(options).  % options_debug/3.

/** options_rvar_rmv( +Rvar, +Opts ).

r_remove/1 R variable Rvar iff rvar_rmv(true) is in Opts.
 
The rationale is to make rvar_rmv(_) a commonly occured option
in b_real predicates.

==
?- x <- 1.
?- <- x.
[1] 1
true.
?- options_rvar_rmv( x, [rvar_rmv(true)] ).
?- <- x.
% Error in print(x) : object 'x' not found
% ERROR: R was unable to digest your statement, either syntax or existance error
?- x <- 1.
?- options_rvar_rmv( x, [rvar_rmv(false),debug(true)] ).
% Keeping R variable: x
true.
==

@author nicos angelopoulos
@version  0.1 2015/1/15
@tbd debug messages via options_debug/3 library(options_debug)
*/

options_rvar_rmv( Rvar, Opts ) :-
	memberchk( rvar_rmv(Rmv), Opts ),
	!,
	ground( Rmv ), % fixme: report rather than fail
	option_boolean_rvar_rmv( Rmv, Rvar, Opts ).
options_rvar_rmv( _Rvar, _Opts ).

option_boolean_rvar_rmv( true, Rvar, Opts ) :-
	!,
	options_debug( 'Removing R variable: ~w', [Rvar], Opts ),
	r_remove( Rvar ).
option_boolean_rvar_rmv( _, Rvar, Opts ) :-
	options_debug( 'Keeping R variable: ~w', [Rvar], Opts ).
