
:- lib(real).
:- lib(arity/3).  % stoics_lib

/** pl_plot_on( +Goal, +On ). 

	Call Goal while recording (R) output to On.
	On should be wrappend in plot_on/1, else it is ignored silently.
	As per order, this execution allows maplisting options through this predicate.
	Goal is called once for plot_on, in that scenario.
	If On's term/atom name is x11, or its == open, then the device
	is left open otherwise r_devoff/0 is called.

==
 ?- pl_plot_on( <- plot(c(1,2,3)), plot_on(x11()) ).
 ?- pl_plot_on( <- plot(c(1,2,3)), plot_on(pdf(+ex.pdf)) ).
 ?- shell( 'evince ex.pdf' ).
 ?- pl_plot_on( <- plot(c(1,2,3)), plot_on(x11(width=14)) ).
==

@author   nicos angelopoulos
@version  0.1 2015/12/16
@tbd      move this to Real ?     

*/
pl_plot_on( Goal, plot_on(Pon) ) :-
	!,
	pl_plot_on_open( Pon ),
	call( Goal ),
	pl_plot_on_close( Pon ).
pl_plot_on( _Goal, _ ).

pl_plot_on_open( open ) :- !.

pl_plot_on_open( Dev ) :-
	<- Dev.

pl_plot_on_close( open ) :- !.
pl_plot_on_close( Dev ) :-
	arity( Dev, x11, _ ),
	!.
pl_plot_on_close( _ ) :-
	r_devoff.
