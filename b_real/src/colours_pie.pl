
:- use_module(library(lists)).
:- lib(real).
:- lib(options).
:- lib(stoics_lib:termplate/3).
:- lib(stoics_lib:position_nth/3).

colours_pie_defaults( Defs ) :-
    Defs = [    cex(1),
                labels(colours), main(""), 
                prefix(number), radius(1)
           ].

/** colours_pie( +Colours ).
    colours_pie( +Colours, +Opts ).

Use the Colours to draw a coloured pie chart via library(real).
Colours can be a list or a flat compound, typically,
c(c11,cl2,c3). This should be digestable by real as an
input to a named argument. Requires library(real).

Opts
  * cex(Cex=1)
    size of the labels
  * main("")            
    main label
  * prefix(Pfx=number)
    or false to exclude labels
  * radius(Radius=1)
    radius for the pie
  * weights(Whg=1)
    relative weights
  * labels(Lbs=Clrs)
    labels for the portions

==
 ?- lib(b_real:c25/1).
 ?- c25(C25), colours_pie( C25 ).

 ?- lib(b_real:colour_cb/1).
 ?- colour_cb(Cb), colours_pie( Cb ).

 ?- lib(real).
 ?- <- library("RColorBrewer").
 ?- Set1 <- brewer.pal(9,"Set1"), colours_pie( Set1, main(main) ).

 ?- colfunc <- colorRampPalette(c("white", "blue")),Ten <- colfunc(10),colours_pie(Ten).

 ?- lib(r("colorspace")).
 ?- Pal <- diverge_hcl(7), colours_pie( Pal ).
 ?- Pal <- sequential_hcl(7), colours_pie( Pal ). % gives you shades of blue
 ?- R <- rainbow_hcl( 4 ), colours_pie( R ).
 ?- colours_pie( ["#008000","#CC0000"], weights(c(1,2)) ).
 ?- colours_pie( ["#008000","#CC0000"], [weights(c(1,2)),prefix(false)] ).
 ?- colours_pie( ["#008000","#CC0000"], [weights(c(1,1)),labels([a,b])] ).
==

@author nicos angelopoulos
@version  0.2 2014/6/9           added Opts, main/1
*/

colours_pie( Colours ) :-
	colours_pie( Colours, [] ).

colours_pie( Colours, Args ) :-
	options_append( colours_pie, Args, Opts ),
	% position_term( Colours, Labels, Arity ),
    termplate( Colours, Arity, Labels ),
	options( prefix(Pfx), Opts ),
	options( labels(Lopt), Opts ),
	colours_pie_labels( Lopt, Arity, Pfx, Colours, Labels ),
	options( main(Main), Opts ),
	( memberchk(weights(Ws),Opts) -> true; Ws = rep(1,Arity) ),
    options( radius(Radius), Opts ),
    options( cex(Cex), Opts ),
	<- pie( Ws, col=Colours, labels=Labels, main=+Main, radius=Radius, cex=Cex ).

colours_pie_labels( colours, Arity, Pfx, Colours, Labels ) :-
	!,
	colours_pie_labels_colours( Arity, Pfx, Colours, Labels ).
% fixme you can still add the prefix below...
colours_pie_labels( Labels, _Arity, _Pfx, _Colours, Labels ).

colours_pie_labels_colours( 0, _Pfx, _Colours, _Labels ) :- !.
colours_pie_labels_colours( N, Pfx, Colours, Labels ) :-
	position_nth( N, Colours, NthCol ),
	colours_pie_label_prefix( Pfx, N, NthCol, NthLbl ),
	position_nth( N, Labels, NthLbl ),
	M is N -1,
	colours_pie_labels_colours( M, Pfx, Colours, Labels ).

colours_pie_label_prefix( false, _N, NthCol, NthLbl ) :-
	atom_string( NthCol, NthLbl ).

colours_pie_label_prefix( number, N, NthCol, NthLbl ) :-
	atom_string( N, Nst ),
	string_concat( Nst, ":", PfxSt ),
	atom_string( NthCol, NthColSt ),
	string_concat( PfxSt, NthColSt, NthLbl ).
