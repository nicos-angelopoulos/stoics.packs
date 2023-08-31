% :- lib(stoics_lib:en_list/2).


gg_terms_defaults([]).

/** src/lib/gg_terms(+Gplot, +GtermS, -Hplot ).

Add a term or list of Gterms to a Gplot, producing Hplot.

The Gplot could be an atom in which case it is assumed to be an Rvar holding a gplot or
a compound in which case it is assumed to be a Prolog term. In the former case, Hplot is 
instantianted to Gplot.

==
?- lib(real).
?- lib(r(ggplot2)).

?- glp <- ggplot(Df, aes(x=x, y=y))
          + geom_segment(aes(x=x,xend=x,y=0,yend=y),color=Cstem)
          + geom_point(color=Chead,size=4,alpha=0.6). 

?- gg_panel_theme( axes, Gts ), gg_terms( glp, Gts ), <- print( summary( glp ) ).
==

@author nicos angelopoulos
@version  0.1 2023/08/31

*/

gg_terms( Gplot, GtermS, Hplot ) :-
     en_list( GtermS, Gterms ),
     ( atomic(Gplot) ->
          gg_terms_r(Gterms,Gplot),
          Gplot = Hplot
          ;
          gg_terms_pl(Gterms,Gplot,Hplot)
     ).

gg_terms_r( [], _Rv ).
gg_terms_r( [G|Gs], Rv ) :-
     Rv <- Rv + G,
     gg_terms_r( Gs, Rv ).

gg_terms_pl( [], Gplot, Hplot ) :-
     Hplot = Gplot.
gg_terms_pl( [G|Gs], Fplot, Hplot ) :-
     Gplot = Fplot + G,
     gg_terms_pl( Gs, Gplot, Hplot ).
