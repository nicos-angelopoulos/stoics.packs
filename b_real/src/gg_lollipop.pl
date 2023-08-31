
:- use_module(library(lib)).

:- lib(real).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:kv_decompose/3).

% :- lib(r(ggplot2)).  % see gg_plot_dep_load/0.
:- lib(gg_terms/3).

gg_lollipop_defaults( Defs ) :-
               Defs = [
                         clr_point("blue"),
                         clr_stem("skyblue"),
                         debug(false),
                         df_rvar(glp_df),
                         flip(false),
                         g_terms([]),
                         gplot_rvar(glp_gp),
                         labels('Category','Quantity',''),
                         order(true),
                         rvar_rmv(true),
                         stem(gg_lollipop),
                         panel_theme(lolli)
                      ].

/** gg_lollipop( +Data, +Opts ).

Plot a lollipop chart for the Data using ggplot2.

Currently the expected format for Data is Label-Value KVPairs.

Opts
  * clr_point(CPoint="blue")
    colour for the head/point of the lollipops
  * clr_stem(CStem="skyblue")
    colour for the stem/stick of the lollipops
  * debug(Dbg=false)
    informational, progress messages
  * df_rvar(Df=glp_df)
    data frame R variable
  * flip(Flip=false)
    whether to flip x- and y-axes. When not, the predicate also turns the x-tick labels 270 degrees. 
    Use =|strict|= for not flipping and not turning, or a number (such as 90) for the actual degrees.
  * g_terms(Gterms=[])
    if given, terms in this list are appended (+) to the ggplot() call
  * labels(X='Category',Y='Quantity',M='')
    labels to use on x- and y-axes and main title
  * order(Ord=true)
    =|false|= displays according to ggplot2 
    =|reverse|= reverses it
    =|true|= keeps the order of labels in Data as is. 
    =|size|= order in descending size of the numerical values in Data, and
    =|size_reverse|= orders elements in ascending size of the numerical values in Data
  * panel_theme(Theme=lolli)
    use some basic theme adjustments making the plots a bit starker (see gg_panel_theme/2)
  * rvar_rmv(DfR=true)
    remove Df and Gp after call
  * stem(Stem=gg_lollipop)
    stem for any output files (overrides def of gg_outputs/2)

Examples
==
?- gg_lollipop([a-2,b-5,c-1,d-3], true).
==

Options are passed to gg_outputs/2.
==
?- gg_lollipop([a-2,b-5,c-1,d-3], outputs(png) ).
==
Produces file:  gg_lollipop.png

[[doc/html/images/gg_lollipop.png]]

Change basic parameters of the plot
==
?- gg_lollipop([a-2,b-5,c-1,d-3], [clr_point("red"),clr_stem("green"),panel_theme(false)] ).
?- gg_lollipop( [a-2,b-5,c-1,d-3], panel_theme(blank) ).
==

Change the labels
==
?- gg_lollipop([a-2,b-5,c-1,d-3], labels(doom,gloom,all_around)).
==

Inject arbitrary ggplot2 terms
==
?- gg_lollipop( [a-2,b-5,c-1,d-3], panel_theme(false) ).
?- gg_lollipop( [a-2,b-5,c-1,d-3], [panel_theme(false),g_terms(theme_light())] ).
==

Adjust angle of tick labels, when not flipping
==
?- gg_lollipop([a-2,b-5,c-1,d-3], flip(45) ).
==

"ordering" can be counter-intuitive. By =|Order=true|= we mean keep the order as in the given list.
By default, ggplot2 presents the categorical variable in lexicographical order.
==
?- gg_lollipop([b-5,a-2,d-3,c-1], true ).
?- gg_lollipop([b-5,a-2,d-3,c-1], order(false) ).
?- gg_lollipop([b-5,a-2,d-3,c-1], order(reverse) ).
==

The plot can also be ordered by the numerical values in the Data.
==
?- gg_lollipop([a-2,b-5,c-1,d-3], order(size) ).
?- gg_lollipop([a-2,b-5,c-1,d-3], order(size_reverse) ).
==

Can keep the R variables around for later use with
==
?- gg_lollipop([a-2,b-5,c-1,d-3], rvar_rmv(false)).
?- lib(real).
?- <- ls().

?- <- print(ls()).
[1] "glp_df" "glp_gp"
true.

?- DF <- glp_df.
DF = [x=[1, 2, 3, 4], y=[2, 5, 1, 3]].

?- <- summary(glp_gp).
data: x, y [4x2]
mapping:  x = ~x, y = ~y
...
==

@author nicos angelopoulos
@version  0.1 2023/08/30
@see gg_outputs/2
@see gg_panel_theme/2
@tbd example with different theme via g_terms()

*/

gg_lollipop( Data, Args ) :-
     Self = gg_lollipop,
     gg_plot_dep_load,
     options_append( Self, Args, Opts ),
     kv_decompose( Data, Names, Times ),
     options( df_rvar(Df), Opts ),
     Df <- data.frame( x=Names, y=Times ),
     options( order(Ord), Opts ),
     gg_lollipop_order( Ord, Names, Data, Df ),
     options( clr_point(Chead), Opts ),
     options( clr_stem(Cstem), Opts ),
     options( gplot_rvar(Gp), Opts ),
     Gp <- ggplot(Df, aes(x=x, y=y))
                   + geom_segment( aes(x=x, xend=x, y=0, yend=y), color=Cstem)
                   + geom_point( color=Chead, size=4, alpha=0.6),

     options( panel_theme(Thm), Opts ),
     gg_panel_theme( Thm, ThemeL ),
     gg_terms( Gp, ThemeL, Gp ),
     options( flip(Flip), Opts ),
     ( Flip == false -> 
               Gp <- Gp + theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
               ; 
               ( Flip == strict ->
                    true
                    ;
                    ( number(Flip) -> 
                         Gp <- Gp + theme(axis.text.x = element_text(angle = Flip, vjust = 0.5, hjust=1))
                         ;
                         Gp <- Gp + coord_flip()
                    )
               )
     ),
     options( labels(Xlb,Ylb,Mlb), Opts ),
     Gp <- Gp + labs(x=+Xlb, y=+Ylb, title=+Mlb),
     options( g_terms(GtsPrv), Opts ),
     en_list( GtsPrv, Gts ),
     gg_terms( Gp, Gts, Gp ),
     gg_outputs( Gp, Opts ),
     options_rvar_rmv( Df, Opts ),
     options_rvar_rmv( Gp, Opts ).

gg_lollipop_order( false, _Names, _Data, _Df ).
gg_lollipop_order( reverse, Names, _Data, Df ) :-
     reverse( Names, Seman ),
     Df$x <- factor( Df$x, levels=Seman ).
gg_lollipop_order( size, _Names, Data, Df ) :-
     sort( 2, @>=, Data, Ord ),
     kv_decompose( Ord, OrdNames, _ ),
     Df$x <- factor( Df$x, levels=OrdNames ).
gg_lollipop_order( size_reverse, _Names, Data, Df ) :-
     sort( 2, @=<, Data, Ord ),
     kv_decompose( Ord, OrdNames, _ ),
     Df$x <- factor( Df$x, levels=OrdNames ).
gg_lollipop_order( true, Names, _Data, Df ) :-
     Df$x <- factor( Df$x, levels=Names ).
