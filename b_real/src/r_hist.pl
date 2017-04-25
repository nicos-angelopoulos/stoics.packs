
:- lib(b_real). % pl_vector/3  % fixme: use lazy loading

r_hist_defaults( [as_density(false),transparent(true),transparency_colour("lightblue")] ).

/** r_hist( +VectSpec, +Opts ).

Plots a histogram of a vector.

VectSpec should be as that recognised by the 1st argument of pl_vector/3.

Displaying is via r_call/2 so Opts can influence that call.

Opts 
 * as_density(AsDense=false)
   when true it plots the density instead of the histogram

 * name(Name) 
   set Name (string on +atom) default value for xlab and main

 * transparency_colour(TransClr="lightblue")
    colour for the t(density's) transparency colour

 * transparent(Trans=true)
   use transparent colour to highlight density (currently only with AsDens=true)

==
?- lib(real).
?- Mtc <- as.matrix(mtcars), r_hist( 2, mtx(Mtc) ).
?- rns <- rnorm(1000).
?- Rns <- rns, r_hist( Rns, name("rnorm") ).
?- Rns <- rns, r_hist( Rns, [name("rnorm"),main="Main Title",outputs(svg),stem(rh1)] ).
==
Produces file: rh1.svg

[[../doc/images/rh1.svg]]

==
?- rnsm <- rnorm(10000), r_hist( rnsm, true ).
?- r_hist( rnsm, as_density(true) ).
?- pl_vector( rnsm, Rnsm, if_rvar(prolog) ), r_hist( Rnsm, name(+true) ).
?- r_hist( rnsm, [as_density(true),transparent(false)] ).
?- r_hist( rnsm, [as_density(true),transparency_colour("lightgreen"),outputs(svg),stem(rh2)] ).
==
Produces file: rh2.svg


[[../doc/images/rh2.svg]]

@author nicos angelopoulos
@version  0.1 2017/4/25

*/
r_hist( VectIn, Args ) :-
    options_append( r_hist, Args, Opts ),
    pl_vector( VectIn, Vect, Opts ),
    ( memberchk(name(Name),Opts) -> append(Opts,[xlab=Name,main=Name],RcOpts)
                                   ; RcOpts = Opts ),
    ( options(as_density(true),Opts) ->
        options( transparent(Trans), Opts ),
        r_hist_density_post( Trans, Vect, Post, Opts ),
        RCall = plot(density(Vect)),
        RcaOpts = [post_call(Post)|RcOpts]
        ;
        RCall = hist(Vect), RcaOpts = RcOpts
    ),
    r_call( RCall, RcaOpts ).

r_hist_density_post( true, Vect, Post, Opts ) :-
    options( transparency_colour(Clr), Opts ),
    Post  = ( <- polygon(density(Vect),col=adjustcolor(Clr,'alpha.f'=0.2),border=adjustcolor(Clr,alpha.f=0.8)) ).
r_hist_density_post( false, _Vect, true, _Opts ).
