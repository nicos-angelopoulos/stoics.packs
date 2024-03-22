
r_hist_defaults( Defs ) :-
          Defs = [  as_density(false),
                    multi(false),
                    outputs(false),
                    transparent(true),
                    transparency_colour("lightblue")
                 ].

/** r_hist( +VectSpecS, +Opts ).

Plots a histogram or density of an arithmetic vector.

VectSpec should be as that recognised by the 1st argument of pl_vector/3.
A list is the simplest representation of a vector.

Displaying is via r_call/2, so Opts can influence that call.

Opts 
  * as_density(AsDense=false)
    when true it plots the density instead of the histogram

  * name(Name) 
    set Name (string or +atom) default value for x-axis and main labels

  * multi(Multi=false)
    is this a multi plot ? It also affects pl_vector/3. When =true= VectSpec should be a list of 
    vector specs. Currently only with =|AsDense=true|=

  * outputs(Outs=false)
    as in r_call/2, however these are treated with r_hist/2 for multi plot outputs

  * transparency_colour(TransClr="lightblue")
    colour for the density, transparency colour

  * transparent(Trans=true)
    use transparent colour to highlight density (currently only with =|AsDense=true|=)

==
?- lib(real).
?- Mtc <- as.matrix(mtcars), r_hist( 2, mtx(Mtc) ).
?- rns <- rnorm(1000).
?- Rns <- rns, r_hist( Rns, name("rnorm") ).
?- Rns <- rns, r_hist( Rns, [name("rnorm"),main="Main Title",outputs(svg),stem(rh1)] ).
==
Produces file: rh1.svg

[[doc/html/images/rh1.svg]]

==
?- lib(real).
?- rnsm <- rnorm(10000), r_hist( rnsm, true ).
?- r_hist( rnsm, as_density(true) ).
?- pl_vector( rnsm, Rnsm, if_rvar(prolog) ), r_hist( Rnsm, name(+true) ).
?- r_hist( rnsm, [as_density(true),transparent(false)] ).
?- r_hist( rnsm, [as_density(true),transparency_colour("lightgreen"),outputs(svg),stem(rh2)] ).
==
Produces file: rh2.svg

[[doc/html/images/rh2.svg]]

==
?- lib(real).
?- rnsm <- rnorm(1000), r_hist( rnsm, true ).
?- rnsm2 <- rnorm(1000,2), r_hist(rnsm2, true).
?- r_hist([rnsm2,rnsm],[multi(true),as_density(true),transparency_colour(["lightgreen","lightblue"])]).
?-   
     Fulls = [ multi(true), as_density(true), transparency_colour(["lightgreen","lightblue"]),
               xlab="x_lab",ylab="y_lab",main="main", ylim=c(0,0.3)
             ],
     r_hist([rnsm2,rnsm],Fulls).
==

@author nicos angelopoulos
@version  0.1 2017/4/25
@version  0.2 2024/3/22,  added option multi(Multi)
@see pl_vector/3

*/
r_hist( VectIn, Args ) :-
    options_append( r_hist, Args, Opts ),
    pl_vector( VectIn, Vect, Opts ),
    ( memberchk(name(Name),Opts) -> append(Opts,[xlab=Name,main=Name],NmOpts)
                                   ; NmOpts = Opts ),
    ( options(as_density(true),Opts) ->
        options( transparent(Trans), Opts ),
        options( transparency_colour(Clrs), Opts ),
        options( multi(Multi), Opts ),
        r_dense( Multi, Trans, Clrs, Vect, Rcall, Post, Opts ),
        % r_dense( Multi, Trans, Clrs, Vect, Goal, Post )
        RcOpts = [post_call(Post)|NmOpts]
        ;
        Rcall = hist(Vect),
        NmOpts = RcOpts
    ),
    r_call( Rcall, RcOpts ).

r_dense( true, Trans, ClrS, VectS, Rcall, Post, Opts ) :-
     ClrS = [Clr|Clrs],
     VectS = [Vect|Vects],
     r_dense( false, Trans, Clr, Vect, Acall, Apost, Opts ),
     r_dense_multi( Vects, Clrs, Trans, Acall, Apost, Rcall, Post ).
r_dense( false, Trans, Clr, Vect, Rcall, Post, _Opts ) :-
     r_hist_density_post( Trans, Clr, Vect, Post ),
     Rcall = plot(density(Vect)).
     % r_call( plot(density(Vect)), [post_call(Post)|Opts] ).

r_dense_multi( [], [], _Trans, Acall, Apost, Rcall, Post ) :-
     Acall = Rcall,
     Apost = Post.
r_dense_multi( [V|Vs], [Clr|Clrs], Trans, Acall, Apost, Rcall, Post ) :-
     Bcall = (Acall,lines(density(V))),
     r_hist_density_post( Trans, Clr, V, Cpost ),
     Bpost = (Apost,Cpost),
     r_dense_multi( Vs, Clrs, Trans, Bcall, Bpost, Rcall, Post ).

r_hist_density_post( true, Clr, Vect, Post ) :-
    Post  = ( <- polygon(density(Vect),col=adjustcolor(Clr,'alpha.f'=0.2),border=adjustcolor(Clr,alpha.f=0.8)) ).
r_hist_density_post( false, _Clr, _Vect, true ).
