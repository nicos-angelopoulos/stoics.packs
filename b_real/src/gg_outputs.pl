
:- use_module(library(apply)).  % maplist/3.
:- use_module(library(lists)).  % memberchk/2.
:- use_module(library(lib)).

:- lib(real).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:compound/3).

:- lib(r(ggplot2)).

gg_outputs_defaults( Defs ) :-
          Defs =  [
                       debug(false),
                       outputs(x11),
                       plot_width(7),
                       plot_height(7),
                       stem(gg_output)
                  ].

/** gg_outputs(+Opts).

Display ggplots to a variety of formats via ggsave().

In addition to be called directly by users, this predicate is also used in 
a number of Prolog library predicates that interface to ggplot2 plots.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * outputs(Outs=x11)
    atoms which are taken to be either x11 (screen output), or filename extensions that dictate type of output.
    Terms can be given with functor as described in last sentence, and can include = pairs
    which are passed to the ggsave() goal
  * plot_height(Height=7)
    height for plots 
  * plot_width(Width=7)
    width for plots 
  * stem(Stem=gg_output)
    file name stem for any output to files

Examples
==
?- ddf <- 'data.frame'(x='LETTERS'[1:26], y=abs(rnorm(26)) ).
?- ggp <-  ggplot(ddf, aes(x=x, y=y)) + geom_segment(aes(x=x,xend=x,y=0,yend=y)) + geom_point(size=4,alpha=0.6). 
?- gg_outputs(ggp,[]).
==

==
?- ddf <- 'data.frame'(x='LETTERS'[1:26], y=abs(rnorm(26)) ).
?- ggp <-  ggplot(ddf, aes(x=x, y=y)) + geom_segment(aes(x=x,xend=x,y=0,yend=y)) + geom_point(size=4,alpha=0.6). 
?- gg_outputs( ggp, outputs(png(file="abc.png")) ).
==
Produces file: abc.png

[[doc/html/images/abc.png]]

Width, Height and Stem can be overridden  by *=* options within Outs.
In the example below, the pdf gets Width 8, from the =|plot_width|= parameter, while the png gets Width 9.
==
?- ddf <- 'data.frame'(x='LETTERS'[1:26], y=abs(rnorm(26)) ).
?- ggp <-  ggplot(ddf, aes(x=x, y=y)) + geom_segment(aes(x=x,xend=x,y=0,yend=y)) + geom_point(size=4,alpha=0.6). 
?- gg_outputs( ggp, [plot_width(8),outputs([pdf,png(file="abc.png",width=9)]),debug(true)] ).
% Sending to Real: ggsave(plot=ggp,width=8,height=7,file= +gg_output.pdf)
% Sending to Real: ggsave(plot=ggp,height=7,file=abc.png,width=9)
==


@author nicos angelopoulos
@version  0.1 2023/08/31

*/

gg_outputs( Ggp, Args ) :-
     Self = gg_outputs,
     options_append( Self, Args, Opts ),
     options( plot_width(Width), Opts ),
     options( plot_height(Height), Opts ),
     options( stem(Stem), Opts ),
     options( outputs(OutsProv), Opts ),
     en_list( OutsProv, Outs ),
     maplist( gg_outputs_on(Ggp,Self,Width,Height,Stem), Outs ).

gg_outputs_on( Ggp, Self, Width, Height, Stem, Out ) :-
     compound( Out, Func, Args ),
     ( Func == x11 -> Defs = [height-Height,width-Width]
                    ; at_con( [Stem,Func], '.', Os ),
                      Defs = [file-(+Os),height-Height,width-Width]
     ),
     gg_outputs_args( Defs, Args, Nrgs ),
     ( Func == x11 -> 
          ( Args == [] ->
               debug( Self, 'Sending to Real: ~w', [print(Ggp)] ),
               <- print( Ggp )
               ;
               compound( X11, x11, Nrgs ),
               debug( Self, 'Sending to Real: ~w', [X11] ),
               <- X11,
               debug( Self, 'Sending to Real: ~w', [print(Ggp)] ),
               <- print(Ggp)
          )
          ;
          compound( GgSave, ggsave, [plot=Ggp|Nrgs] ),
          debug( Self, 'Sending to Real: ~w', [GgSave] ),
          <- GgSave
    ).

gg_outputs_args( [], Args, Nrgs ) :-
     Args = Nrgs.
gg_outputs_args( [Name-Value|T], Args, Nrgs ) :-
     ( memberchk(Name=_This,Args) -> 
          Args = Xrgs
          ;
          Xrgs = [Name=Value|Args]
     ),
     gg_outputs_args( T, Xrgs, Nrgs ).