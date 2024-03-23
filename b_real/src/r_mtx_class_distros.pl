
:- use_module(library(lib)).

r_mtx_class_distros_defaults( Defs ) :-
               Defs = [
                         class_main(true),
                         class_postfix(true),
                         debug(false),
                         debug_r_hist(false),
                         outputs(svg),
                         stem(''),
                         transparency_colour(["lightblue","lightgreen","indianred1"])
                      ].

/** r_mtx_class_distros(+Mtx, +Opts).

Draws a plot for each data column of Mtx- comprising of N distributions, where N are the number of
classes in first column of Mtx.

Opts
  * class_main(CidMain=true)
    should we pass Cid as main title of the plot. When false, empty string is passed,
    which can be overriden with an explicit main() option
  * class_postfix(Cfx=true)
    add column name to stem
  * debug(Dbg=true)
    informational, progress messages
  * debug_r_hist(Drh=false)
    whether to debug the call to r_hist/2
  * dir(Dir='.')
    directory for any output files (when stem is present)
  * outputs(Outs=svg)
    see r_call/2
  * stem(Stem='')
    the stem for the file outputs.
    Stem_Cid is passed to r_hist/2 if =|Cfx=true|= else Stem is passed.
    If =|Stem=''|= and =|Cfx=true|= (defaults) Cid is passed as r_hist/2 stem().
    If =|Stem=''|= and =|Cfx=false|= then =|class|= is passed.
  * transparency_colour(TranspClr=["lightblue","lightgreen","indianred1"])
    passed to r_hist, after being trimmed to number of classes

The predicate options are also passed to r_hist/2 (with debug_r_hist() renamed to debug()).

Examples
==
?- use_module(library(mtx)),
   mtx_data( iris, Iris )
   r_mtx_class_distros(Iris,[]).

?- ls().
Petal.Length.svg  Petal.Width.svg  plots/  Sepal.Length.svg  Sepal.Width.svg
==

As above, but cuts Y max values to 2 and minimum of 0.

==
?- use_module(library(mtx)),
   mtx_data( iris, Iris ),
   r_mtx_class_distros(Iris,[dir(plots),ylim=c(0,2)]).
==

@author nicos angelopoulos
@version  0.1 2024/03/23
@see mtx/2,3
@see r_hist/2
@tbd fixme: allow for headerless matrices and different class column.

*/
r_mtx_class_distros( CsvF, Args ) :-
     Self = r_mtx_class_distros,
     options_append( Self, Args, Opts ),
     mtx_lists( CsvF, MtxL ),
     MtxL = [[_ClassCid|ClassClm]|DataL],
     sort( ClassClm, Classes ),
     findall( Class-[], member(Class,Classes), Acc ),
     r_mtx_class_distro_columns( DataL, Self, ClassClm, Classes, Acc, Opts ),
     debuc( Self, end, true ).

r_mtx_class_distro_columns( [], _Self, _ClassClm, _Classes, _Empty, _Opts ).
r_mtx_class_distro_columns( [[Cid|Clm]|Clms], Self, ClassClm, Classes, Empty, Opts ) :-
     debug( Self, 'Doing: ~w', [Cid] ),
     r_mtx_class_distro_column_classed( Clm, ClassClm, Empty, CVs ),
     kv_decompose( CVs, Cs, Vs ),
     ( Cs == Classes -> true; throw(mis_classes_in_cvs(Cs,Classes),b_real:Self/2) ),
     options_rename( Opts, [debug-debug_r_mtx_class_distro_columns,debug_r_hist-debug], RnmOpts, replace(false) ),
     r_mtx_class_distro_columns_transparency_colours( Self, Classes, Tlrs, Opts ),
     ( options(class_main(true),Opts) -> Main = Cid; Main = '' ),
     options( stem(Stem), Opts ),
     ( options(class_postfix(true),Opts) -> at_con([Stem,Cid],'_',RhStem); (Stem=='' -> RhStem = class; Stem=RhStem) ),
     DisOpts =  [multi(true),as_density(true),transparency_colour(Tlrs),main= +Main],
     append( [stem(RhStem)|RnmOpts], DisOpts, RhiOpts ),
     r_hist( Vs, RhiOpts ),
     r_mtx_class_distro_columns( Clms, Self, ClassClm, Classes,  Empty, Opts ).
     
r_mtx_class_distro_column_classed( [], [], AccCVs, CVs) :-
     keysort(  AccCVs, CVs ).
r_mtx_class_distro_column_classed( [Val|Vals], [C|Cs], AccCVs, CVs ) :-
     once( select( C-Vs, AccCVs, RemCVs ) ),
     r_mtx_class_distro_column_classed( Vals, Cs, [C-[Val|Vs]|RemCVs], CVs ).

r_mtx_class_distro_columns_transparency_colours( Self, Classes, Tlrs, Opts ) :-
    options( transparency_colour(ClrS), Opts ),
    en_list( ClrS, Clrs ),
    length( Clrs, ClrsLen ),
    length( Classes, ClassesLen ),
    ( ClrsLen < ClassesLen -> throw(too_few_class_colors(ClrsLen,ClassesLen),b_real:Self/2)
                           ;  findall( Tlr, (nth1(N,Classes,_),nth1(N,Clrs,Tlr)), Tlrs )
    ).
