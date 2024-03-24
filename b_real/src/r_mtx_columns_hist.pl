
:- use_module(library(lib)).

r_mtx_columns_hist_defaults( Defs ) :-
               Defs = [
                         cid_lbl(both),
                         cid_postfix(true),
                         debug(true),
                         debug_r_hist(false),
                         outputs(svg),
                         stem(''),
                         transparency_colour(["lightblue","lightgreen","indianred1"])
                      ].

/** r_mtx_columns_hist(+Mtx, +Opts).

Draws a histogram for each data column of Mtx.

The predicate uses r_hist/2 and with default values it will create .svg versions of the plots in 
sub-directory plots.

Opts
  * cid_lbl(Lbl=both)
    should we pass Cid as main title and or the x-axis label of the plot?
    These can be overriden with explicit main=Main xlab=Xlab arguments.
    (others: true, main, xlab, false)
  * cid_postfix(Cfx=true)
    add column name to stem
  * debug(Dbg=true)
    informational, progress messages
  * debug_r_hist(Drh=false)
    whether to debug the call to r_hist/2
  * dir(Dir)
    directory for any output files (default is the relative stem of Mtx, if Mtx is atomic and '.' otherwise)
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
   r_mtx_columns_hist(Iris,[]).

?- ls(plots).
Petal.Length.svg  Petal.Width.svg  plots/  Sepal.Length.svg  Sepal.Width.svg
==

As above, but cuts Y max values to 2 and minimum of 0.

==
?- use_module(library(mtx)),
   mtx_data( iris, Iris ),
   r_mtx_class_distros(Iris,[dir(plots),ylim=c(0,2)]).
==

@author nicos angelopoulos
@version  0.1 2024/03/24
@see mtx/2,3
@see r_hist/2
@tbd fixme: allow for headerless matrices and different class column.

*/
r_mtx_columns_hist( CsvF, Args ) :-
     Self = r_mtx_columns_hist,
     options_append( Self, Args, Opts ),
     mtx_lists( CsvF, MtxL ),
     options( cid_postfix(Cfx), Opts ),
     options( cid_lbl(Lbl), Opts ),
     % fixme: use os_name
     ( catch(os_cast(CsvF,AtmF),_,fail) -> os_ext(_Ext,AutoDir,AtmF),
                                           os_make_path(AutoDir)
                                        ;  AutoDir = '.' 
     ),
     append( Opts, [dir(AutoDir)], Dpts ),
     options( dir(Dir), Dpts ),
     os_make_path( Dir ),
     options( stem(Stem), Opts ),
     Prs = [debug-debug_r_mtx_class_distro_columns,debug_r_hist-debug],
     options_rename( Dpts, Prs, Rpts, replace(false) ),
     r_mtx_columns_hist_lists( MtxL, Self, Dir, Cfx, Lbl, Stem, Rpts ).

r_mtx_columns_hist_lists( [], _Self, _Dir, _Cfx, _Lbl, _Otem, _Opts ).
r_mtx_columns_hist_lists( [[Cnm|Vals]|Clms], Self, Dir, Cfx, Lbl, Otem, Opts ) :-
     r_mtx_columns_hist_stem( Cfx, Otem, Cnm, Stem ),
     r_mtx_columns_hist_labels( Lbl, Cnm, Main, Xlab ),
     append( [stem(Stem)|Opts], [main= +Main, xlab= +Xlab], Rpts ),
     r_hist( Vals, Rpts ),
     r_mtx_columns_hist_lists( Clms, Self, Dir, Cfx, Lbl, Otem, Opts ).

r_mtx_columns_hist_labels( both, Cnm, Cnm, Cnm ).
r_mtx_columns_hist_labels( true, Cnm, Cnm, Cnm ).
r_mtx_columns_hist_labels( main, Cnm, Cnm,  '' ).
r_mtx_columns_hist_labels( xlab, Cnm,  '', Cnm ).
r_mtx_columns_hist_labels( false,_Cnm,  '', '' ).

r_mtx_columns_hist_stem( true, Otem, Cnm, Stem ) :-
     at_con( [Otem,Cnm], '_',  Stem ).
r_mtx_columns_hist_stem( false, Stem, _Cnm, Stem ).
