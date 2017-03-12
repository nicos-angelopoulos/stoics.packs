
% :- lib(options).  % options_append/3, options/2.
% :- use_module( library(mtx) ).  % mtx_column_select/3.

:- use_module( library(real) ).  % <-/2, r_call/2.
:- r_library( pheatmap ).

mtx_pheatmap_defaults( Defs ) :- 
	Defs = [mtx(_),rvar(mtx_hmap), names([]) ].

%% mtx_pheatmap( +Mtx, +OptS ).
%
% Plots the data in Mtx using pheatmap() from omonymous package.
% See mtx_column_pheatmap/3. Here we print vertically though.
%
% Opts
%  * Ropt=Rarg
%    pass Ropt=Rarg to the pheatmap() R call
%  * rvar(Rvar=mtx_heatmap)
%    R variable to use
%  * names(Names=[])
%    list of names or column id
%
% Predicate uses r_call/2 which takes its own options.
%
% Dependencies
%==
% ?- pack_install( real ).
% ?- use_module( library(real) ).
% ?- install.packages( "pheatmap" ).
%== 
%
% Examples
%==
% ?- use_module( library(real) ).
% ?- <- write.csv( mtcars, "mtcars.csv" ).  % mtcars is an example dataset in R
% ?- <- csv_read_file( Mt, 'mtcars.csv' ), assert( mt(Mt) ).
% ?- mt(Mt), mtx_pheatmap( Mt, [names(1),scale="column"] ).
% ?- mt(Mt), mtx_pheatmap( Mt, [names(1),scale="column",debug(true)] ).
%==
% @author nicos angelopoulos
% @version  0.1 2015/1/13
% @see http://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
% @tbd centre around value (balanced and unbalanced // interval)
%
mtx_pheatmap( Mtx, Args ) :-
	options_append( mtx_pheatmap, Args, Opts ),
	options( rvar(Rvar), Opts ),
	options( names(NmsOpt), Opts ),
	mtx_pheatmap_names_column( NmsOpt, Mtx, Names, [Hdr|DataMtx] ),
	Rvar <- DataMtx,
	Hdr =.. [_|Cnames],
	colnames(Rvar) <- Cnames,
	( Names == [] -> true; (rownames(Rvar) <- Names) ),
	r_call( pheatmap(Rvar), Opts ).

mtx_pheatmap_names_column( List, Mtx, Names, DataMtx ) :-
	is_list( List ),
	!,
	DataMtx = Mtx,
	Names = List.
mtx_pheatmap_names_column( Cid, Mtx, Names, DataMtx ) :-
	mtx_column_select( Mtx, Cid, DataMtx, Sel ),
	Sel = [_|Names].
