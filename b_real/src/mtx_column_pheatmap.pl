
:- lib(real).                       % r_call/2, r_library/1
:- lib(options).
:- lib(suggests(mtx)).
% :- use_module( library(real) ).  % r_call/2, r_library/1

% :- r_library(pheatmap).
:- lib(promise(r(pheatmap),pheatmap)).

:- lib(heatmap_breaks/3).

:- lib(stoics_lib:select_all/4).
% :- requires( options_append/3 ).  % in planned pack

mtx_column_pheatmap_defaults( Args, Opts ) :-
	( memberchk(privates(true),Args) -> 
	       Tail = [legend_horiz='TRUE',legend_hf=0.4,legend_wf=0.7],
		  Legend='TRUE'
		  ;
		  Tail = [],
		  Legend='FALSE'
	),
	Opts = [
	               mtx(_), column(_), cname(_), cposition(_),
				centre(false), breakpoints(50),
				rvar(mcp), rvar_rmv(true),
				names([]), sort(true),
				outputs(x11),
				cluster_rows='FALSE', cluster_cols='FALSE',
				scale="none", 
				cellheight=8, cellwidth=8,
				fontsize_col=4, fontsize_row=4, fontsize=6,
				legend=Legend| Tail
	].

/** mtx_column_pheatmap( +Mtx, +Cid, +Opts ).

Plot pheatmap of column Cid (mtx_column/4) of matrix Mtx (mtx/2).

This is mainly constructed as a demonstration of using R's pheatmap function
from the omonymous library (http://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf).

Opts
  * Ropt=Rarg        
    pass Ropt=Rarg to the underlying  (see below for defaults)
  * breakpoints(Bkps=51)
    number of breakpoints if Centre is a number (should be colours + 1)
  * centre(Centre=false) 
    when number is given, center breakpoints around this value
  * cname(Cnm)
    returns the column name of Cid
  * column(Clm)
    returns the values of Cid in Mtx (does not include Cnm)
  * cposition(Cps)
    returns the column position of Cid
  * mtx(Mtx)
    returns the cannonical representation of Mtx
  * names(Names=[])
    defines heatmap's columns names, either a list or a column identifier in Mtx
  * outputs(Outs=x11)
    a single or list of [x11,pdf], see r_call/2
  * privates(Prv=false)
    if true, include private Ropts
  * rvar(Rvar=mcp)
    R variable to use for the data
  * rvar_rmv(Rdel=true)
    delete the Rvariable at end of call   fixme: not implemented yet...
  * stem(Stem)
    stem for output files. Default: atomic_list_concat([Cnm,phmap],'_',Stem).

Ropt
  * breaks=
    allows specific colour index assignments
  * cellheight=8
  * cellwidth=8
  * cluster_rows='FALSE'
    as this is for single row
  * cluster_cols='FALSE'
    ditto
  * fontsize_col=4
  * fontsize_row=4
  * fontsize=6
  * legend=Legend
    'TRUE' if Prv=true and 'FALSE' otherwise
  * legend_horiz='TRUE'
    private (only include if privates(true)
  * legend_hf=0.4
    private
  * legend_wf=0.7
    private
  * scale="none" 

==
?- use_module( library(real) ).
?- <- write.csv( mtcars, "mtcars.csv" ).  % mtcars is an example dataset in R
?- csv_read_file( 'mtcars.csv', Mt ), mtx_column_pheatmap( Mt, 2, [] ).
?- csv_read_file( 'mtcars.csv', Mt ), mtx_column_pheatmap( Mt, 2, [names(1)] ).
?- csv_read_file( 'mtcars.csv', Mt ), mtx_column_pheatmap( Mt, 2, [names(1),legend='TRUE'] ).
?- csv_read_file( 'mtcars.csv', Mt ), mtx_column_pheatmap( Mt, 2, [names(1),outputs(pdf),stem(mtcars_c2)] ).
?- shell( 'ls -l mtcars_c2.pdf' ).
-rw------- 1 user user 4212 Jan 13 13:56 mtcars_c2.pdf
==
@author nicos angelopoulos
@version  0.1 2014/1/7
@see http://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
@see mtx_column/3
@see mtx/2
@tbd remove NAs, with reprecations to names()
*/
mtx_column_pheatmap( MtxF, Cid, ArgS ) :-
	% options_append( mtx_column_pheatmap, Args, Opts ),
	% fixme: use pack(options) to replace next 3 lines
	( is_list(ArgS) -> Args=ArgS; Args = [ArgS] ),
	mtx_column_pheatmap_defaults( Args, Defs ),
	append( Args, Defs, Opts ),

	mtx( MtxF, Mtx ),
	mtx_column( Mtx, Cid, Clm, Cpos, Cnm ),
	memberchk( mtx(Mtx), Opts ),
	memberchk( column(Clm), Opts ),
	memberchk( cname(Cnm), Opts ),
	memberchk( cposition(Cpos), Opts ),

	select_all( Opts, rvar(Rvar), [Hrv|_], RemOpts ),
	Hrv = rvar(Rvar),
	ground( Rvar ),
	memberchk( sort(Sort), Opts ),
	% Rvar <- [Clm],
	% mtx_column_pheatmap_vector_order( Sort, Rvar ),
	mtx_column_pheatmap_assign_col_names( Opts, Mtx, CNames ),
	mtx_column_pheatmap_vector_order( Sort, Clm, CNames, Ord, ONames ),
	Rvar <- [Ord],
	colnames(Rvar) <- ONames,
	heatmap_breaks( Ord, BksTerm, Opts ),
     lib_r_promised( pheatmap ),
	r_call( pheatmap(Rvar), [BksTerm|RemOpts] ).

mtx_column_pheatmap_assign_col_names( Opts, Mtx, CNames ) :-
	memberchk( names(NmsOpt), Opts ),
	ground( NmsOpt ),
	mtx_column_pheatmap_assign_col_names_opt( NmsOpt, Mtx, CNames ).

mtx_column_pheatmap_vector_order( false, Clm, CNames, Clm, CNames ).
mtx_column_pheatmap_vector_order( true, Clm, CNames, Ord, ONames ) :-
	findall( Val-OName, (nth1(Num,Clm,Val),(nth1(Num,CNames,OName)->true;OName="")), Pairs ),
	sort( Pairs, OrdPairs ),
	findall( Key, member(Key-_,OrdPairs), Ord ),
	findall( Ona, member(_-Ona,OrdPairs), ONames ).

mtx_column_pheatmap_assign_col_names_opt( [], _Mtx, [] ) :-
	!. % do nothing
mtx_column_pheatmap_assign_col_names_opt( [H|T], _Mtx, [H|T] ) :-
	% colnames(Rvar) <- [H|T],
	!.
mtx_column_pheatmap_assign_col_names_opt( Cid, Mtx, Clm ) :-
	mtx_column( Mtx, Cid, Clm ).
	% colnames(Rvar) <- Clm.
