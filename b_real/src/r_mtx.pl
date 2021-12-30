
:- lib(options).                        % options_append/3
:- lib(debug_call).
:- lib(suggests(mtx)).                  % mtx/2

:- lib( stoics_lib:term_length/2 ).
:- lib( stoics_lib:position_nth/4 ).

r_mtx_defaults( [ rownames(integer), 
                  header(true), colnames(header), debug(false)
                     ] ).

/** r_mtx( +Rvar, ?Mtx ).
   r_mtx( +Rvar, ?Mtx, +Opts ).

Load Mtx onto Rvar and vice versa. Mtx is passed through mtx/2. It can also be an Real matrix, ie. a list of lists. <br>
Rows in mtx/2 are of the form of a list of n-ary compounds (as in csv/3).

Opts
 * rownames(Rnames=integer) 
    number indicates column position, list for given names and _integer_ for auto naming 1...n .
 * header(Hdr=true)
    whether the file incorporates a header
 * colnames(Cnames=header)
    or list for given names or _integer_ for 1...n. If Cnames
    is _header_ and Hdr is false, integer column names are used.
 * debug(Dbg=false)
    or _true_ for self debugging call. Note that _false_ turns off, not just ignore
    and that in both cases original debugging status is re-instated at end of call.

==
?- mtx_data( mtcars, Mt ), r_mtx( rv, Mt, [debug(true),rownames(1)] ).
==

@author nicos angelopoulos
@version  0.1 2014/8/20
@tbd  add generic selection predicate(s) rows + columns with/out renames for columns and rows
@tbd add reading through R's read.csv()/read.table()
@tbd the Rvar -> Mtx  mode !
*/
r_mtx( Rvar, Mtx ) :-
	r_mtx( Rvar, Mtx, [] ).

r_mtx( Rvar, MtxF, Args ) :-
	Self = r_mtx,
	debugging_status( Self, PriorDbg ),
	options_append( Self, Args, Opts ),
	memberchk( debug(Dbg), Opts ), 
	mtx_matrix_set_debug( Dbg, Self ),
	mtx( MtxF, PrvMtx ),
	mtx_matrix_col_names( PrvMtx, CnamesFull, Rows, Opts ),
	memberchk( rownames(RnOpt), Opts ),
	mtx_matrix_row_names( RnOpt, Rows, CnamesFull, Rnames, MtxRows, Cnames ),
	debug( r_mtx, 'Rownames: ~w', [Rnames] ),
	debug( Self, 'Colnames: ~w', [Cnames] ),
	debug( Self, 'Transferring to R var: ~w', [Rvar] ),
	Rvar <- MtxRows,
	rownames( Rvar ) <- Rnames,
	colnames( Rvar ) <- Cnames,
	mtx_matrix_set_debug( PriorDbg, Self ).
	% here( Mtx, Rvar ).

mtx_matrix_row_names( integer, Mtx, CnFull, Rnames, MtxOut, Cnames ) :- !,
	length( Mtx, Len ),
	numlist( 1, Len, Rnames ),
	Cnames = CnFull,
	MtxOut = Mtx.
	% mtx_matrix_header_term_typed( Mtx, NumList, Rnames ).
mtx_matrix_row_names( [H|T], Mtx, CnFull, Rnames, MtxOut, Cnames ) :-  !,
	% mtx_matrix_header_term_typed( Mtx, [H|T], Rnames ),
	Rnames = [H|T],
	Cnames = CnFull,
	MtxOut = Mtx.
mtx_matrix_row_names( I, Mtx, CnFull, Rnames, MtxOut, Cnames ) :-
	integer( I ),
	maplist( position_nth(I), Mtx, Rnames, MtxOut ),
	position_nth( I, CnFull, _IthC, Cnames ).

mtx_matrix_col_names( Mtx, Cnames, Rows, Opts ) :-
	memberchk( header(HdrB), Opts ),
	mtx_matrix_header_boolean_col_names( HdrB, Mtx, Cnames, Rows, Opts ).

mtx_matrix_header_boolean_col_names( true, Mtx, Cnames, Rows, Opts ) :-
	Mtx = [Hdr|Rows],
	memberchk( colnames(CnOpt), Opts ),
	mtx_matrix_col_opt_header_col_names( CnOpt, Hdr, Cnames ).
mtx_matrix_header_boolean_col_names( false, Mtx, Cnames, Rows, Opts ) :-
	Mtx = Rows,
	Mtx = [First|_],
	mtx_matrix_row_default_header( First, DefHdr ),
	memberchk( colnames(CnOpt), Opts ),
	mtx_matrix_col_opt_header_col_names( CnOpt, DefHdr, Cnames ).

mtx_matrix_col_opt_header_col_names( header, Hdr, Cnames ) :-
	( Hdr=[_|_] -> Cnames = Hdr; Hdr =.. [_|Cnames ] ).
	% update Real to accept row().
mtx_matrix_col_opt_header_col_names( integer, Hdr, Cnames ) :-
	mtx_matrix_row_default_header( Hdr, Cnames ).
mtx_matrix_col_opt_header_col_names( [H|T], Hdr, Cnames ) :-
    ( is_list(Hdr) -> Cnames = [H|T]; Cnames =.. [row,H|T] ).  
    % fixme: 18.01.24: untested from term_simile( Hdr, [H|T], Cnames ).

% this should be independent predicate
mtx_matrix_set_debug( true, Topic ) :-
	debug( Topic ).
mtx_matrix_set_debug( false, Topic ) :-
	nodebug( Topic ).

mtx_matrix_row_default_header( First, Default ) :-
	term_length( First, N ),
	numlist( 1, N, Default ).

mtx_matrix_header_term_typed( Mtx, Term, Typed ) :-
	Mtx = [First|_],
	to_same_type( First, Term, Typed ).
