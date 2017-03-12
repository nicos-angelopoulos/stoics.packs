
% :- lib(suggests(mtx)).  			% does not error if not available.
:- lib(error).  			        % must_be/2.

:- lib(stoics_lib:holds/2).

mlu_mtx_header_rows_defaults( Defs ) :-
	Defs = [has_header(true),is_matrix(IsM)],
	holds( current_predicate(mtx/2), IsM ).

/** mlu_mtx_header_rows( +Data, -Hdr, -Rows, +Opts ).

Select header and rows from Data, which might possibly be a 
mtx/2 input or a list of data.

Opts 
 * has_header(HasHdr=true)
  whether Data include a Header element (first). 
  If false, Hdr is constructed as term or arity the same as the first
  element of Mtx (Data after mtx/2) and of the form false(c1,...cN),

  * is_matrix(IsM=true/false)
  whether to pass the data through mtx/2. Default depends on whether mtx/2 
  is in memory (true) or not (false)

@author nicos angelopoulos
@version  0.1 2016/11/08
@see kvold_segments/3

*/
mlu_mtx_header_rows( Data, Hdr, Rows, Args ) :-
	options_append( mlu_mtx_header_rows, Args, Opts ),
	options( is_matrix(IsM), Opts ),
	must_be( boolean, IsM ),  % fixme: pack_errors
	mlu_mtx( IsM, Data, Mtx ),
	options( has_header(HasHdr), Opts ),
	must_be( boolean, HasHdr ),
	mlu_header_data( HasHdr, Mtx, Hdr, Rows ).

mlu_header_data( true, [Hdr|Rows], Hdr, Rows ).
mlu_header_data( false, Mtx, Hdr, Mtx ) :-
	Mtx = [Row|_Rows],
	functor( Row, _, Arity ),
	findall( Cnm, (between(1,Arity,I),atomic_list_concat([c,I],'',Cnm)), Cnms ),
	Hdr =.. [false|Cnms].

mlu_mtx( true, MtxF, Mtx ) :-
	mtx( MtxF, Mtx ).
mlu_mtx( false, Mtx, Mtx ).
