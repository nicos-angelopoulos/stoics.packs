
:- lib( stoics_lib:compare/4 ).

:- lib( mlu_mtx_header_rows/4 ).

k_fold_segments_defaults( Defs ) :-
	Defs = [ 
	         by_permutation(true),folds(10), 
		    has_header(true), is_matrix(IsM)
	],
	holds( current_predicate(mtx/2), IsM ).

/** k_fold_segments( +Data, -Header, -Segments, +Opts ).

Split data to N segments. Header is the header of Data or a made 
up one if Data does not have a header.

==
?- Data = [r,a,b,c,d,e,f], k_fold_segments( Data, H, Sgs, folds(3) ).
Data = [r, a, b, c, d, e, f], H = r, 
Sgs = [[a, d], [b, c], [e, f]].

?- Data = [r,a,b,c,d,e,f], k_fold_segments( Data, H, Sgs, [folds(3),by_permutation(false)] ).
Data = [r, a, b, c, d, e, f], H = r,
Sgs = [[b, e], [c, d], [a, f]].

==

Opts
  * by_permutation(By=false)
  whether to create segments by a single permutation operation.
  Althernative is by sequentially chossing bins for each datum 
  until all bins are full (and Data is empty).

  * folds(K=10)
  number of segments to split the data to 
  (exhaustive and mutual exclusive splits)

@author  nicos angelopoulos
@version 0.1 2016/11/08
@see     k_fold_learn/4

*/
k_fold_segments( Data, Hdr, Segments, Args ) :-
	options_append( k_fold_segments, Args, Opts ),
	options( folds(K), Opts ),
	mlu_mtx_header_rows( Data, Hdr, Rows, Opts ),
	length( Rows, Dlen ),
	debug( mlu(k_fold_segments), 'Data rows length: ~d (excluding header if present) ', [Dlen] ),
	compare( meta, Op, Dlen, =<(K) ),
	k_fold_enough_data( Op, Dlen, K ),
	options( by_permutation(By), Opts ),
	must_be( boolean, By ),
	numlist( 0, Dlen, Idxs ),
	n_breaks( Idxs, K, [_|Breaks], [] ),
	debug( mlu(k_fold_segments), 'Data segment breaks: ~w', [Breaks] ),
	k_fold_segments_by( By, Rows, Breaks, Segments ).

k_fold_segments_by( true, Rows, Breaks, Segments ) :-
	random_permutation( Rows, Shuffled ),
	Breaks = [B|TBs],
	compare( meta, Cop, 1, >=(B) ),
	k_fold_segment_break( Cop, 1, B, TBs, Shuffled, Segm, TSegms ),
	holds( debugging(mlu(k_fold_learn)), Dbg ),
	Segments = [Segm|TSegms],
	debug_segment_lengths( Dbg, Segments ).
k_fold_segments_by( false, Rows, Breaks, Segments ) :-
	length( Breaks, NofSegms ),
	findall( SegmI+PopI, (nth1(I,Breaks,BreakI),
					  ( I =:= 1 -> PopI is BreakI; 
					    J is I - 1,
					    nth1(J,Breaks,BreakJ),
					    PopI is BreakI - BreakJ
					  ),
	                      nth1(I,Segments,SegmI)
					   ),
					   	Pairs ),
	k_fold_segments_by_bins( NofSegms, Rows, Pairs ),
	k_fold_segments_pairs( Pairs, Segments ).

k_fold_segments_pairs( [], [] ).
k_fold_segments_pairs( [Sgm+_|T], [Sgm|R] ) :-
	k_fold_segments_pairs( T, R ).

k_fold_segments_by_bins( 0, Rows, Pairs ) :- !,
	k_fold_segment_residual( Rows ),
	( Pairs = [] -> true; throw( non_empty_pairs(Pairs) ) ).
k_fold_segments_by_bins( NofSegms, [Row|Rows], Pairs ) :-
	Rnd is random_float,
	k_fold_segments_by_bins_select( Rnd, 1, NofSegms, Row, Pairs, NxtPairs, NxtNofSs ),
	k_fold_segments_by_bins( NxtNofSs, Rows, NxtPairs ).

k_fold_segments_by_bins_select( Rnd, I, NofSegms, Row, Pairs, NxtPairs, NxtNofSs ) :-
	( NofSegms =:= I -> 
		AtEnd = true
		;
		( Rnd < (I / NofSegms) ->
			AtEnd = true
			;
			AtEnd = false
		)
	),
	k_fold_segments_by_bins_select_at_end( AtEnd, Rnd, I, NofSegms, Row, Pairs, NxtPairs, NxtNofSs ).

k_fold_segments_by_bins_select_at_end( true, _Rnd, _I, NofSegms, Row, Pairs, NxtPairs, NxtNofSs ) :-
	k_fold_segments_by_bin_add_to_head( Pairs, Row, NofSegms, NxtPairs, NxtNofSs ).
k_fold_segments_by_bins_select_at_end( false, Rnd, I, NofSegms, Row, [P|Pairs], [P|NxtPairs], NxtNofSs ) :-
	J is I + 1,
	( NofSegms =:= J -> 
		AtEnd = true
		;
		( Rnd < J / NofSegms ->
			AtEnd = true
			;
			AtEnd = false
		)
	),
	k_fold_segments_by_bins_select_at_end( AtEnd, Rnd, J, NofSegms, Row, Pairs, NxtPairs, NxtNofSs ).
		
k_fold_segments_by_bin_add_to_head( [BinTail+BinR|T], Row, NofSegms, NxtPairs, NxtNofSs ) :-
	( BinR =:= 1 -> 
		BinTail = [Row],
		NxtPairs = T,
		NxtNofSs is NofSegms - 1
		;
		BinTail = [Row|BinRem],
		BinQ is BinR - 1,
		NxtPairs = [BinRem+BinQ|T],
		NxtNofSs is NofSegms
	).

k_fold_segment_break( <>, I, _OldB, Bs, Data, [], Segms ) :-
	% k_fold_break_header( IncHdr, Hdr, Segm, Hegm ),
	k_fold_segment_break_continue( Bs, I, Data, Segms ).
k_fold_segment_break( =, I, B, TBs, [D|Data], [D|Segm], Segms ) :-
	J is I + 1,
	compare( meta, Cop, J, >=(B) ),
	k_fold_segment_break( Cop, J, B, TBs, Data, Segm, Segms ).

k_fold_segment_break_continue( [], _I, Data, [] ) :-
	k_fold_segment_residual( Data ).
k_fold_segment_break_continue( [B|TBs], I, Data, [Segm|Segms] ) :-
	% J is I + 1,
	compare( meta, Cop, I, >=(B) ),
	% should  always be Cop = (=) 
	debug( mlu(k_fold_learn), 'Breaking segment at position: ~d', I ),
	k_fold_segment_break( Cop, I, B, TBs, Data, Segm, Segms ).

k_fold_segment_residual( [] ).
k_fold_segment_residual( [D|Data] ) :-
	length( [D|Data], Dlen ),
	throw( pack_error(mlu,fold_data_residual(Dlen)) ).

k_fold_enough_data( =, _Dlen, _N ).
k_fold_enough_data( <>, Dlen, N ) :-
	throw( pack_error(mlu,fold_data_insufficient(Dlen,N)) ).
