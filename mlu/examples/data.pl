
:- lib( mlu ).

/** mlu_data.

A couple of example on segmenting datasets.

*/

data :-
	Data = [r,a,b,c,d,e,f],
	k_fold_segments( Data, H, Sgs, folds(3) ),
	write( h(H) ), nl,
	write( segments(Sgs) ), nl.
