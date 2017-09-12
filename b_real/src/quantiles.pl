
:- lib( stoics_lib:n_breaks/4 ).

/* quantiles( List, Q, Qist ).

	Replace each element in List by an element in Q producing Qist,
    with the ith element in Q replacing for the elemnts in List on the ith quantile.
	Qist contains overall length(Q) distinct elements spliting the list ot Length(Q)-1 quantiles.
    The predicate would always replace the same value in List to the same Q element. Thus the quantiles
    might be imbalanced in lengths.

    Strictly speaking quantiles are the cut points but this predicate produces the rank of the elements.
    The purpose is to discretise vector of continuous values or to crate a vector of fewer factor values
    for a discrete vector.

==
?- quantiles( [6,5,4,3,2,1], [0,1], BinV ).
BinV = [1, 1, 1, 0, 0, 0].

?- quantiles( [6,6,6,2,2,2], [0,1], BinV ).
BinV = [1, 1, 1, 0, 0, 0].

?- quantiles( [6,6,6,6,6,2], [0,1], BinV ).
BinV = [1, 1, 1, 1, 1, 0].

?- quantiles( [6,6,6,6,6,6], [0,1], BinV ).
BinV = [0, 0, 0, 0, 0, 0].

==


*/
quantiles( List, Qs, Qist ) :-
	length( Qs, Qlen ),
	n_breaks( List, Qlen, [_|Breaks], [] ),
	findall( Q, (  member(Elem,List),
	               once(  (nth1(Bth,Breaks,B),Elem =< B) ),
				nth1(Bth,Qs,Q)
				), 
					Qist ),
	length( List, LLen ),
	length( Qist, LLen ).  % fixme: throw error instead
