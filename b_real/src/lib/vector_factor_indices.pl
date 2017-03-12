
:- lib( options ).
:- lib( pl_vector/3 ).

vector_factor_indices_defaults( sort(true) ).

/** vector_factor_indices( +Vector, -Indices, Opts ).

Replace each element in a Vector by an index to its factor.

Opts
 * sort(Sort=true)
   to sort the factors before mapping them to the list (else list_to_set/2 is used).
 * vals(Vals)
   can be used to return the unique vals of Vector as used in Indices

==
?- vector_factor_indices( [d,a,b,c,a,b,c], Is, [] ).
Is = [4, 1, 2, 3, 1, 2, 3].

?- vector_factor_indices( [d,a,b,c,a,b,c], Is, [sort(false)] ).
Is = [1, 2, 3, 4, 2, 3, 4].
==

@author nicos angelopoulos
@version  0.1 2015/11/25
@see pl_vector/3
*/

vector_factor_indices( Vector, Indices, Args ) :-
	options_append( vector_factor_indices, Args, Opts ),
	pl_vector( Vector, List, Opts ),
	options( sort(Sort), Opts ),
	vector_factor_indices_values( Sort, List, Vals ),
	findall( I, (member(Elem,List),nth1(I,Vals,Elem)), Indices ),
	( memberchk(vals(Vals),Opts) -> true; true ).

vector_factor_indices_values( true, List, Vals ) :-
	sort( List, Vals ).
vector_factor_indices_values( false, List, Vals ) :-
	list_to_set( List, Vals ).
