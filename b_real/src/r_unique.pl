/** r_unique( +Stem, -Unique ).

Create a Unique R variable of the form Stem.N where N is an integer.
The value 'NA' is passed to the variable.

==
?- r_unique( df.data, Uniq ).
Uniq = df.data.1.

?- r_unique( df.data, Uniq ).
Uniq = df.data.2.
==

*/
r_unique( Stem, Unique ) :-
	r_unique( 1, Stem, Unique ).

r_unique( I, Stem, Unique ) :-
	atomic_list_concat( [Stem,I], '.', Unique ),
	\+ r_is_var( Unique ),
	Unique <- 'NA',
	!.
r_unique( I, Stem, Unique ) :-
	J is I + 1,
	r_unique( J, Stem, Unique ).
