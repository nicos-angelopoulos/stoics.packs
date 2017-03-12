/** onoma( Atom, Onoma ).

	As it is my name day today, this is a Greekified version
	of upcase_first/1.

	@author nicos angelopoulos
	@version  0.2 2015/12/6 St Nicholas day (Greek calendar)

*/
onoma( Atom, Onoma ) :-
	sub_atom( Atom, 0, 1, N, First ),
	upcase_atom( First, Capital ),
	sub_atom( Atom, 1, N, 0, Rest ),
	atom_concat( Capital, Rest, Onoma ).
