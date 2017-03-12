
:- lib( stoics_lib:compound/3 ).

/** term_name( +Term, -Name ).

True iff Name is an atom representation of the name of Term defined as:

  * Term
    when Term is an atom

  * Atomed
    with atom_codes(Term,Codes),atom_codes(Atomed,Codes). if Term is atomic but not an atom

  * TermName
    with compound(Term,TermName,_) if Term is compound (including 0 arity) compounds of SWI-7

?- term_name( atom, Name ).
Name = atom.

?- term_name( 3, Name ).
Name = '3'.

?- term_name( abc(), Name ).
Name = abc.

?- term_name( abc(x), Name ).
Name = abc.

@author nicos angelopoulos
@version  0.1 2016/01/11

*/
term_name( Term, Name ) :-
	atomic( Term ),
	!,
	( atom(Term) -> IsAtom=true; IsAtom=false ),
	term_name_atom( IsAtom, Term, Name ).
term_name( Term, Name ) :-
	compound( Term, Name, _ ).

term_name_atom( true, Atom, Atom ).
term_name_atom( false, Atomic, Atom ) :-
	atom_codes( Atomic, Codes ),
	atom_codes( Atom, Codes ).
