
:- lib( at_con/3 ).

/** dot( +StemS, +ExtS, -Dotted ).

	Create a dotted amalgamation (atom_concat-enation) of 
	StemS and ExtS. Both can be atomic or lists. 

	Note that at_con/3 is used so '' is handled differently
	to atomic_list_concat/3.

==
?- dot( tmp, rv, Dot ).
Dot = tmp.rv.

?- dot( [tmp,rv], phase1, Dot ).
Dot = tmp.rv.phase1.

?- dot( '', x, Dot ).
Dot = x.

?- dot( x, ['',x], Dot ).
Dot = x.x.

==
@author nicos angelopoulos
@version  0.1 2015/12/16

*/

dot( StemS, ExtS, Dotted ) :-
	en_list( StemS, Stems ),
	en_list( ExtS, Exts ),
	append( Stems, Exts, Parts ),
	at_con( Parts, '.', Dotted ).
