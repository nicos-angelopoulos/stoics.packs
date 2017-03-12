
:- lib(stoics_lib:en_list/2).

/** wgraph_add_edges( +Graph, +Edges, -NewGraph ).

Add edges to a graph. NewGraph is sorted.

==
?- wgraph_add_edges( [], a-b:1, One ), wgraph_add_edges( One, b-c:2, Two ), wgraph_add_edges( Three, a-c:3, Three ).
==

@author nicos angelopoulos
@version  0.1 2015/3/30

*/
wgraph_add_edges( G, ES, New ) :-
	en_list( ES, Es ),
	append( Es, G, Bag ),
	sort( Bag, New ).
