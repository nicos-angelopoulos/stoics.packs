
:- lib(stoics_lib:en_list/2).

/** wgraph_add_vertices( +Graph, +Verts, -NewGraph ).

==
?- wgraph_add_vertices( [1-3:1], [2,3], New ).
New = [2, 1-3:1].

==
*/
wgraph_add_vertices( G, VS, New ) :-
	en_list( VS, Vs ),
	wgraph_add_vertices_1( Vs, G, New ).

wgraph_add_vertices_1( [], New, New ).
wgraph_add_vertices_1( [V|Vs], G, New ) :-
	atomic( V ),
	wgraph_has_vertex( G, V ),
	!,
	wgraph_add_vertices_1( Vs, G, New ).
wgraph_add_vertices_1( [V|Vs], G, New ) :-
	atomic( V ),
	ord_add_element( G, V, G1 ),
	wgraph_add_vertices_1( Vs, G1, New ).

wgraph_has_vertex( [E|_Es], V ) :-
	wgraph_edge_has_vertex( E, V ),
	!.
wgraph_has_vertex( [_E|Es], V ) :-
	wgraph_has_vertex( Es, V ).

wgraph_edge_has_vertex( V, V ).
wgraph_edge_has_vertex( V-_:_, V ).
wgraph_edge_has_vertex( _-V:_, V ).
