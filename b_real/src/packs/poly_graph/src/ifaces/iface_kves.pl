
:- lib( graph_direction_neighbour_kves/4 ).
:- lib( graph_nodes_sub_kves/4 ).

k_graph_neighbours( 0, _Verts, Seen, _Type, _Edges, _Dct, Seen ) :- !.
k_graph_neighbours( K, Verts, Seen, _Type, Edges, Dct, Red ) :-
	findall( X, ( member(V,Verts),
				    % edges_graph_direction_vertex_neighbour( Dct, V, Edges, X ),
			        graph_direction_neighbour_kves( Dct, V, Edges, X ),
	                \+ memberchk(X,Seen)
                ),
                    Nvs ),
	J is K - 1,
	list_to_ord_set( Nvs, NvsOrd ),
	ord_union( NvsOrd, Seen,  NxtSeen ),
	k_graph_neighbours( J, Nvs, NxtSeen, _TheType, Edges, Dct, Red ).

/** graph_nodes_sub( +Graph, +Nodes, -Sub ).

    Reduce Graph to that part that contains Nodes.

==
?- graph_nodes_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1,2,3] , Sub ).
Sub = [1-2, 2-3].

?- graph_nodes_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1,2,5] , Sub ).
Sub = [1-2, 1-5].
==

@author nicos angelopoulos
@version  0.1 2018/1/9

*/
graph_nodes_sub( Graph, Nodes, Sub ) :-
    graph_nodes_sub_kves( Graph, Nodes, Sub ).

graph_k_sub( Graph, Nodes, K, Drc, Sub ) :-
    integer( K ), 
    K > -1,
    graph_k_neighbours( Graph, Nodes, K, Drc, RedVerts ),
	graph_nodes_sub_kves( Graph, RedVerts, Sub ).
