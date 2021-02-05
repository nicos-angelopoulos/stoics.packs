
:- lib(stoics_lib:en_list/2).
:- lib(wgraph_vertex_in_item/2).

/** wgraph_del_vertices( +Graph, +Vertices, -NewGraph ).

Remove all Vertices (list) and all edges referring to these Vertices from Graph to produce NewGraph.

The predicate follows name and argument conventions from ugraph:del_vertices/3.
However, Vertices can be a single Vertex here (non-list).

==
?- 
     wgraph_del_vertices( [a,b-c:1,b-d:2], d, G1 ).
G1 = [a, b-c:1].


?- 
     wgraph_del_vertices( [a,b-c:1,b-d:2], [c,d], G2 ).
G2 = [a].
==

@author nicos angelopoulos
@version  0:1 2021/02/05
*/
wgraph_del_vertices( GraphIn, VertsIn, NewG ) :-
     en_list( VertsIn, Verts ),
     wgraph( GraphIn, Graph ),
     wgraph_del_vertices_1( Verts, Graph, GraphOut ),
     wgraph( GraphOut, NewG ).

wgraph_del_vertices_1( [], Graph, Gout ) :-
     Graph = Gout.
wgraph_del_vertices_1( [V|Vs], Graph, Gout ) :-
     exclude( wgraph_vertex_in_item(V), Graph, Next ),
     wgraph_del_vertices_1( Vs, Next, Gout ).
