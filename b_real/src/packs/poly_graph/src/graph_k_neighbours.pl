
% :- lib( expects(graph_direction_neighbour/4) ). % was expects/1

%% graph_k_neighbours( +Graph, +Verts, +K, -RedVerts ).
%% graph_k_neighbours( +Graph, +Verts, +K, +Drc, -RedVerts ).
%
% For a vertex or set of vertices, Verts, get all (sorted) vertices RedVerts, that connect
% from Verts in Graph in K steps. Graph is either a KV or module graph.
% This implementation assumes undirected graph (Drc==false, X-Y and Y-X both make
% X and Y into neighbours) except if Drc is _true_ in which case X-Y is interpreted
% as Y is a neighbour of X, but not the other way around, or if Drc is 
% _reverse_ where X-Y sets X as a neighbour of Y.
%
%==
% ?- graph_k_neighbours( [1-2,1-5,2-3,3-4,5-6,6-7], 1, 2, Which ).
% Which = [1, 2, 3, 5, 6].
%
% ?- graph_k_neighbours( [1-2,1-5,2-3,3-4,4-5,5-6,6-7], 1, 2, Which ).
% Which = [1, 2, 3, 4, 5, 6].
%  
% ?- graph_k_neighbours( [1-2,1-5,2-3,3-4,5-6,6-7], 2, 2, true, Which ).
% Which = [2, 3, 4].
% 
% ?- graph_k_neighbours( [1-2,1-5,2-3,3-4,5-6,6-7], 2, 2, false, Which ).
% Which = [1, 2, 3, 4, 5].
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/5/22
%
graph_k_neighbours( Graph, InVertS, K, Reds ) :-
	graph_k_neighbours( Graph, InVertS, K, false, Reds ).

graph_k_neighbours( Graph, InVertS, K, Dct, Reds ) :-
	integer( K ), 
	K > -1,
	en_list( InVertS, InVerts ),
	list_to_ord_set( InVerts, Verts ),
    poly_graph( Graph, Gty ),
	k_graph_neighbours( K, Verts, Verts, Gty, Graph, Dct, Reds ).
