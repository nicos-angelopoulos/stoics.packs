/** wgraph_neighbours( +Vertex, +Wgraph, -Neighs ).

Get all (un-directional) neighbours of Vertex in Wgraph.

?- wgraph_neighbours( b, [a-b:1,a-c:1,a-d:1,b-c:1], Neighs ).

*/
wgraph_neighbours( Vtx, Wgraph, Neighs ) :-
	wgraph_neighbours_1( Wgraph, Vtx, Neighs ).

wgraph_neighbours_1( [], _Vtx, [] ).
wgraph_neighbours_1( [H|T], Vtx, Neighs ) :-
	wgraph_edge_vertex_neighbours( H, Vtx, Neighs, TNeighs ),
	wgraph_neighbours_1( T, Vtx, TNeighs ).

wgraph_edge_vertex_neighbours( Vtx-Neigh:_W, Vtx, Neighs, TNeighs ) :-
	!,
	Neighs = [Neigh|TNeighs].
wgraph_edge_vertex_neighbours( Neigh-Vtx:_W, Vtx, Neighs, TNeighs ) :-
	!,
	Neighs = [Neigh|TNeighs].
wgraph_edge_vertex_neighbours( _, _Vtx, Neighs, Neighs ).

%% wgraph_neighbours( +Vtx, +Wgraph, +Neighs, -Weights, -Rgraph ).
%
% Very specialised version maybe it should be called something else.
% Fails if not all Neighs are neighbours of Vtx.
% Rgraph is the reduced graph after removing neighbour edges.
%
%==
% ?- G = [a-b:1,a-c:1,a-d:1,b-d:1], assert( wg(G) ).
% ?- wg(G), wgraph_neighbours( a, G, [b,c], Ws, Red ).
% G = [a-b:1, a-c:1, a-d:1, b-d:1],
% Ws = [1, 1],
% Red = [a-d:1, b-d:1].
% ?- wg(G), wgraph_neighbours( a, G, [b,c,e], Ws, Red ).
%==
%
wgraph_neighbours( Vtx, Wgraph, Neighs, Weights, Rgraph ) :-
	ground( Neighs ),
	sort( Neighs, OrdNeighs ),
	wgraph_neighbours_1( Wgraph, Vtx, OrdNeighs, Weights, Rgraph ).

wgraph_neighbours_1( [], _Vtx, [], [], [] ).
wgraph_neighbours_1( [H|T], Vtx, Neighs, Ws, Rg ) :-
	wgraph_edge_vertex_neighbours( H, Vtx, Neighs, Ns, Ws, Rg, TWs, Tg ),
	wgraph_neighbours_1( T, Vtx, Ns, TWs, Tg ).

wgraph_edge_vertex_neighbours( Vtx-Neigh:W, Vtx, Ns, NxtNs, Ws, Rg, TWs, Tg ) :-
	ord_selectchk( Neigh, Ns, NxtNs ),
	% ord_del_element( Ns, Neigh, NxtNs ),
	!,
	Ws = [W|TWs],
	Tg = Rg.
wgraph_edge_vertex_neighbours( Neigh-Vtx:W, Vtx, Ns, NxtNs, Ws, Rg, TWs, Tg ) :-
	ord_selectchk( Neigh, Ns, NxtNs ),
	!,
	Ws = [W|TWs],
	Tg = Rg.
wgraph_edge_vertex_neighbours( Edge, _Vtx, Ns, Ns, Ws, Rg, TWs, Tg ) :-
	TWs = Ws,
	Rg = [Edge|Tg].
