%% wgraph_ugraph( +Wgraph, -Ugraph ).
%% wgraph_ugraph( -Wgraph, +Ugraph ).
%
% Convert between wgraph and ugraph representations. 
%
% Add this as a speical case of wgraph/2 ?
%
%==
% ?- wgraph_ugraph( [1-2:1,1-3:2,2-3:4], Ug ).
% Ug = [1-[2, 3], 2-[3], 3-[]].
%
% ?- wgraph_ugraph( [1-2:1,1-3:2,2-3:1,4], Ug ).
% Ug = [1-[2, 3], 2-[3], 3-[], 4-[]].
%==
%
wgraph_ugraph( Wg, Ug ) :-
	ground( Wg ),
	!,
	wgraph_to_ugraph( Wg, [], Ug ).

wgraph_to_ugraph( [], Ug, Ug ).
wgraph_to_ugraph( [Edge|Wg], Acc, Ug ) :-
	wgraph_edge_ugraph( Edge, Acc, Nxt ),
	wgraph_to_ugraph( Wg, Nxt, Ug ).

wgraph_edge_ugraph( From-To:_W, Acc, Nxt ) :-
	!,
	add_edges( Acc, [From-To], Nxt ).
wgraph_edge_ugraph( Node, Acc, Nxt ) :-
	atomic( Node ),
	!,
	add_vertices( Acc, [Node], Nxt ).
