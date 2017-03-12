%% wgraph_vertices( +Wgraph, -Vertices ).
%
% Nodes is the set of all nodes in Wgraph (wgraph/1).
%==
% ?- wgraph_vertices( [a-b:3,b-c:4], Nodes ).
% Nodes = [a, b, c].
% ?- wgraph_vertices( [a-b:3,b-c:4,d], Nodes ).
% Nodes = [a, b, c, d].
%==
% @author nicos angelopoulos
% @version  0.1 2014/11/13
%
wgraph_vertices( WgraphIn, Verts ) :-
	wgraph( WgraphIn, Wgraph ),
	findall( X, (member(H,Wgraph),((H=N1-N2:_Z,member(X,[N1,N2]));(atomic(H),X=H))), XYsNest ),
	flatten( XYsNest, XYs ),
	sort( XYs, Verts ).
