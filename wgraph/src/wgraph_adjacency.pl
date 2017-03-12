%% wgraph_adjacency( +Wgraph, +Nodes, +Rmtx ).
%% wgraph_adjacency( +Wgraph, +Rmtx ).
%
% Create and occupy R matrix Rmtx as the weighed adjacency
% matrix of Wgraph (wgraph/1).
% When present Nodes is used to define the shape and location of nodes in Rmtx.
% Else the set of N1 and N2s in Wgraph is used (see wgraph_vertices/2).
% 
%==
% ?- wgraph_adjacency( [a-b:2,b-c:3,c-a:1], wga ).
% ?- <- wga.
%      [,1] [,2] [,3]
% [1,]    0    2    0
% [2,]    0    0    3
% [3,]    1    0    0
% true.
%==
% 
% @author nicos angelopoulos
% @version  0.1 2014/11/13
%
wgraph_adjacency( Wgraph, Rmtx ) :-
	wgraph_vertices( Wgraph, Nodes ),
	wgraph_adjacency( Wgraph, Nodes, Rmtx ).

wgraph_adjacency( Wgraph, Nodes, Rmtx ) :-
	length( Nodes, Len ),
	Rmtx <- matrix( nrow=Len, ncol=Len, 0 ),
	wgraph_adjacency_mtx( Wgraph, Nodes, Rmtx ).

wgraph_adjacency_mtx( [], _, _ ).
wgraph_adjacency_mtx( [H|T], Nodes, Rmtx ):-
	wgraph_adjacency_elem( H, Nodes, Rmtx ),
	wgraph_adjacency_mtx( T, Nodes, Rmtx ).

wgraph_adjacency_elem( V1-V2:W, Nodes, Rmtx ) :-
	nth1( N1, Nodes, V1 ),
	nth1( N2, Nodes, V2 ),
	Rmtx[N1,N2] <- W, % fixme: real does nt seem to like alist[[1]] = Rmtx
	!.
wgraph_adjacency_elem( Node, _Nodes, _Rmtx ) :-
	atomic( Node ). % do nothing there should all be 0
