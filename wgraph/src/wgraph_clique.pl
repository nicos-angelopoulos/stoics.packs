/** wgraph_clique( +Nodes, +Wgraph ).
    wgraph_clique( +Nodes, +Wgraph, -Weights, -Rgraph ).

	True iff Nodes form a clique in Wgraph. Currently only testing is implemented.

	Rgraph is the reduced graph after removing the clique of Nodes,
with Weights being the nest list of removed weights.

==
?- G = [a-b:1,a-c:1,a-d:1,b-c:1,b-d:1,c-d:1,d-e:1], assert( wg(G) ).
?- wg(G), wgraph_clique([a,b,c,d],G).
G = [a-b:1, a-c:1, a-d:1, b-c:1, b-d:1, c-d:1, d-e:1].

?- wg(G), wgraph_clique([a,b,c,d],G,W,R).
G = [a-b:1, a-c:1, a-d:1, b-c:1, b-d:1, c-d:1, d-e:1],
W = [[1, 1, 1], [1, 1], [1], []],
R = [d-e:1].

==

@author nicos angelopoulos
@version  0.1 2015/3/30

*/
wgraph_clique( [], _Wgraph ).
wgraph_clique( [N|Ns], Wgraph ) :-
	wgraph_neighbours( N, Wgraph, Neighs ),
	ord_intersection( Neighs, Ns, Ns ),
	wgraph_clique( Ns, Wgraph ).

wgraph_clique( [], Wgraph, [], Wgraph ).
wgraph_clique( [N|Ns], Wgraph, [Weights|TWs], Rgraph ) :-
	wgraph_neighbours( N, Wgraph, Ns, Weights, Mgraph ),
	% ord_intersection( Neighs, Ns, Ns ),
	wgraph_clique( Ns, Mgraph, TWs, Rgraph ).
