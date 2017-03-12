
:- use_module( library(wgraph) ).
:- debug( wgraph ).

clique_ex :-
	findall( Edge, clex1(Edge), Wgraph ),
	CRopts = cliques(is_known_clique),
	wgraph_known_cliques_replace( Wgraph, Cgraph, _Fired, CRopts ),
	debug( wgraph, 'Cgraph: ~w', [Cgraph] ),
	wgraph_plot( Cgraph, [] ).

clique_ex_2 :-
	findall( Edge, (clex1(Edge);clex2_adds(Edge)), Wgraph ),
	CRopts = cliques(is_known_clique),
	wgraph_known_cliques_replace( Wgraph, Cgraph, _Fired, CRopts ),
	debug( wgraph, 'Cgraph: ~w', [Cgraph] ),
	wgraph_plot( Cgraph, [] ).

clique_ex_2_col :-
	findall( Edge, (clex1(Edge);clex2_adds(Edge)), Wgraph ),
	CRopts = cliques(is_known_clique),
	wgraph_known_cliques_replace( Wgraph, Cgraph, Fired, CRopts ),
	debug( wgraph, 'Cgraph: ~w', [Cgraph] ),
	findall( Clq-Elem, (member(Clq-Set,Fired),member(Elem,Set)), Pairs ),
	clique_ex_2_wgraph_colours( Cgraph, Pairs, Clrs ),
	wgraph_plot( Cgraph, [plotter(igraph),edge_colours(Clrs)] ).

clique_ex_2_wgraph_colours( [], _Pairs, [] ).
clique_ex_2_wgraph_colours( [H|T], Pairs, Clrs ) :-
	( H=(From-To:_) ->
		( (memberchk(From-To,Pairs);memberchk(To-From,Pairs)) ->
			Clrs = [120-60|TClrs]
			;
		     Clrs = [260-60|TClrs]
		)
		;
		Clrs = TClrs 
	),
	clique_ex_2_wgraph_colours( T, Pairs, TClrs ).

clex1( a-b:1 ).
clex1( a-c:1 ).
clex1( a-d:1 ).
clex1( b-c:1 ).
clex1( b-d:1 ).
clex1( c-d:1 ).
clex1( d-e:1 ).

clex2_adds( a-e:1 ).
clex2_adds( b-e:1 ).
clex2_adds( c-e:1 ).

is_known_clique( a, abcd ).
is_known_clique( b, abcd ).
is_known_clique( c, abcd ).
is_known_clique( d, abcd ).
is_known_clique( z, abcd ).

is_known_clique( k, klm ).
is_known_clique( l, klm ).
is_known_clique( m, klm ).
