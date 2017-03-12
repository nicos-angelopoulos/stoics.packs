
:- lib( stoics_lib:goal/4 ).

wgraph_known_cliques_replace_defaults( Defs ) :-
	Defs = [ min_pop(4),
	         replace_requires(all_present)
	       ].

/** wgraph_known_cliques_replace( +Wgraph, -Cgraph, -Fired, +Opts ).

Replace edges of cliques with central nodes from which edges eminate
to all present members. Non clique nodes that connect to all present
nodes point to the central node, else their connections are untouched.
Fired is a list of Clq-Set pairs, where Clq is a new node and Set is the 
set of all nodes connecting to Clq.

Opts 
 * cliques(Cliques)
   either a Clique-Members list, or /2 predicate holding Member, Clique relations

 * replace_requires(RR=all_present)
   i think this is min_pop_present actually
     
 * min_pop(MinPop=4)
   how many of the clique members have to be present for replace to take place

@author nicos angelopoulos
@version  0.1 2015/3/30
@tbd  complete(Comp) flag
@tbd  alternative implementations of replace_requires(RR) flag

*/
wgraph_known_cliques_replace( Wgraph, Cgraph, Fired, Args ) :-
	options_append( wgraph_known_cliques_replace, Args, Opts ),
	options( cliques(CliquesOpt), Opts ),
	wgraph_known_cliques( CliquesOpt, Cliques ),
	debug( wgraph, 'Known cliques: ~w', [Cliques] ),
	options( replace_requires(RR), Opts ),
	options( min_pop(MinPop), Opts ),
	integer( MinPop ),
	CutPop is MinPop - 0.5, % avoid =
	wgraph_vertices( Wgraph, Wnodes ),
	wgraph_known_cliques_replace_method( RR, Wgraph/Wnodes, Cliques/CutPop, Fired, Cgraph ).

wgraph_known_cliques_replace_method( RRMeth, Wgraph, Cliques, Fired, Cgraph ) :-
	wgraph_known_cliques_replace_known_method( RRMeth, Wgraph, Cliques, Fired, Cgraph ),
	!.
wgraph_known_cliques_replace_method( RRMeth, Cgraph, _Cliques, [], Cgraph ) :-
	throw( fixme(unknown_or_failed(replace_requires(RRMeth))) ).

wgraph_known_cliques_replace_known_method( all_present, WGTerm, CCTerm, Fired, Cgraph ) :-
	WGTerm = Wgraph/Cnodes,
	CCTerm = Cliques/MinPop,
	wgraph_known_cliques_ap( Cliques, Cnodes, Wgraph, MinPop, Fired, Lgraph ),
	debug( wgraph, 'Fired: ~w', [Fired] ),
	debug( wgraph, 'Clique replaced graph: ~w', [Lgraph] ),
	wgraph_cliques_neighbours_replace( Fired, Lgraph, Cgraph ).

wgraph_cliques_neighbours_replace( [], Graph, Graph ).
wgraph_cliques_neighbours_replace( [Clq-Set|T], Lgraph, Cgraph ) :-
	wgraph_clique_neighbours( Lgraph, Clq, Set, Rgraph, NWeighs ),
	findall( Clq-Neigh:Weigh, member(Neigh:Weigh,NWeighs), AddEdges ),
	wgraph_add_edges( Rgraph, AddEdges, Mgraph ),
	wgraph_cliques_neighbours_replace( T, Mgraph, Cgraph ).

wgraph_clique_neighbours( Lgraph, Clq, Set, Rgraph, NWeighs ) :-
	wgraph_vertices( Lgraph, Lverts ),
	ord_subtract( Lverts, Set, ClqIPverts ),
	ord_del_element( ClqIPverts, Clq, Pverts ),
	wgraph_clique_neighbours_1( Pverts, Lgraph, Set, Rgraph, NWeighs ).

wgraph_clique_neighbours_1( [], Graph, _Set, Graph, [] ).
wgraph_clique_neighbours_1( [Vtx|Vtcs], Lgraph, Set, Rgraph, NWeighs ) :-
	wgraph_neighbours( Vtx, Lgraph, Set, Weights, Ngraph ),
	!,
	min_list( Weights, MinW ),
	NWeighs = [Vtx:MinW|TNWeighs],
	wgraph_clique_neighbours_1( Vtcs, Ngraph, Set, Rgraph, TNWeighs ).
wgraph_clique_neighbours_1( [_Vtx|Vtcs], Lgraph, Set, Rgraph, NWeighs ) :-
	wgraph_clique_neighbours_1( Vtcs, Lgraph, Set, Rgraph, NWeighs ).


wgraph_known_cliques( [], [] ) :- !. % allows non cliques checking.
wgraph_known_cliques( Clique-Clembers, [Clique-Ordbers] ) :-
	!,
	sort( Clembers, Ordbers ).
wgraph_known_cliques( [C-M|T], Cliques ) :-
	!,
	maplist( wgraph_known_clique_sort, [C-M|T], Cliques ).
wgraph_known_cliques( CliquesPred, Cliques ) :-
	goal( CliquesPred, [X,CliqueName], user, Goal ),
	setof( CliqueName, X^Goal, CliqueNames ),
	findall( CliqueName-CliqueMembers, 
				( member(CliqueName,CliqueNames),
				  findall(X,Goal,CliqueMembers)
				),
				Cliques ).

wgraph_known_clique_sort( C-M, C-O ) :-
	sort( M, O ).

wgraph_known_cliques_ap( [], _Cnodes, Graph, _MinPop, [], Graph ).
wgraph_known_cliques_ap( [Clq|Clqs], Gnodes, Wgraph, MinPop, Fired, Cgraph ) :-
	wgraph_known_clique_ap( Clq, Gnodes, Wgraph, MinPop, Fired, TF, Mgraph ),
	wgraph_known_cliques_ap( Clqs, Gnodes, Mgraph, MinPop, TF, Cgraph ).

wgraph_known_clique_ap( Clq-Cnodes, Gnodes, Wgraph, MinPop, Fired, TF, Mgraph ) :-
	ord_intersection( Cnodes, Gnodes, ComNodes ),
	length( ComNodes, ComPop ),
	compare( Op, ComPop, MinPop ),
	wgraph_known_clique_ap_pop_op( Op, Clq, ComNodes, Wgraph, Fired, TF, Mgraph ).

wgraph_known_clique_ap_pop_op( <, Clq, _ComNodes, Mgraph, TF, TF, Mgraph ) :-
	debug( wgraph, 'Not enough clique members for: ~w', Clq ).
wgraph_known_clique_ap_pop_op( >, Clq, ComNodes, Wgraph, Fired, TF, Mgraph ) :-
	wgraph_known_clique_ap_include( Clq, ComNodes, Wgraph, Fired, TF, Mgraph ).

wgraph_known_clique_ap_include( Clq, ComNodes, Wgraph, Fired, TF, Mgraph ) :-
	wgraph_clique( ComNodes, Wgraph, CliqueWs, Rgraph ),
	!,
	% wgraph_remove_vertices_edges( Wgraph, ComNodes, Mgraph ),
	flatten( CliqueWs, FlatWs ),
	min_list( FlatWs, MinW ),
	findall( Clq-Nth:MinW, member(Nth,ComNodes), AddEdges ),
	% nth1(M,ComNodes,Mth),N<M), AddEdges ),
	wgraph_add_edges( AddEdges, Rgraph, Mgraph ),
	Fired = [Clq-ComNodes|TF].

wgraph_known_clique_ap_include( Clq, ComNodes, Mgraph, TF, TF, Mgraph ) :-
	Mess = 'Not all clique members of: ~w are cliqued: ~w',
	debug( wgraph, Mess, [Clq,ComNodes] ).
