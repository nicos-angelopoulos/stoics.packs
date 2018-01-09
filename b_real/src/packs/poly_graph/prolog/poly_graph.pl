:- module( poly_graph, [    
                poly_graph/1, poly_graph/2,
                polymorphic_graph/1, polymorphic_graph/2,
                graph_k_neighbours/4, graph_k_neighbours/5,
                graph_k_sub/4, graph_k_sub/5,
                graph_nodes_sub/3
    ] ).

:- ensure_loaded( library(lib) ).
:- ensure_loaded( library(ugraphs) ).

:- lib(stoics_lib).   % functor_term/2, en_list/2, kvs_k_memberchk/3.

:- lib(source(poly_graph), homonyms(true)).

:- ( current_prolog_flag(poly_graph_load,Iface) -> true; Iface = all ),
   ( Iface == all ->
        ensure_loaded( '../src/ifaces/iface_all' )
        ;
        ( Iface == kves ->
            ensure_loaded( '../src/ifaces/iface_kves' )
            ;
            throw( poly_graph(unimplemented_load_interface(Iface)) )
        )
    ),
    set_prolog_flag(poly_graph_iface,Iface).

:- lib(graph_k_neighbours/4).
:- lib(graph_k_sub/4).

:- lib(end(poly_graph)).

%%%%  here: in lib(lib) add support for "private" packs in src/packs/ ... 
%%%%% maybe within lib(source(_)) and lib(end(_)) ?!

/** <module> poly_graph

	Polymorphic graphs.

==
?- lib(poly_graph).
?- graph_k_neighbours( [1-2,1-5,2-3,3-4,5-6,6-7], 1, 2, Which ).
Which = [1, 2, 3, 5, 6].
?- listing( graph_direction_neighbour ).
poly_graph:graph_direction_neighbour(kves, A, B, C, D) :-
	graph_direction_neighbour_kves(A, B, C, D).
poly_graph:graph_direction_neighbour(kvns, A, B, C, D) :-
	graph_direction_neighbour_kvns(A, B, C, D).
poly_graph:graph_direction_neighbour(fact, A, B, C, D) :-
	graph_direction_neighbour_fact(A, B, C, D).

true.
==

==
?- set_prolog_flag( poly_graph_load, kves ).
?- lib(poly_graph).
?- prolog_flag(poly_graph_iface,Iface).
Iface = kves.
?- graph_k_neighbours( [1-2,1-5,2-3,3-4,5-6,6-7], 1, 2, Which ).
Which = [1, 2, 3, 4, 5].

?- listing( graph_direction_neighbour ).
ERROR: procedure `graph_direction_neighbour' does not exist (DWIM could not correct goal)
--- % because the specialised version by-passes this step in the interest of performance
==

==
?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1], 2, Sub ).
Sub = [1-2, 1-5, 2-3, 5-6].



==

@author nicos angelopoulos
@version  0.1 2018/01/08
@see poly_graph/2

*/


%% poly_graph(+Graph).
%% poly_graph(+Graph,-Type).
%% polymorphic_graph(+Graph).
%% polymorphic_graph(+Graph,-Type).
%
% A polymorphic_graph, or poly_graph for short, is either a 
% graph represented by V1-V2 edges, or a module in which edge/2 and/or node/1 are defined.
%
% Type
% * fact
%    as, Mod:edge(X,Y) and/or Mod:node(X)
% * kves
%    Nd1-Nd2 ordered lists  
% * kvns 
%    Nd-Neighs  ordered lists of ordered Neighbours
%    (Neighbours membership map to Nd-Nd1 and edge(Nd,Nd1))
%
%  Whether the graph is directed and if so to which direction is underspecified as 
%  it depends on the operation.
% 
%==
% ?- poly_graph( [1-2,3-4], Type ).
% Type = kves.
% ?- poly_graph( [1-[2,3],3-[4,5]], Type ).
% Type = kvns.
% ?- assert( mod1:edge(c,b) ), assert( mod1:edge(b,e) ).
% ?- poly_graph( mod1, Type ),
% Type = fact.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/28
%
poly_graph( Graph ) :-
	polymorphic_graph( Graph, _Type ).
poly_graph( Graph, Type ) :-
	polymorphic_graph( Graph, Type ).

polymorphic_graph( Graph ) :-
	polymorphic_graph( Graph, _Type ).

polymorphic_graph( Graph, Type ) :-
	\+ var( Graph ),
	polymorphic_graph_known( Graph, Type ).

polymorphic_graph_known( Graph, Type ) :-
	maplist( neighbours_term, Graph ),
	sort( Graph, Graph ),
	!,
	Type = kvns.

polymorphic_graph_known( Graph, Type ) :-
	maplist( functor_term((-)/2), Graph ),
	sort( Graph, Graph ),
	!,
	Type = kves.
polymorphic_graph_known( Graph, Type ) :-
	atom( Graph ),
	( current_predicate(Graph:edge/2) ; current_predicate(Graph:node/1) ),
	% fixme: print warning otherwise
	!,
	Type = fact.

neighbours_term( Node-Edges ) :-
	atomic( Node ), 
	\+ var(Edges),
	Edges=[_|_],
	sort( Edges, Edges ).
