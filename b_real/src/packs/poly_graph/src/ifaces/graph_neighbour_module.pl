%% graph_neighbour_module.
%
% Loading prerequisites predicate,
% use lib(graph_direcxtion_neighbour_module/0)
% to load the modules version of graph_neighbour/3 and associated predicates..
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
graph_neighbour_module.

%% graph_neighbour( +V, +Mod, -X ).
% 
% As graph_direction_neighbour( +Drc, +V, +Mod, -X ).
% where Drc is important and goes from V to X when Mod:edge(V,X), 
% is a fact in the database.
%
%==
% ?- G = [1-2,1-5,2-3,3-4], member(X-Y,G),  assert( g:edge(X,Y) ), fail.
% ?- graph_neighbour( 1, g, Y ).
% Y = 2 ;
% Y = 5.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
graph_neighbour( V, Mod, X ) :-
	call( Mod:edge(V,X) ).

%% graph_neighbour_undirected( +V, +Mod, -X ).
% 
% As graph_direction_neighbour( +Drc, +V, +Mod, -X ).
% where Drc is false.
%==
% ?- G = [1-2,1-5,2-3,3-1,3-4], member(X-Y,G),  assert( g:edge(X,Y) ), fail.
% ?- graph_neighbour_undirected( 1, g, Y ).
% Y = 2 ;
% Y = 5 ;
% Y = 3.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
graph_neighbour_undirected( V, Mod, X ) :-
	call( Mod:edge(V,X) ).
graph_neighbour_undirected( V, Mod, X ) :-
	call( Mod:edge(X,V) ).

%% graph_direction_neighbour( +Drc, +V, +Mod, -X ).
% 
%  True iff X is a neighbour of V in graph Mod.
%  This implementation of the predicate applies to facts-in-memory
%  representation of graphs.
%==
% ?- G = [1-2,1-5,2-3,3-4], member(X-Y,G),  assert( g:edge(X,Y) ), fail.
% ?- graph_direction_neighbour( true, 1, g, Y ).
% Y = 2 ;
% Y = 5.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
graph_direction_neighbour( false, X, Mod, Y ) :-
	graph_direction_neighbour_unidirected( X, Mod, Y ).
graph_direction_neighbour( true, X, Mod, Y ) :-
	call( Mod:edge(X,Y) ).
graph_direction_neighbour( reverse, X, Mod, Y ) :-
	call( Mod:edge(Y,X) ).

graph_direction_neighbour_unidirected( X, Mod, Y ) :-
	call( Mod:edge(X,Y) ).
graph_direction_neighbour_unidirected( X, Mod, Y ) :-
	call( Mod:edge(Y,X) ).
