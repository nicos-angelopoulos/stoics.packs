
:- lib( stoics_lib:kvo_k_memberchk/3 ).

%% graph_neighbour_kvs.
%
% Loading prerequisites predicate,
% use lib(graph_neighbour_kvs/0)
% to load the kvs version of graph_neighbour/3 and associated predicates..
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
% graph_neighbour_kvs.
% 
% graph_neighbour( V, Edges, X ) :-
	% kvs_k_member( V, Edges, X ).

%% graph_direction_neighbour_kves( +Dct, +Vert, +Edges, -Neighbour ).
% 
% True iff Neighbour is a neighbour of Vert in Edges, which is a (at least key)
% sorted edges graph with direction indicated by Dct (_false_, _true_ or _reverse).
% When looking for neighbours of X and Dct == false (= undirected graph)
% if both X-Y and Y-X are in the mix, then Y is returned twice. 
% Callers, should sort findalls in this scenario.
%
% Note that _reverse_ is less efficient as it uses a member/2 check that takes
% no advantage of the ordering to conclude early failure.
% 
%==
%  ?- EG1 = [1-2,2-3,3-4,1-5,5-6,6-7], keysort( EG1, Ord1 ), assert( eg1(Ord1) ).
%  ?- eg1(EG1), edges_graph_direction_vertex_neighbour( false, 1, EG1, X ).
%  X = 2 ;
%  X = 5 ;
%  false.
%
%  ?- eg1(EG1), edges_graph_direction_vertex_neighbour( reverse, 5, EG1, X ).
%  X = 1 ;
%  false.
%
% ?- eg1(EG1), edges_graph_direction_vertex_neighbour( true, 5, EG1, X ).
% EG1 = [1-2, 1-5, 2-3, 3-4, 5-6, 6-7],
% X = 6 ;
% false.

% ?- eg1(EG1), edges_graph_direction_vertex_neighbour( true, 1, EG1, X ).
% EG1 = [1-2, 1-5, 2-3, 3-4, 5-6, 6-7],
% X = 2 ;
% EG1 = [1-2, 1-5, 2-3, 3-4, 5-6, 6-7],
% X = 5 ;
% false.
% ?- eg1(EG1), edges_graph_direction_vertex_neighbour( false, 1, EG1, X ).
% % EG1 = [1-2, 1-5, 2-3, 3-4, 5-6, 6-7],
% X = 2 ;
% EG1 = [1-2, 1-5, 2-3, 3-4, 5-6, 6-7],
% X = 5 ;
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/5/28
% @see edges_graph_k_neighbourhood/4
% @tbd add better _false_ example
%
graph_direction_neighbour_kves( true, V, Edges, X ) :-
	kvo_k_memberchk( V, Edges, X ).
graph_direction_neighbour_kves( reverse, V, Edges, X ) :-
	member( X-V, Edges ).
graph_direction_neighbour_kves( false, V, Edges, X ) :-
	kvo_k_memberchk( V, Edges, X ) ; member( X-V, Edges ).
