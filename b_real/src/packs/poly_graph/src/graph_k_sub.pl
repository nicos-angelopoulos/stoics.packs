/** graph_k_sub( +Graph, +Nodes, +K, -Sub ).
    graph_k_sub( +Graph, +Nodes, +K, +Drc, -Sub ).

For a subset of Nodes appearing in Graph create the Sub-graph that 
can be reached within K steps. Default Drc is false.

==
?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1], 2, Sub ).
Sub = [1-2, 1-5, 2-3, 5-6].

?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1], 1, true, Sub ).
Sub = [1-2, 1-5].

?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [1], 2, true, Sub ).
Sub = [1-2, 1-5, 2-3, 5-6].

?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [2], 1, true, Sub ).
Sub = [2-3].

?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [2], 1, false, Sub ).
Sub = [1-2, 2-3].

?- graph_k_sub( [1-2,1-5,2-3,3-4,5-6,6-7], [2], 2, false, Sub ).
Sub = [1-2, 1-5, 2-3, 3-4].

==

@author nicos angelopoulos
@version  0.1 2018/1/9

*/
graph_k_sub( Graph, Nodes, K, Sub ) :-
    graph_k_sub( Graph, Nodes, K, false, Sub ).
