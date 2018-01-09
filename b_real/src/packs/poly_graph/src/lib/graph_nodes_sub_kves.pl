graph_nodes_sub_kves( [], _, [] ).
graph_nodes_sub_kves( [V1-V2|T], RedNodes, Sub ) :-
    !,
	list_to_ord_set( [V1,V2], V12 ),
	( ord_intersection( V12, RedNodes, [_,_] ) -> Sub = [V1-V2|TSub]; TSub = Sub ),
    graph_nodes_sub_kves( T, RedNodes, TSub ).
graph_nodes_sub_kves( [Node|T], RedNodes, Sub ) :-
    ( atomic(Node),ord_memberchk(Node,RedNodes) -> 
        Sub = [Node|TSub]
        ;
        TSub = Sub
    ),
    graph_nodes_sub_kves( T, RedNodes, TSub ).
