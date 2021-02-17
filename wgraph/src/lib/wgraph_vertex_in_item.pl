/** wgraph_vertex_in_item( ?Vert, +Elem ).

True iff Vert is a vertex referenced in graph element Elem. 

This can be either because Item is an edge of the form Vert1-Vert2:Weight, 
or Item is the vertex itself. The order of the arguments is unconventional,
to make it compatible for meta-calls (see first example below). 
The predicate can generate Vert (all, on backtracking).


==
?- include( wgraph_vertex_in_item(b), [a,b-c:1,b-d:2], Graph ).
Graph = [b-c:1, b-d:2].

?- include( wgraph_vertex_in_item(a), [a,b-c:1,b-d:2], Graph ).
Graph = [a].

?- include( wgraph_vertex_in_item(a), [a,b-c:1,b-d:2], Graph ).
Graph = [b-d:2].

?- wgraph_vertex_in_item( b, b-c:1 ).
true.

?- wgraph_vertex_in_item( X, b-c:1 ).
?- wgraph_vertex_in_item( X, b-c:1 ).
X = b ;
X = c.
==

@author nicos angelopoulos
@version  0:1 2021/02/05

*/
wgraph_vertex_in_item( Vert, Item ) :-
     Item = Vert1-Vert2:_Weight,
     !,
     ( atomic(Vert) ->
          ( Vert == Vert1 ->
               true
               ;
               Vert == Vert2
          )
          ;
          ( Vert = Vert1 ; Vert = Vert2 )
     ).

wgraph_vertex_in_item( Vert, Item ) :-
     atomic( Item ),
     Item = Vert.
