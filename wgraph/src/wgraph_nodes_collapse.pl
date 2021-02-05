:- lib(stoics_lib:en_list/2).

/** wgraph_nodes_collapse( +Graph, +Nodes, +Collapse, -NewGraph ).


Replace all Nodes in Graph with a single Collapsed node to produce
New Graph.

Only 
==

?- 
     wgraph_nodes_collapse( [a,b-c:1,b-d:2], [d,c], e, G1 ).
G1 = [a, b-e:2].

?-
     wgraph_nodes_collapse( [a,b-c:1,b-d:2,e], [a,e], f, G2 ).

==

@author nicos angelopoulos
@version  0:1 2021/02/05
@tbd add rules or difference conflict resolution

*/
wgraph_nodes_collapse( GraphIn, NodesIn, Coll, NewG ) :-
     wgraph( GraphIn, Graph ),
     en_list( NodesIn, NodesL ),
     sort( NodesL, Nodes ),
     wgraph_nodes_collapse_1( Graph, Nodes, Coll, Gout ),
     sort( Gout, Sout ),  % remove multiple orphans
     wgraph_remove_dup_edges_ord( Sout, Uout ),
     wgraph( Uout, NewG ).

wgraph_remove_dup_edges_ord( [], [] ).
wgraph_remove_dup_edges_ord( [Itm|Itms], Gut ) :-
     ( atomic(Itm) ->
          NxtItms = Itms,
          Gut = [Itm|Nut]
          ;
          NxtItms = [],
          % can leave Nut free here
          wgraph_remove_dup_edges_ord_3( Itms, Itm, Gut )
     ),
     wgraph_remove_dup_edges_ord( NxtItms, Nut ).

wgraph_remove_dup_edges_ord_3( [], Prv, Out ) :-
     Out = [Prv].
wgraph_remove_dup_edges_ord_3( [Itm|Itms], Prv, Out ) :-
     ( atomic(Itm) ->
          Out = [Prv|Tut]
          ;
          Itm = Nd1-Nd2:Wgt1,
          ( Prv = NdA-NdB:WgtA ->
               ( (Nd1 == NdA, Nd2 == NdB) ->
                    Out = Tut,
                    ( Wgt1 > WgtA -> 
                         NxtPrv = Itm
                         ;
                         NxtPrv = Prv
                    )
                    ;
                    NxtPrv = Itm,
                    Out = [Prv|Tut]
               )
               ;
               NxtPrv = Itm,
               Tut = [Prv|Out]
          )
     ),
     wgraph_remove_dup_edges_ord_3( Itms, NxtPrv, Tut ).

wgraph_nodes_collapse_1( [], _Nodes, _Coll, Gout ) :-
     Gout = [].
wgraph_nodes_collapse_1( [Itm|Itms], Nodes, Coll, Gout ) :-
     ( atomic(Itm) -> 
          ( memberchk(Itm,Nodes) ->
               Gout = [Coll|Tout]
               ;
               Gout = [Itm|Tout]
          )
          ;
          Itm = Nd1-Nd2:Wgt,
          ( memberchk(Nd1,Nodes) -> NdA = Coll; NdA = Nd1 ),
          ( memberchk(Nd2,Nodes) -> NdB = Coll; NdB = Nd2 ),
          ( (NdA==Coll,NdB==Coll) -> 
               % fixme: optionise this
               Gout = Tout
               ;
               Gout = [NdA-NdB:Wgt|Tout]
          )
     ),
     wgraph_nodes_collapse_1( Itms, Nodes, Coll, Tout ).
