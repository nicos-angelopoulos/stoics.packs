/** wgraph_vertex_frequencies( +Wgraph, -Freqs ).

Edge frequencies for all nodes in Wgraph.

==
?- 
     wgraph_vertex_frequencies( [a,b-c:1,b-d:2], Freqs1 ).
Freqs1 = [a-0, b-2, c-1, d-1].

==

@author nicos angelopoulos
@version  0:1 2021/02/05
@tbd add argument/option to add weights rather then counts

*/
wgraph_vertex_frequencies( Wgraph, Freqs ) :-
     wgraph_vertices( Wgraph,  Verts ),
     sort( Verts, Oerts ),
     findall( Vert-0, member(Vert,Oerts), Pairs ),
     wgraph_edges_vertex_frequencies( Wgraph, Pairs, Freqs ).

wgraph_edges_vertex_frequencies( [], Pairs, Freqs ) :-
     Pairs = Freqs.
wgraph_edges_vertex_frequencies( [Eg|Egs], Pairs, Freqs ) :-
     ( Eg = Nd1-Nd2:_Wgt ->
          sort( [Nd1,Nd2], [NdA,NdB] ),
          wgraph_verticies_frequency_increase( Pairs, NdA, [NdB], Next )
          ;
          Next = Pairs
     ),
     wgraph_edges_vertex_frequencies( Egs, Next, Freqs ).

wgraph_verticies_frequency_increase( [], Nd, Nds, _Rem ) :-
     ( (Nd = [],Nds = []) -> true; throw( cannot_locate_node(Nd) ) ).
wgraph_verticies_frequency_increase( [Nd-Cnt|Pairs], NdA, Nds, Counts ) :-
     ( Nd == NdA ->
          NewNdCnt is Cnt + 1,
          ( Nds = [NxtNd|NxtNds] ->
               NxtPairs = Pairs,
               Counts = [Nd-NewNdCnt|Tounts]
               ;
               NxtPairs = [],
               NxtPairs = [],
               NxtNds = [],
               Counts = [Nd-NewNdCnt|Pairs]
          )
          ;
          NxtPairs = Pairs,
          NxtNd = NdA,
          NxtNds = Nds,
          Counts = [Nd-Cnt|Tounts]
     ),
     wgraph_verticies_frequency_increase( NxtPairs, NxtNd, NxtNds, Tounts ).
