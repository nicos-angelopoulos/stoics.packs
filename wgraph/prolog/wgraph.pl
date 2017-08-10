:- module( wgraph, [
                        wgraph/2, wgraph/3,
                        wgraph_add_edges/3,
                        wgraph_add_vertices/3,
                        wgraph_adjacency/2,wgraph_adjacency/3,
                        wgraph_clique/2, wgraph_clique/4,
                        wgraph_known_cliques_replace/4,
                        wgraph_vertices/2,
                        wgraph_plot/2,
                        wgraph_ugraph/2,
                        wgraph_version/2
                ] 
                    ).

/** <module> Weighted graphs, with plotting function via Real

For now the emphasis is on plotting via igraph and qgraph R libraries via Real. 

A weighted graphs is represented as a list of From-To:W edges or Node entries
for orphans.

See wgraph_plot/2.

@author nicos angelopoulos
@version  0.1 2015/6/12
@version  0:2 2016/1/23
@version  0.3 2017/3/12

*/

:- use_module( library(lib) ).
:- lib(real).

%% wgraph_version( -Version, -Date ).
%
% Version (Mj:Mn:Fx) and Date of publication (date(Y,M,D)).
%
%==
% ?- wgraph_version( V, D ).
% V = 0:3:0
% D = date(2017,3,2)
%==
wgraph_version( 0:3:0, date(2017,3,2) ).  % rebased to pack(lib)

:- lib( source(wgraph), homonyms(true) ).
:- lib(wgraph/2).
:- lib(wgraph_plot/2).
:- lib(wgraph_clique/2).
:- lib(wgraph_ugraph/2).
:- lib(wgraph_vertices/2).
:- lib(wgraph_adjacency/2).
:- lib(wgraph_add_edges/3).
:- lib(wgraph_neighbours/3).
:- lib(wgraph_add_vertices/3).
:- lib(wgraph_known_cliques_replace/4 ).
:- lib(end(wgraph)).
