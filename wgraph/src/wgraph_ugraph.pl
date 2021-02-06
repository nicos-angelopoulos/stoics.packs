
:- lib(stoics_lib:en_list/2).

%% wgraph_ugraph( +Wgraph, -Ugraph ).
%% wgraph_ugraph( -Wgraph, +Ugraph ).
%% wgraph_ugraph( -Wgraph, +Weights, +Ugraph ).
%
% Convert between wgraph and ugraph representations. 
%
% When constructing a Wgraph, Weights can be a single atomic, or a list of atomic value and
% Wgraph edges (Nd1-Nd2:Wgt). If a single atomic value (also works if a singleton atomic is given),
% then all weights are set to this atomic. When a list given then for each Ugraph Nd1-Nd2, Nd1-Nd2:Wgt is looked for in Weights, and if found Wgt is used. Else the first atomic in the list is used. 
% If no atomic is given in the list, default is 1. Which is also the default if no Weights
% is given (wgraph_ugraph(-,+).
% 
%==
% ?- wgraph_ugraph( [1-2:1,1-3:2,2-3:4], Ug ).
% Ug = [1-[2, 3], 2-[3], 3-[]].
%
% ?- wgraph_ugraph( [1-2:1,1-3:2,2-3:1,4], Ug ).
% Ug = [1-[2, 3], 2-[3], 3-[], 4-[]].
% 
% ?- wgraph_ugraph( [1-2:1,1-3:2,2-3:1,4], Ug ), wgraph_ugraph( Wg, Ug ).
% Ug = [1-[2, 3], 2-[3], 3-[], 4-[]],
% Wg = [4, 1-2:1, 1-3:1, 2-3:1].
%
% ?- 
%    Org = [1-2:1,1-3:2,2-3:1,4],
%    wgraph_ugraph( Org, Ug ), del_vertices( Ug, [2], Del ),
%    wgraph_ugraph( New, Org, Del ).
% Org = [1-2:1, 1-3:2, 2-3:1, 4],
% Ug = [1-[2, 3], 2-[3], 3-[], 4-[]],
% Del = [1-[3], 3-[], 4-[]],
% New = [4, 1-3:2].
% 
%==
%
wgraph_ugraph( Wg, Ug ) :-
	ground( Wg ),
	!,
	wgraph_to_ugraph( Wg, [], Ug ).
wgraph_ugraph( Wg, Ug ) :-
	ground( Ug ),
	wgraph_ugraph( Wg, 1, Ug ).

wgraph_ugraph( Wg, WgtsIn, Ug ) :-
	ground( Ug ),
     en_list( WgtsIn, Wgts ),
     partition( atomic, Wgts, Atms, Edges ),
     ( Atms = [Def|_] -> true; Def = 1 ),
     ugraph_to_wgraph( Ug, Edges, Def, Wg ).

wgraph_to_ugraph( [], Ug, Ug ).
wgraph_to_ugraph( [Edge|Wg], Acc, Ug ) :-
	wgraph_edge_ugraph( Edge, Acc, Nxt ),
	wgraph_to_ugraph( Wg, Nxt, Ug ).

wgraph_edge_ugraph( From-To:_W, Acc, Nxt ) :-
	!,
	add_edges( Acc, [From-To], Nxt ).
wgraph_edge_ugraph( Node, Acc, Nxt ) :-
	atomic( Node ),
	!,
	add_vertices( Acc, [Node], Nxt ).

ugraph_to_wgraph( Ug, Xdges, Def, Wg ) :-
     edges( Ug, Udges ),
     findall( NotSource, member(NotSource-[],Ug), NotSourced ),
     findall( Set, member(_-Set,Ug), Sets ),
     ord_union( Sets, SeenIn ),
     ord_subtract( NotSourced, SeenIn, Orphans ),
     maplist( ugraph_edge_wgraph(Xdges,Def), Udges, Wdges ),
     append( Orphans, Wdges, Pg ),
     sort( Pg, Wg ).

ugraph_edge_wgraph( Xdges, Def, Nd1-Nd2, Wdge ) :-
     % fixme: should we also look for N2-N1:Wgt ???
     ( memberchk(Nd1-Nd2:Wgt,Xdges) ->
          Wdge = Nd1-Nd2:Wgt
          ;
          Wdge = Nd1-Nd2:Def
     ).
