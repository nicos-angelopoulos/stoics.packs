
:- lib(suggests(disp_bn)).  % load it if itis there
:- lib(options).        % /2, _append/3
:- lib(ugraphs).        % add_edges/3, add_vertices/3.
:- lib(poly_graph).
% :- lib(graph_k_sub/4).

:- lib(stoics_lib:kvs_k_memberchk/3).

% :- lib(k_sub_graph/4).              % expects sub_graph/3.
:- lib(colour_de_reg/1).
% :- lib(sub_graph_kvs/0).
% :- lib(graph_neighbour_kvs/0).

symboled_graph_display_defaults( [
			   edge_width(true),edge_width_min(1),edge_width_max(2),
			   focus_genes([]),focus_k(2),
			   range_min(500),range_max(1000),
			   show_orphans(true),symb_colours([]),
			   stem(symb_graph),title('')
                 ] ).

/** symboled_graph_display( SymbG, UpAtms, DwAtms, Gsymbs, Opts ).

This is the graphviz version, SymbG is expected to be a ugraph 
and information about edges should be already in Opts. 

We need to modernise this and add it is an alternative to wgraph_plot.
Modernise by dealing with widths within the predicate and letting SymbG 
be a wgraph.

See string_symboled_display/4. 

Used to have 2 more args, Stem & Title; 4+5, which are now in Opts.

Opts
 * output(Dev)
    output device |
 * symb_colours([])
    GeneSymb-ColourString pairs
 * focus_k(Fk=2)
    the length of the focus chain
 * focus_genes(Fgs=[])
   genes to focus graph on. no focus when []
 * edge_width(true)
    whether to use edge width
 * edge_width_max(EWmax=2)
    maximum edge width
 * edge_width_min(EWmin=1)
    minimum edge width
 * range_max(RGmax=1000)
    maximum for range of weights
 * range_min(RGmin=500)
    minimum for range of weights
 * stem(Stem=symb_graph)
    stem for filename
 * show_orphans(SwOrph=true)
    else _false_
 * title(Title='')
    title for the graph

==
?- assert( g([a-b,b-c,c-a]) ), assert ns([a,b,c]).
?- g(G), ns(Ns), symboled_graph_display( G, Ns, [], Ns, [] ).
?- g(G), ns(Ns), symboled_graph_display( G, Ns, [], Ns, [output(svg)] ).
?- @ eog( abc.svg ).
==

@author nicos angelopoulos
@version  0.1 2015
@tbd allow flag that tells us the Graph is not a de_regulation graph (as to use a different colour for instance).
*/
symboled_graph_display( SymbG, Ups, Dws, Args ) :-
	options_append( symboled_graph_display, Args, Opts ),
	Edgies = [edge_width(Ewidth),edge_width_min(EwMin),edge_width_max(EwMax)],
	Rangies = [range_max(Rmx),range_min(Rmn)],
	options( Edgies, Opts, rem_opts(EOpts) ),
	options( Rangies, EOpts, rem_opts(ROpts) ),
	options( [stem(Stem),title(Title)], ROpts, rem_opts(StOpts) ),
	debug( string, 'Option, edge_width(~w)', [Ewidth] ),

	plot_edge_widths( Ewidth, EwMin, EwMax, SymbG, Rmx, Rmn, WidthOpt ),
	options( show_orphans(ShO), StOpts, rem_opts(SOpts) ),
	options( [focus_genes(Fenes),focus_k(K)], SOpts, rem_opts(FOpts) ),
	sort( SymbG, OrdSymbG ),
	focus_genes_reduce_graph( Fenes, K, OrdSymbG, FenbG ),
	append( Ups, Dws, Gsymbs ),
	show_orphans( ShO, Gsymbs, FenbG, FGsymbs ),
	Gopts = [WidthOpt|FOpts],
	focused_graph_display( FenbG, Ups, Dws, Fenes, Stem, Title, FGsymbs, Gopts ).

show_orphans( false, _Gsymbs, FenbG, FGsymbs ) :- 
	!,
	findall( [X,Y], member(X-Y,FenbG), XYs ),
	flatten( XYs, FGsymbsUno ),
	sort( FGsymbsUno, FGsymbs ).
show_orphans( true, Gsymbs, _FenbG, Gsymbs ).

% if there are focus genes call the reduction, otherwise return the input
focus_genes_reduce_graph( [], _K, SymbG, SymbG ) :- !.
focus_genes_reduce_graph( Fenes, K, SymbG, FeneG ) :-
	graph_k_sub( SymbG, Fenes, K, FeneG ).
	% edges_graph_vertex_k_graph( SymbG, Fenes, K, FeneG ).

/*
symboled_graph_display( SymbG, UpAtms, DwAtms, Stem, Title, Gsymbs, Mod, Opts ) :-
	SymbG \== [],  % this ensures that debug message in fgd/2 below is accurate
	select_first( Opts, focus_k(_K), RemOpts ),
	focused_graph_display( SymbG, UpAtms, DwAtms, '$$not_in_exec_path', Stem, Title, Gsymbs, Mod, RemOpts ).
	*/

focused_graph_display( [], _UpAtms, _DwAtms, Fenes, Stem, Title, _Gsymbs, _Opts ) :-
	!,
	% ( memberchk(focus_genes(Fenes),Opts) -> true; Fenes = '$$not_present$$' ),
	% fixme: this should only be true in Focused runs. otherwise it should throw an error.
	debug( string, 'Skipped writing empty graph:~w, stem:~w, title:~w', [Fenes,Stem,Title] ).
focused_graph_display( [H|T], UpAtms, DwAtms, _Fenes, Stem, Title, Gsymbs, Opts ) :-
	% maplist( writeln, EBMap ),
	SymbG = [H|T],
	length( SymbG, SymbGLen ),
	debug( string, 'Number of edges symboled: ~d', SymbGLen ),
	add_vertices( [], Gsymbs, PlGraph1 ),
	debug( _, 'SymbG: ~w', [SymbG] ),
	findall( Edge, member(Edge:_,SymbG), SymbPlG ),
	add_edges( PlGraph1, SymbPlG, PlGraph ),
	colour_de_reg( DeRegC ),
	Style = style('radial'),
	Fill  = fillcolor(Fcol),
	% select_first( Opts, symb_colours(SymbClrsIn), RemOpts ),
	options( symb_colours(SymbClrsIn), Opts, rem_opts(SOpts) ),
	keysort( SymbClrsIn, SymbClrs ),
	findall( Gsymb-Attrs, (  member(Gsymb-_Edged,PlGraph),
						gsymb_colour( Gsymb, SymbClrs, UpAtms, DwAtms, DeRegC, Col ),
						atom_concat('white:',Col,Fcol),
	                      	Attrs = [Style,Fill] 
				       ), NdAttrs ),
	assert( pl_graph(PlGraph) ),
	file_name_extension( Stem, graph, GraphF ),
	open( GraphF, write, Out ),
	portray_clause( Out, graph(PlGraph) ),
	close( Out ),
	debug( string, 'Graph file: ~p', GraphF ),
	file_name_extension( Stem, dot, Dotted ),
	DispOpts = [    title(Title), % output(x11),
		       output(png), 
		       output(pdf), 
		       output(svg), 
			  type(graph),
	            output_stem(Stem), nodes_attrs(NdAttrs),
			  dot_file(Dotted)
			  | SOpts % fixme
			  % | RemOpts % fixme
			 ],
	% debug( string, 'disp_bn/2 options: ~p', [DispOpts] ),
	debug( string, 'disp_bn opts: ~w', [DispOpts] ),
	disp_bn( PlGraph, DispOpts ).

plot_edge_widths( false, _EwMin, _EwMax, _Graph, _Rmn, _Rmx, false ).
plot_edge_widths( true, Min, Max, Graph, Coff, Rmx, Widths ) :-
	Range is Rmx - Coff,
	Diff is Max - Min,
	Segm = 6,
	ColSpc is Range / Segm,
	findall( X-Y-[penwidth(Width),color(Steel)], ( member(X-Y:Ev,Graph),
	                      Val is (Ev - Coff),
	                      Width is Min + ( (Val / Range ) * Diff ),
					  SteelId is min((Val//integer(ColSpc))+2,7),
					  % atomic_list_concat( ['/blues5/',SteelId], Steel )
					  atomic_list_concat( ['/bupu7/',SteelId], Steel )
					), Trips ),
	Widths = edges_attrs(Trips).

gsymb_colour( Gsymb, GsClrs, _UpAtms, _DwAtms, _DeRegC, Clr ) :-
	kvs_k_memberchk( Gsymb, GsClrs, GsClr ),
	!,
	GsClr = Clr.
gsymb_colour( Gsymb, _GsClrs, SymbsUp, SymbsDw, DeRegC, Col ) :-
	atomic_list_concat( Gparts, ';', Gsymb ),
	findall( Gn, (member(Gn,Gparts),memberchk(Gn,SymbsUp)), UpAs ),
	findall( Gn, (member(Gn,Gparts),memberchk(Gn,SymbsDw)), DwAs ),

	maplist( length, [UpAs,DwAs], [UpAsLen,DwAsLen] ),
	de_reg_length_colour( UpAsLen, DwAsLen, DeRegC, Col ).

	/* memberchk(Ep-Id,EBMap),
		(memberchk(Ep,UpELs) -> Col=UpCol;Col=DwCol), */

de_reg_length_colour( 0, 0, _DeRegC, "black" ) :- !.
de_reg_length_colour( _, 0, up_down(UpCol,_), UpCol ) :-  !.
de_reg_length_colour( 0, _, up_down(_,DwCol), DwCol ) :-  !.
de_reg_length_colour( _, _, up_down(_,_), "orange" ).

