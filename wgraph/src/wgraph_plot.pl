
:- lib(mtx).
:- lib(real).
:- lib(options).  % /2, _append/4.
:- lib(debug_call).

:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:kv_decompose/3).
% :- lib(wgraph).
% :- lib(wgraph_adjacency).
% :- lib(wgraph_vertices/2 ).

wgraph_plot_defaults( All, Defs ) :- 
	All = [graph(WgraphIn)|Args],
	wgraph( WgraphIn, WgraphPrv ),
	( memberchk(orphan_edge_weight(OrphanW),Args) ->
		debug( wgraph_plot, 'Provisional: ~w', [WgraphPrv] ),
		wgraph_add_edges_for_orphans( WgraphPrv, OrphanW, Wgraph ),
		debug( wgraph_plot, 'Augmented: ~w', [Wgraph] ),
		debug_call( wgraph_plot, length, augm_graph/Wgraph )
		;
		WgraphPrv = Wgraph
	),
	Tail = [width(7),height(7),format(x11),post_call(true)],
	( memberchk(layout_call(LayG),Args) ->
		layout_plot_qgraph_layout(LayG,Lay)
		;
		wgraph_adjacency( Wgraph, lp_adj ),
		% wgraph_weights( ... )
		findall( W, member(_-_:W,Wgraph), Ws ),
		( memberchk(layout_mtx(LayF),Args) ->
			mtx( LayF, Lay, convert(true) ),
			mtx_column_name_options( Lay, x, layout, Xs, Args ),
			mtx_column_name_options( Lay, y, layout, Ys, Args )
			;
			Lay = [row(x)],
			gnet <- 'graph.adjacency'(lp_adj, weighted='T', mode ="undirected"),
			( memberchk(layout_fun(LayFun),Args) -> true; 
				% LayFun = layout.fruchterman.reingold.grid ),
				LayFun = 'layout.fruchterman.reingold' ),
			( memberchk(layout_args(LayArgS),Args) -> en_list(LayArgS,LayArgs);
			     LayArgs = [] ),
			FunCall =.. [LayFun,gnet|LayArgs], % fixme: parameters
			layout1 <- FunCall,
			Lay1 <- layout1, 
			findall( X-Y, member([X,Y],Lay1), XYs ),
			kv_decompose( XYs, Xs, Ys )
		)
	),
	mtx_column_name_options( Lay, color, Clrs="steelblue1", Clrs, Args ),
	mtx_column_name_options( Lay, label_distance, Ldist=0.5, Ldist, Args ),
	% Ldegre = -pi/4, although doc says -pi/2
	mtx_column_name_options( Lay, label_degree, Ldegree= -0.7853982, Ldegree, Args ),
	wgraph_vertices( Wgraph, Nodes ),
	( memberchk(labels(Lbls),Args) ->  
		true
		;
			% fixme: was labels() instead or label, which is correct?
		mtx_column_name_options( Lay, label, Lbls=Nodes, Lbls, Args )
	),
	( memberchk(stem(Stem),Args) -> 
		Save = true
		;
		Save = false,
		Stem = wgraph
	),
	( memberchk(w_threshold(Thres),Args) -> true; Thres=1 ),
	append( Args, [ max_width(3), max_weight(max), min_width(1),
	                min_weight(min), w_threshold(Thres) ], ArgsLim ),
	wgraph_plot_weights_widths( Ws, Wis, Hus, ArgsLim ),
	maplist( en_list, [Ws,Wis,Hus], [WsL,WisL,HusL] ),
	debug_call( wgraph_plot, length, [ws,wis,hus]/[WsL,WisL,HusL] ),
	Defs = [useDingbats('TRUE'),orphan_edge_weight(0),label_distance(Ldist),label_degree(Ldegree),save(Save),stem(Stem),colours(Clrs),labels(Lbls),x(Xs),y(Ys),plotter(qgraph),w(Wis),edge_colours(Hus),node_size(8)|Tail].

wgraph_plot_test( Args ) :-
	Wg = [a-b:1,a-c:2,b-c:1,a-d:3,e],
	Clrs = c("khaki"), Lbls = ["A","B","C","D","E"],
	wgraph_plot( Wg, [colours(Clrs),labels(Lbls),width(3)|Args] ).
	
%% wgraph_plot( +Wgraph, +Opts ).
%
% Display weighted graph Wgraph from a layout that may include colours and labels.
% Wgraph should be in a form accepted by wgraph/3.
%
% Layout (see below) should be an mtx/1 matrix with at least two columns: x, y defining
% the positions of the nodes.
%
% In addition if columns: labels and colours are present but not given in Opts they will be used.
%
% Graph should be an mtx/1 matrix with at least 2 columns: from and to. 
% In addition column `weight` is also processed. When missing and no weights option
% is given, all weights are set to 1.
% 
% Opts 
%  * Ropt=Rval 
%    = paired options are passed to the plotter call, see r_call/2
%  * cnm_colour(Clrs=colour)
%    = (layout) colours column name
%  * cnm_label(Lbs=label)
%    (layout) labels column name
%  * cnm_x(Xc=x)
%    (layout) column name for X position coordinate
%  * cnm_y(Yc=y)
%   (layout) column name for Y position coordinate
%  * cnm_from(From=from)
%    (graph) column name for From column
%  * cnm_to(To=to)
%    (graph) column name for To column
%  * cnm_weight(Wc=weight)
%    (graph) column name for edge weight
%  * colours(Clrs="white")
%    colours for the nodes - should be known to R
%  * format(Frm=x11)
%    output format: _pdf_, _x11_ or _none_ (as x11 without explicit <- x11() call).
%    when Plotter is ggnet2 then Frm can x11 or any file extension recognised by
%    your installation of ggsave().
%  * labels(Lbs=`nodes`)
%    labels for the nodes, _[]_ for none, _false_ for leaving it unset
%  * layout_call(LayG) 
%    layout R/qgraph function to use for getting x,y coordinates.
%    defaults to... which is only used if layout_mtx(LayM) is also missing
%  * layout_mtx(LayM)
%    used if layout_call(LayG) is not present
%  * orphan_edge_weight(OEW)
%    if present an edge is added for every orphan from a medianed node with this weight
%  * plotter(Plotter=qgraph)
%    also known: _igraph_ and _ggnet2_
%  * save(Save=true)
%    to save the layout and graph, defaults to false if no stem is given
%  * stem(Stem)
%    stem of the output file (def. replaces  .csv to .pdf if LayM is a file- else wgraph_plot)
%  * height(H=7)
%    height of the plot device
%  * width(W=7)
%    width of the plot device
%  * label_distance(LbD=0.5)
%    distance for vertex labels 
%  * node_size(Vz)
%    size of nodes, can be prop(Min,Mult)- size being proportional to label length
%  * wgraph(Wgraph)
%    the weighted graph (wgraph/1)
%  * wgraph_mtx(WgMtx)
%    the matrix from which to extract the graph if one is not given by Wgraph
%  * w_threshold(Thres=1)
%    values below that are not ajusted for width
%  * useDingbats(DingB='TRUE')
%    when format is pdf, setting this to FALSE, turns the homonym R option for pdf() function
%
% Also see wgraph/2 options for saving the graph (save/1 and stem/1).
%
%==
% ?- G = [1-2:50,2-3:100], assert( wg(G) ),
% ?- wg(M), wgraph_plot(M,[]).
% 
% ?- M = [row(from,to,weight),row(1,2,50),row(2,3,100)], assert(wg0(M) ).
% ?- wg0(M), wgraph_plot(M,[]).
% 
% ?- G = [row(from,to,weight),row(1,2,50),row(2,3,100),row(4,'','')], assert(wg1(G) ).
% ?- wg1(G1), wgraph_plot(G1,true).
% 
% ?- G = [1-2:200,2-3:400,4], assert(wg1(G) ).
% ?- wg1(G1), wgraph_plot(G1,true).
% ?- wg1(G1), wgraph_plot(G1,orphan_edge_weight(0.1) ).
%==
% @author nicos angelopoulos
% @version  0.1 2014/11/21
% @version  0.2 2016/01/23
%
wgraph_plot( ArgS ) :-
	en_list( ArgS, Args ),
	Args = [Wgraph|Rest],
	wgraph_plot( Wgraph, Rest ).

wgraph_plot( WgraphIn, ArgS ) :-
	en_list( ArgS, Args ),
	wgraph( WgraphIn, Wgraph ),
	Self = wgraph_plot,
	options_append( Self, [graph(Wgraph)|Args], Opts, process(debug) ),
	debug( wgraph_plot, 'Options: ~w', [Opts] ),
	options( [x(Xs),y(Ys)], Opts ),
	length( Xs, Len ),
	lp_coords <- matrix( nrow=Len, ncol=2, 0 ),
	lp_coords[*,1] <- Xs,
	lp_coords[*,2] <- Ys,
	options( labels(Labels), Opts ),
	options( colours(Clr), Opts ),
	elemental_list_template( Clr, Labels, Clrs ),
	options( plotter(Plotter), Opts ),
	wgraph( _WgFile, Wgraph, Opts ),

	% wgraph_plot_save_layout
	options( label_distance(Ldist), Opts ),
	options( label_degree(Ldegree), Opts ),
	options( w(Ws), Opts ),
	options( edge_colours(EClrs), Opts ),
	debug( wgraph_plot, 'Node colours: ~w', [Clrs] ),
	wgraph_plotter( Plotter, Self, Ws, EClrs, Labels, Clrs, Ldist, Ldegree, Opts ),
	options( save(Save), Opts ),
	wgraph_layout_save( Save, Labels, Xs, Ys, Clrs, Ldist, Ldegree, Opts ),
	wgraph_graph_save( Save, Wgraph, Opts ).

% new 19.4.12
wgraph_plotter( ggnet2, Self, Ws, _Hus, Labels, Clrs, _Ldist, _Ldegr, Opts ) :-
    debug( Self, 'Labels: ~w', [Labels] ),
    debug( Self, 'Clrs: ~w', [Clrs] ),
	debug( Self, 'ggnet2 options: ~w', [Opts] ),
    memberchk( node_size(Nsz), Opts ),
    findall( Wxh, (member(Wx,Ws),Wxh is Wx / 2), Whs ),
    GGopts = [size=Nsz, label=Labels, color=Clrs, vjust= -1, 'edge.size'=Whs, 'edge.color'="#BEAED4"],
    append( Opts, GGopts, ROpts ),
    r_call( ggnet2(lp_adj), [rvar(pltv)|ROpts] ),
    options( format(Fmt), Opts ),
    ( Fmt == none ->
        <- print(pltv)
        ;
        ( Fmt == x11 -> 
            <- x11(),
            <- print(pltv)
            ;
            ( compound(Fmt) ->   % eg  x11(a,b,c)
                <- Fmt
                ;
                options( [width(Wi),height(Hi)], Opts ),
                options( stem(Stem), Opts ),
                file_name_extension( Stem, Fmt, File ),
                <- ggsave(file= +File, plot=pltv, width=Wi, height=Hi )
            )
        )
    ).

wgraph_plotter( qgraph, Self, _Ws, _Hus, Labels, Clrs, _Ldist, _Ldegr, Opts ) :-
	Qopts = [width(Width),height(Height),format(Form)],
	options( Qopts, Opts ),
	debug( wgraph_plot, 'Qgraph options: ~w', [Qopts] ),
	lp_option_output( Form, Width, Height, Opts, Self, OutputL ),
	( memberchk(node_size(VszOpt),Opts) ->
			lp_option_labels_vsize( VszOpt, Labels, Vsz ),
			QTail = [vsize=Vsz|OutputL]
			;
			QTail = OutputL
	),
	QgraphL = [layout=lp_coords,normalize='F',width=Width,colors=Clrs,labels=Labels|QTail],
	append( QgraphL, Opts, QgOpts ),
	r_call( qgraph(lp_adj), QgOpts ).

% fixme add tkplot & RBL
wgraph_plotter( igraph, Self, Ws, HclS, Labels, Clrs, Ldist, Ldegr, Opts ) :-
	% edge.width
	ilp <- 'graph.adjacency'(lp_adj, mode="undirected", weighted='TRUE' ),
	% findall( Arg=Val, member(Arg=Val,Opts), Pairs ),
	en_list( HclS, Hcls ),
	wgraph_igraph_hue_luminance_vectors( Hcls, 260, Hus, Lus ),
	Qopts = [width(Width),height(Height),format(Form)],
	options( Qopts, Opts ),
	lp_option_r_call_output( Form, Width, Height, Opts, Self, OutputL ),
	% 120 = Green, 260 = Blue
	maplist( hcl_colour, Hus, Lus, EClrS ),
	% r_call( plot.igraph(ilp), [edge.color=EClrs,edge.width=Ws,layout=lp_coords,vertex.color=Clrs,vertex.label=Labels,vertex.label.dist=Ldist,vertex.label.degree=Ldegr|Opts] ).
	% findall( Ropt=Rval, member(Ropt=Rval,Opts), RinOpts ),
	append( OutputL, Opts, Aopts ),
	en_list( EClrS, EClrs ),
	% maplist( en_plus, EnpClrs, EClrs ),
    Ropts = ['edge.color'=EClrs,'edge.width'=Ws,layout=lp_coords,'vertex.color'=Clrs,'vertex.label'=Labels,'vertex.label.dist'=Ldist,'vertex.label.degree'=Ldegr,'vertex.size'=Vsize|Aopts],
	debug( wgraph_plot, 'igraph R options: ~w', [Ropts] ),
	options( node_size(Vsize), Opts ),
	r_call( 'plot.igraph'(ilp),  Ropts ).
	% % r_call( plot.igraph(lp_coords[*,1],lp_coords[*,2]), [layout=lp_coords,vertex.color=Clrs,vertex.label=Labels,vertex.label.dist=Ldist|Opts] ).
	% <- Qgraph.

% EClrS <- hcl(Hus,l=Lus,c=100),
hcl_colour( Hu, Lu, Clr ) :-
	number( Hu ),
	!,
	Clr <- hcl( Hu, l=Lu, c=100 ).
hcl_colour( Hu, _Lu, Hu ).

wgraph_igraph_hue_luminance_vectors( [], _DefHue, [], [] ).
wgraph_igraph_hue_luminance_vectors( [H|T], DefHue, [Hu|Hus], [Lu|Lus] ) :-
	% ( atomic(H) -> Hu = DefHue, Lu = H ; H=(Hu-Lu) ),
	( H=(Hu-Lu) -> true ; Hu=DefHue, Lu=H ),
	wgraph_igraph_hue_luminance_vectors( T, DefHue, Hus, Lus ).

wgraph_plot_weights_widths( Wes, Wis, Hus, Opts ) :-
	options( max_width(MaxWi), Opts ),
	MaxWi == false,
	!,
	Wis = Wes,
	Hus = 50.
wgraph_plot_weights_widths( Wes, Wis, Hus, Opts ) :-
	Lims = [max_width(MaxWiM),max_weight(MaxWeM),min_width(MinWiM),min_weight(MinWeM)],
	options( w_threshold(Thres), Opts ),
	include( =<(Thres), Wes, ThreWes ),
	options( Lims, Opts ),
	maplist( wgraph_plot_limit_value(ThreWes), [MaxWiM,MaxWeM,MinWiM,MinWeM], [MaxWi,MaxWe,MinWi,MinWe] ),
	RangeWe is MaxWe - MinWe,
	wgraph_plot_weights_range_widths( RangeWe, MinWe, MaxWi, MinWi, Wes, Thres, Wis, Hus ).

wgraph_plot_weights_range_widths( 0, _MinWe, _MaxWi, _MinWi, Wes, _Thres, Wis, Hus ) :-
	!,
	Wis = Wes,
	Hus = 50.
wgraph_plot_weights_range_widths( RangeWe, MinWe, MaxWi, MinWi, Wes, Thres, Wis, Hus ) :-
	Factor is RangeWe / (MaxWi - MinWi),
	maplist( wgraph_plot_scale_widths(MinWi,MinWe,Factor,Thres,0.0000001), Wes, Wis ),
	MaxHu = 50, MinHu = 90,  % higher, means lighter colour, so we are reversing
	HuFact is (MaxWi - MinWi) / (MaxHu - MinHu ),
	maplist( wgraph_plot_scale_widths(MinHu,MinWi,HuFact,Thres,"white"), Wis, Hus ).

wgraph_plot_scale_widths( _MinWi, _MinWe, _Factor, Thres, Def, We, Wi ) :-
	We < Thres,
	!,
	Wi = Def.
wgraph_plot_scale_widths( MinWi, MinWe, Factor, _Thres, _Def, We, Wi ) :-
	Wi is MinWi + ((We - MinWe) / Factor ).

wgraph_plot_limit_value( Wes, Tkn, Val ) :-
	atom( Tkn ),
	!,
	atomic_concat( Tkn, '_list', Pname ),
	Goal =.. [Pname,Wes,Val],
	( call(Goal) -> true; Val is 1 ).
wgraph_plot_limit_value( _Wes, Val, Val ) :-
	number( Val ).

wgraph_plot_lp_adj( [], [], [], _Rvar ).
wgraph_plot_lp_adj( [F|Fs], [T|Ts], [W|Ws], Rvar ) :-
	Rvar[F,T] <- W,
	wgraph_plot_lp_adj( Fs, Ts, Ws, Rvar ).

lp_option_output( pdf, _W, _H, Opts, Self, Output ) :-
	lp_option_stem( Self, Stem, Opts ),
	Output = [filetype="pdf", filename=+Stem].
lp_option_output( ps, _W, _H, Opts, Self, Output ) :-
	lp_option_stem( Self, Stem, Opts ),
	Output = [filetype="postscript", filename=+Stem].
lp_option_output( x11, W, H, _Opts, _Self, [filetype=""] ) :-
	<- x11( width = W, height = H ).
lp_option_output( none, _W, _H, _Opts, _Self, [filetype=""] ).

lp_option_stem( Self, Stem, Opts ) :-
	( memberchk(stem(Stem),Opts) ->
		true
		;
		( (memberchk(layout_mtx(MtxF),Opts),\+ is_list(MtxF)) ->
			( (    file_name_extension(Stem,Ext,MtxF),
		      	 memberchk( Ext, [csv,tsv,tab,txt] )
			  ) ->
			  		true
					; 
					Stem = MtxF
			)
			;
			Stem = Self
		)
	).

lp_option_r_call_output( pdf, W, H, Opts, Self, Output ) :-
	lp_option_stem( Self, Stem, Opts ),
	/*
	( memberchk(stem(Stem),Opts) ->
		true
		;
		( (memberchk(layout_mtx(MtxF),Opts),\+ is_list(MtxF)) ->
			( (    file_name_extension(Stem,Ext,MtxF),
		      	 memberchk( Ext, [csv,tsv,tab,txt] )
			  ) ->
			  		true
					; 
					Stem = MtxF
			)
			;
			Stem = Self
		)
	),
	*/
    options( useDingbats(UseD), Opts ),
	Output = [outputs(pdf(width=W,height=H,'useDingbats'=UseD)), stem(Stem) ].
lp_option_r_call_output( ps, W, H, Opts, Self, Output ) :-
	lp_option_stem( Self, Stem, Opts ),
	Output = [outputs(postscript(fonts=c("serif"),width=W,height=H)), stem(Stem) ].

lp_option_r_call_output( x11, W, H, _Opts, _Self, Output ) :-
	Output = [outputs(x11(width=W,height=H))].
lp_option_r_call_output( none, _W, _H, _Opts, _Self, [] ).

lp_option_labels_vsize( prop(Min,Fact), Labels, Vsz ) :-
	!,
	maplist( label_name_proportional_vsize(Min,Fact), Labels, Vsz ).
lp_option_labels_vsize( Vsz, _Labels, Vsz ).

label_name_proportional_vsize( Min, Fact, Label, Vsz ) :-
    ( is_list(Label) -> length(Label,Len); functor(Label,_,Len) ),
	Vsz is Min + (Len * Fact).

wgraph_layout_save( false, _Labels, _Xs, _Ys, _Clrs, _Ldist, _Ldegr, _Opts ).
wgraph_layout_save( true, Labels, Xs, Ys, Clrs, Ldist, Ldegr, Opts ) :-
	options( stem(Stem), Opts ),
	atom_concat( Stem, '_layout', LayStem ),
	file_name_extension( LayStem, csv, File ),
	elemental_list_template( Ldist, Xs, LdistL ),
	elemental_list_template( Ldegr, Xs, LdegrL ),
	% ( is_list(Ldegr) -> LdegrL = Ldegr; findall(Ldegr,member(_,Xs),LdegrL) ),
	mtx_lists( File, [[label|Labels],[x|Xs],[y|Ys],[color|Clrs],[label_distance|LdistL],[label_degree|LdegrL]] ).

wgraph_graph_save( false, _Graph, _Opts ).
wgraph_graph_save( true, Graph, Opts ) :-
	options( stem(Stem), Opts ),
	atom_concat( Stem, '_graph', GStem ),
	file_name_extension( GStem, csv, File ),
	wgraph( File, Graph ).

% fixme: add options and put in separate file:
wgraph_add_edges_for_orphans( InW , Ow, Wgraph ) :-
	wgraph_undirected_vertex_edge_count( InW, [], Freq ),
	debug( wgraph_plot, 'Edge counts: ~w', [Freq] ),
	findall( Count, member(_Vertex-Count,Freq), Counts ),
	Med <- median( Counts ),
	findall( Orphan, member(Orphan-0,Freq), Orphans ),
	findall( NonOrphan-Count, (member(NonOrphan-Count,Freq),Count>0), NonOFreq ),
	wgraph_add_edges_for_orphans_counts( Orphans, NonOFreq, Med, NonOFreq, InW, Ow, Wgraph ).

wgraph_add_edges_for_orphans_counts( [], _Freq, _Med, _AllFreq, Wgraph, _Ow, Wgraph ).
wgraph_add_edges_for_orphans_counts( [O|Os], ThisFrq, Med, AllFrq, InW, Ow, Wgraph ) :-
	kv_v_over( ThisFrq, Med, AllFrq, [], RemFrq, From ),
	debug( _, 'InW: ~w', [InW] ),
	wgraph_add_edge_for_orphan( InW, O, From, Ow, NxtW ),
	debug( _, 'NxtW: ~w', [NxtW] ),
	wgraph_add_edges_for_orphans_counts( Os, RemFrq, Med, AllFrq, NxtW, Ow, Wgraph ).

wgraph_add_edge_for_orphan( Wgraph, Orphan, From, OrphanW, Ngraph ) :-
	once( select(Orphan,Wgraph,Rgraph) ),
	( From @< Orphan -> Edge = From-Orphan:OrphanW; Edge = Orphan-From:OrphanW ),
	ord_add_element( Rgraph, Edge, Ngraph ),
	!.
wgraph_add_edge_for_orphan( Wgraph, Orphan, From, _OrphanW, _Ngraph ) :-
	throw( could_not_fix_orphan(Orphan,From,Wgraph) ).

/*
wgraph_add_edge_for_orphan( [], Orphan, From, _Ow, _NxtW ) :-
	throw( fixme(not_orphan_to_connect_to(Orphan,From)) ).
wgraph_add_edge_for_orphan( [Orphan|T], Orphan, From, Ow, Wgraph ) :-
	( From @< Orphan -> Edge = From-Orphan:Ow; Edge = Orphan-From:Ow ),
	debug( wgraph_plot, 'Adding orphan edge: ~w', Edge ),
	Wgraph = [Edge|T],
	!.
wgraph_add_edge_for_orphan( [Edge|T], Orphan, From, Ow, [Edge|R] ) :-
	wgraph_add_edge_for_orphan( T, Orphan, From, Ow, R ).
	*/

kv_v_over( [], _Med, AllFrq, _Acc, RemFrq, From ) :-
	random_member( From-_Count, AllFrq ),
	RemFrq = AllFrq.
kv_v_over( [K-V|T], Med, _AllFrq, AccFrq, RemFrq, From ) :-
	V > Med,
	!,
	From = K,
	reverse( AccFrq, Left ),
	append( Left, T, RemFrq ).
kv_v_over( [K-V|T], Med, AllFrq, AccFrq, RemFrq, From ) :-
	kv_v_over( T, Med, AllFrq, [K-V|AccFrq], RemFrq, From ).

wgraph_undirected_vertex_edge_count( [], Freq, Freq ).
wgraph_undirected_vertex_edge_count( [Edge|T], Acc, Counts ) :-
	wgraph_undirected_edge_count( Edge, Acc, Nxt ),
	wgraph_undirected_vertex_edge_count( T, Nxt, Counts ).

wgraph_undirected_edge_count( From-To:_W, Acc, New ) :-
	!,
	wgraph_increase_edge_count( Acc, From, Nxt ),
	wgraph_increase_edge_count( Nxt, To, New ).
wgraph_undirected_edge_count( Orphan, Acc, Nxt ) :-
	atomic( Orphan ),
	( ord_member_chk(Acc,Orphan-_Count) ->
		Nxt = Acc
		;
		ord_add_element( Acc, Orphan-0, Nxt )
	).

wgraph_increase_edge_count( [], Vertex, [Vertex-1] ).
wgraph_increase_edge_count( [Vertex1-Count|T], Vertex2, Nxt ) :-
	compare( Op, Vertex1, Vertex2 ),
	wgraph_increase_edge_count_op( Op, Vertex1, Count, Vertex2, T, Nxt ).

wgraph_increase_edge_count_op( =, _Vertex1, Count, Vertex2, T, [Vertex2-Inc|T] ) :-
	Inc is Count + 1.
wgraph_increase_edge_count_op( <, Vertex1, Count, Vertex2, T, [Vertex1-Count|R] ) :-
	wgraph_increase_edge_count( T, Vertex2, R ).
wgraph_increase_edge_count_op( >, Vertex1, Count, Vertex2, T, [Vertex2-1,Vertex1-Count|T] ).

ord_member_chk( [Elem|_], Elem ) :- !.
ord_member_chk( [H|T], Elem ) :-
	H @< Elem,
	ord_member_chk( T, Elem ).

en_plus( Elem, +(Elem) ).

elemental_list_template( List, _Template, Elemental ) :-
	is_list( List ),
	!,
	Elemental = List.
elemental_list_template( Elem, Template, Elemental ) :-
	% \+ var( Elem ), % fixme: if you want to be steadfast
	!,
	findall( Elem, member(_,Template), Elemental ).
