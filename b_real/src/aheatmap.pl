
:- lib( suggests(r('NMF')), suggests_warns(false) ).

:- lib( stoics_lib:kv_decompose/3 ).
:- lib( stoics_lib:locate/3 ).

:- lib( palette_n_colours/3 ).
:- lib( heatmap_breaks/4 ).
:- lib( head/2 ).
:- lib( term_args/2 ).

aheatmap_defaults( [  rvar(hm_data),
                      % hp_token(pastel_ryg),
                      hp_token(brewer_ryb_rev),
                      scale(false),
				  stem(none),
				  % centre(1.0)
				  rowV(true),colV(true),
				  centre(false),
				  breakpoints(14),
				  cellwidth(8),
				  cellheight(32)
			   ]
			 ).

%% aheatmap( +Csv ).
%% aheatmap( +Csv, +Opts ).
% 
%  Generic aheatmap plots with aheatmap.2 from NMF. Csv can be 
%  a mtx/1 recognised matrix or an R variable.
% 
%  note that standard version of aaheatmap() has a bug 
%  when breaks and colours are given: https://github.com/renozao/NMF/issues/12
%  2014/08/12
%
%  Opts 
%    * centre(Centre=false)
%      1.0 or 0.0, or non number for not centering
%    * cellheight(32)
%      height of the cell
%    * cellwidth(8)
%      width of the cell
%    * rvar(hm_data)
%    the R variable on which to load the data (if Csv is not an R var itself
%    * hp_token(pastel_ryg)
%    * hmap([])
%      aheatmap() call additional parameters given in Name(Value) syntax
%    * scale(true)
%      should we scale ?
%    * rowV(RowV=true)
%      do not use a rows dendrogram 
%    * colV(ColV=true)
%      do not use a columns dendrogram 
% ==  
% 
% ?- Mpg <- mtcars$mpg, aheatmap( Mpg, [rowV(false),colV(false)]  ).
% ?- Mpg <- mtcars$mpg, x <- [Mpg], colnames(x) <- rownames(mtcars), 
%     aheatmap( x, [cellheight=32,scale(false),rowV(false)]  ).
% == 
%
% @author nicos angelopoulos
% @version  0.1 2014/5/18
%
/*
?- 
   Mtc <- as.list(mtcars), memberchk(hp=HP,Mtc), memberchk(disp=Disp,Mtc),
   x <- [HP, Disp], rownames(x) <- c("horsepower","displacement"),
   colnames(x) <- rownames(mtcars),
   aheatmap(x).

*/
aheatmap( Csv ) :-
	aheatmap( Csv, [] ).

aheatmap( Rv, Opts ) :-
	options_append( aheatmap, Opts, All ),
	aheatmap_opts( Rv, All ).

aheatmap_opts( Rv, Opts ) :-
	r_is_var( Rv ),
	!,
	rvar_aheatmap( Rv, Opts ).
aheatmap_opts( CsvF, Opts ) :-
	\+ head( CsvF, _ ),
	locate( CsvF, [csv], CsvX ),
	!,
	memberchk( rvar(Rv), Opts ),
	Rv <- 'read.csv'( +CsvX, row.names=1, as.is='TRUE' ), % fixme: add to option
	%% Rv <- Rv[*,1:6], %% fixme: add option?
	% Rv <- as.numeric( Rv ),
	rvar_aheatmap( Rv, Opts ).
% fixme add CsvF = Data
aheatmap_opts( TermData, Opts ) :-
	head( TermData, List ),
	compound( List ),
	!,
	maplist( term_args, TermData, [[_|Hdr]|RowsData] ),
	memberchk( rvar(Rv), Opts ),
	findall( RowName-Row, member([RowName|Row],RowsData), RRs ),
	kv_decompose( RRs, RowNames, Data ),
	% maplist( writeln, Data ),
	Rv <- Data,
	colnames(Rv) <- Hdr,
	rownames(Rv) <- RowNames,
	rvar_aheatmap( Rv, Opts ).
aheatmap_opts( Data, Opts ) :-
	head( Data, List ),
	( head( List, _ ) ; List = (_=_) ),
	!,
	memberchk( rvar(Rv), Opts ),
	Rv <- Data,
	rvar_aheatmap( Rv, Opts ).
aheatmap_opts( Data, Opts ) :-
	head( Data, _List ),
	!,
	memberchk( rvar(Rv), Opts ),
	Rv <- [Data],
	% Rv <- as.matrix(t(Rv)),
	rvar_aheatmap( Rv, Opts ).
aheatmap_opts( Data, _Opts ) :-
	throw( cannt_figure_out_the_data_format_of(Data) ).

rvar_aheatmap( Rv, Opts ) :-
	memberchk( scale(Scale), Opts ),
	rvar_aheatmap_scale( Scale, Rv, _Centre ),
	options( [centre(Centre),breakpoints(Bps)], Opts ),
	heatmap_breaks( Centre, Bps, Rv, BsOpt ),
	options( hp_token(HmTkn), Opts ),
	HmPalV = hm_palette,
	aheatmap_palette( HmTkn, HmPalV, HmPal ),
	options( [rowV(RowV),colV(ColV)], Opts ),
	maplist( aheatmap_bool, [RowV,ColV], [Rowv,Colv] ),
	options( [cellheight(CellH),cellwidth(CellW)], Opts ),
	HmapBase = aheatmap(Rv,'Colv'=Colv,'Rowv'=Rowv,col=HmPal,
	                        cellwidth=CellW,cellheight=CellH),
	flatten( [BsOpt|Opts], HmapOpts ),
	r_call( HmapBase, HmapOpts ).

rvar_aheatmap_scale( true, Rv, 0 ) :-
	Rv <- scale( Rv ).
rvar_aheatmap_scale( log2, Rv, 0 ) :-
	CcnmAtmS <- colnames(Rv),
	RcnmAtmS <- rownames(Rv),
	Pv <- Rv,
	maplist( log2_vect_sf, Pv, Log2s ),
	Rv <- Log2s,
	en_list( CcnmAtmS, CcnmAtms ),
	en_list( RcnmAtmS, RcnmAtms ),
	maplist( term_string, CcnmAtms, Ccnms ),
	maplist( term_string, RcnmAtms, Rcnms ),
	colnames(Rv) <- Ccnms,
	rownames(Rv) <- Rcnms.
rvar_aheatmap_scale( false, Rv, Mean ) :-
	Mean <- mean(Rv).

log2_vect_sf( X, LgX ) :-
	debug( aheatmap, 'Logging: ~w', [X] ),
	maplist( log2_sf, X, LgX ).
	
log2_sf( X, LgX ) :-
	LgX <- log2(X).

aheatmap_bool( true, 'TRUE' ) :- !.
aheatmap_bool( false, 'NA' ) :- !.
aheatmap_bool( X, X ).

aheatmap_palette( [H|T], _, [H|T] ) :- !.
aheatmap_palette( red_blue, _, redblue(10) ).
aheatmap_palette( brewer_yor, HmPlt, HmPlt ) :-
	HmPlt <- brewer.pal(30,"YlOrRd").
	% mypalette <-
	% mypalette<-brewer.pal(11,"RdYlBu"), % not that great
aheatmap_palette( brewer_ryb, HmPlt, HmPlt ) :-
	HmPlt <- rev(brewer.pal(11,"RdYlBu")).
aheatmap_palette( brewer_ryb_rev, HmPlt, HmPlt ) :-
	HmPlt <- rev(brewer.pal(11,"RdYlBu")).
aheatmap_palette( yel_red_adj, HmPlt, HmPlt ) :-
	HmPlt <- 'colorRampPalette(c("#FFFFCC", "#FEB24C", "#E31A1C" ))(30)'.
aheatmap_palette( heat, HmPlt, HmPlt ) :-
	% HmPlt <- 'heat.colors'.
 	Ramp = ramp( "green", "black", "red"),
	palette_n_colours( Ramp, 31, RampPal ),
	HmPlt <- RampPal.
aheatmap_palette( pastel_ryg, HmPlt, HmPlt ) :-
 	Ramp = ramp( "#FC8D59", "#FFFFBF", "#91CF60"),
	palette_n_colours( Ramp, 200, RampPal ),
	HmPlt <- RampPal.
