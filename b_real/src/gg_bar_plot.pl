
:- lib(options).
:- use_module( library(real) ).
:- lib(promise(gg_plot_dep/0,call(gg_plot_dep_load))).


% now from stoics
:- lib(stoics_lib:positions/3).
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:compound/3).
:- lib(stoics_lib:kv_decompose/3).
:- lib(stoics_lib:kv_ks/2).

% local:
:- lib(head/2).
% :- requires( gg_hue_colour_strings/2 ).

:- lib(stoics_lib:kv_decompose/3).

gg_plot_dep_load :-
     lib(r("ggplot2")),
     assert(gg_plot_dep).

gg_bar_plot_defaults( ArgS, Defs ) :-
    ( is_list(ArgS) -> Args=ArgS; Args=[ArgS] ),
    DefFlip = false,
    ( memberchk(flip(Flip),Args) -> true; Flip = DefFlip ),
    ( Flip == true -> Rev = true; Rev = false ),
    Defs = [    
                df_rvar(gbp_df), df_rvar_rmv(true), 
                fill_colours(false),
                geom_bar(true), 
                geom_bar_draw_colour(white),
                geom_bar_position(dodge),
                gg_terms([]),
                flip(DefFlip), 
                keep_order(true),
                labels('','',''),
                legend_labels([]),
                legend_reverse(Rev),
                legend_title("colours"),
                level_colours(false),
                level_colours_title('Group'),  % fixme: doesnt like spaces currently
                output(false),
                panel_theme(standard),
                extra_legend(false),
                extra_legend_position(12,182,0,4)
           ].

/** gg_bar_plot( +Pairs, +Opts ).

Plot a bar plot of population Pairs (Name-Populations).

Opts is a combination of options controlling the predicate as per normal
Prolog convention, and term structures that translate to '+' ggplot2 terms.

Originally this only supported lists at the value part of Pairs. Now single 
values make the predicate plot a non-grouped barplot.

Opts
  * bar_draw_colour(Bdc=white) 
    draw colour for bars

  * debug(Dbg=false)
    _true_ turns debugging on

  * df_rvar(Df=gbp_df)
    data frame R variable

  * df_rvar_rmv(DfR=true)
    remove Df after call

  * extra_legend(XtLeg=false)    
    adds extra legend

  * extra_legend_position(Xn,Xx,Yn,Yx)
    position for extra legend

  * fill_colours(Clrs=true)
    _false_ removes scale_fill_manual() term, _true_ use def. colours, else give list of colours

  * flip(Flip=true) 
    flip coordinates

  * geom_bar(Gb=true)
    _false_ removes removes geom_bar() term
  
  * geom_bar_draw_colour(Gc=white)
    colour for bar outlines

  * geom_bar_position(Gp=dodge)
    or _stack_; positions bars

  * gg_terms([])
    arbitrary terms to add to the plot

  * labels(X,Y,Main)
    X='',Y='',Main='', if Flip==true then you should also swap X with Y here

  * legend_reverse(Lrv=true)
    reverse legend order (Lrv=false when Flip=false)

  * legend_title(Lt)
    legend title ([] for default ggplot2() one). only works for Clrs \== false

  * legend_labels(Ll=[])
    labels for legend (only valid if colours are given)

  * keep_order(Kord=true)
    display x axis according to given order (else, passes through sort)

  * output(Outp=false)
    else send a R command such as pdf("myfile.pdf")

  * panel_theme(PnlTheme=standard)
    theme for the whole panel (graph)- see gg_panel_theme/2

==
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, true ).

?- FClrs = ["gold1", "#E31A1C", "blue1"],
BoldTitle = theme(plot.title(element_text(face(+"bold")))),
Pairs = [a-[1,2,3],b-[2,4,6]],
gg_bar_plot( Pairs, [debug(true), geom_bar_draw_colour(black), labels(x,y,main), fill_colours(FClrs), gg_terms(BoldTitle),legend_title(leeg)] ).

?-  FClrs = ["gold1", "#E31A1C","blue1","darkolivegreen"], Pairs = [a-1,b-2,c-3,d-4], 
gg_bar_plot( Pairs, [flip(false),geom_bar(empty),fill_colours(FClrs)] ).

?- Pairs = [a-1,b-2,c-3,d-4],
   gg_bar_plot( Pairs, [flip(false),geom_bar(empty),fill_colours(true),df_rvar_rmv(false)] ).
==

@author nicos angelopoulos
@version  0.1 2014/10/21
@version  0.2 2016/01/23
@version  0.3 2016/08/31, added singleton groups as normal barplots
@version  0.4 2022/02/16, re-introduced dependency to ggpubr

*/
gg_bar_plot( Pairs, Args ) :-
    gg_plot_dep_load,
    options_append( gg_bar_plot, Args, Opts, process(debug) ),
    gg_bar_plot_base( Pairs, Df, Len, GGbase, Nest, Opts ),
    options( flip(Flip), Opts ),
    gg_bar_plot_flip_term( Flip, GGbase, GGflip ),
    options( legend_reverse(Lrv), Opts ),
    gg_bar_plot_leg_reverse( Lrv, GGflip, GGlrev ),
    GBs =[geom_bar_draw_colour(Gbc),geom_bar(Gbb),geom_bar_position(Gbp)],  
    options( GBs, Opts ),
    options( fill_colours(Fclrs), Opts ),
    debuc( gg_bar_plot, 'fill_colours(~w)', [Fclrs] ),
    options( legend_title(Ltitle), Opts ),
    options( legend_labels(LLbls), Opts ),
    gg_bar_plot_geom_bar( Nest, Gbb, Gbp, Gbc, Fclrs, GGlrev, GGgb ),
    debuc( gg_bar_plot, 'Legend labels: ~w', [LLbls] ),
    gg_bar_plot_fill_colours( Fclrs, Ltitle, Len, LLbls, GGgb, GGfill ),
    options( labels(Xlbl,Ylbl,Mlbl), Opts ),
    GGlbl = ( GGfill + labs( x=+Xlbl, y=+Ylbl, title=+Mlbl ) ),
    options( panel_theme(PnlTheme), Opts ),
    gg_panel_theme( PnlTheme, GGThemeL ),
    gg_bar_plot_opts( GGThemeL, GGlbl, GGtheme ),

    % GG = ( GGlbl + theme(plot.title=element_text(face=+bold) ) ),
    options( gg_terms(PlTermS), Opts ),
    en_list( PlTermS, PlTerms ),
    gg_bar_plot_opts( PlTerms, GGtheme, GG ),

    debuc( gg_bar_plot, 'GG bar plot term: ~w', [GG] ),
    % <- print( GG + theme( panel.background='element_blank()', panel.grid='element_blank()', axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black") ) ),
    options( [extra_legend(XtraLeg),extra_legend_position(Xn,Xx,Yn,Yx)], Opts ),  % experimental
    options( output(OutTerm), Opts ),
    gg_bar_plot_extra_legend_plots( XtraLeg, Xn, Xx, Yn, Yx, OutTerm, GG ),
    options( df_rvar_rmv(Rmv), Opts ),
    gg_bar_plot_rvar_remove( Rmv, Df ).

gg_bar_plot_extra_legend_plots( false, _, _, _, _, OutTerm, GG ) :-
    gg_bar_plot_open_dev( OutTerm ),
    <- print( GG ),
    gg_bar_plot_close_dev( OutTerm ).
gg_bar_plot_extra_legend_plots( bottom(Lbla,TopH,BotH,KVs), _Xn, _Xx, _Yn, _Yx, _OutTerm, GG ) :-
    !,
    % <- print( GG ),
    gg_to_string( Lbla, Lbl ),
    findall( I, nth1(I,KVs,_), Is ),
    findall( K, member(K-_V,KVs), Ks ),
    df   <- 'data.frame'( tmpx=Is, tmpy=Is, Lbl=Ks ),
    findall( V, (member(_K-Va,KVs),gg_to_string(Va,V)), Vs ),
         <- library("ggpubr"),     % ggarrange() + ggscatter()- was supposed to be removed in 0.4a but it is still there...
    gs   <- ggscatter(df, x="tmpx", y="tmpy", color=Lbl, palette = Vs, ggtheme = theme_minimal() + theme(legend.position="top")),
    gslt <- as_ggplot(get_legend(gs)),
    % ggp  <- as_ggplot(arrangeGrob(GG,gslt,heights=c(TopH,BotH))),
    ggp  <- ggarrange(GG, gslt, ncol = 1, nrow = 2, heights = c(TopH,BotH)),
         <- print(ggp),
    !.
gg_bar_plot_extra_legend_plots( [H|T], Xn, Xx, Yn, Yx, _OutTerm, GG ) :-
    % fixme: use OutTerm to redirect output
    % 2020.7.29: this no longer works: try the clause above...
    kv_decompose( [H|T], Labels, Clrs ),
    <- library( "gridExtra" ),   % arrangeGrob()
    <- library( "gtable" ),
    length( Clrs, Len ),
    CClrs =.. [c|Clrs],
    tmp_df <- 'data.frame'( x=1:Len, y=1:Len, 'X.label'=factor(Labels) ),
    tmp_plot1 <- ggplot( data = tmp_df, aes(x=x,y=y))
                 + geom_point( aes(colour='X.label') )
               + scale_colour_manual(values=CClrs)
               + theme('legend.position'="right"),
    tmp_leg1 <- gtable_filter(ggplot_gtable(ggplot_build(tmp_plot1)), "guide-box"),
    % fixme make the following x+ys in to options
    tmp_plot_new <- GG + annotation_custom( grob=tmp_leg1, xmin=Xn, xmax=Xx, ymin=Yn, ymax=Yx ),
    tmp_plot_new <- arrangeGrob( tmp_leg1, tmp_plot_new, 
                      heights = 'unit.c'( tmp_leg1$height,unit(1,"npc") - tmp_leg1$height), ncol=1 ),
    <- 'grid.newpage'(),
    <- 'grid.draw'(tmp_plot_new).

gg_bar_plot_open_dev( false ) :- !.
gg_bar_plot_open_dev( R ) :- 
    <- R.

gg_bar_plot_close_dev( false ) :- !.
gg_bar_plot_close_dev( _R ) :- 
    <- 'dev.off'().

/** gg_bar_plot_base( +Pairs, -Df, -Len, -GGbase, -Nest, +Opts )

Nest returned as true iff the pairs hald multi-values. 

*/
gg_bar_plot_base( Pairs, Df, Len, GGbase, Nest, Opts ) :-
    head( Pairs, _-FstPops ),
    ( is_list(FstPops) ; compound(FstPops) ),
    !,
    Nest = true,
    length( FstPops, Len ),
    positions( FstPops, Dtype, Poss ),
    maplist( gg_bar_plot_kvs(Dtype,Poss), Pairs, DataKVs ),
    flatten( DataKVs, KVs ),
    kv_decompose( KVs, ClustPosPs, Pops ),
    kv_decompose( ClustPosPs, Clusts, Posits ),
    options( df_rvar(Df), Opts ),
    Df <- 'data.frame'( pop=Pops, clu=Clusts, pos=Posits  ),
    options( keep_order(Kord), Opts ),
    gg_bar_plot_base_x_order( Kord, Pairs, Df ),
    flatten( DataKVs, KVs ),
    kv_decompose( KVs, ClustPosPs, Pops ),
    kv_decompose( ClustPosPs, Clusts, Posits ),
    % GGbase =  ggplot(Df, drop='T', aes(x=as.factor(clu), y=pop, fill=as.factor(pos), levels=Ks)).
    % % GGbase =    ggplot(Df, drop='T', aes(x=as.factor(clu), y=pop, fill=as.factor(pos))).
    GGbase =    ggplot(Df, aes(x=as.factor(clu), y=pop, fill=as.factor(pos))).
gg_bar_plot_base( Pairs, Df, Len, GGbase, false, Opts ) :-
    % added 16.08.31: when FstPops is not a list or compound, revert to a simple barplot...
    % add option for multi colour or single ?
    % kv_decompose( Pairs, Vals, Pops ),
    length( Pairs, Len),
    % numlist( 1, Len, Poss ),
    options( df_rvar(Df), Opts ),
    % Df <- data.frame( pop=Pops, pos=Poss, vals=Vals ),
    findall( Occ, (member(Occ-Times,Pairs), between(1,Times,_)), Occs ),
    % testing: 
    options( level_colours(LvlClrs), Opts ),
    findall( Lvl, member(Lvl-_,Pairs), Lvls ),
    gg_bar_plot_base_level_colours( LvlClrs, Occs, Lvls, Pairs, Df, GGbase, Opts ).

gg_bar_plot_base_level_colours( false, Occs, Lvls, _Pairs, Df, GGbase, _Opts ) :-
    !,
    Df <- data.frame( occ = factor( Occs, levels= Lvls ) ),
    GGbase = ggplot( Df, aes(factor(occ),fill=occ) ).
gg_bar_plot_base_level_colours( LvlClrs, Occs, Lvls, Pairs, Df, GGbase, Opts ) :-
    /*
    length( Lvls, Len ),
    % findall( Times, member(_-Times,Pairs), TimeL ),
    % sumlist( TimeL, Sum ),
    Mid is Len / 2,
    findall( LftTms, (nth1(Nid,Pairs,_-LftTms),Nid =< Mid), Lefts ),
    sumlist( Lefts, Lim ),
    % Mid is Sum / 2,
    gg_bar_plot_progressive_colours( Pairs, 0, Lim, Clrs ),
    */
    options( level_colours_title(ClrsTitle), Opts ),
    findall( Clr, (member(Lvl-Tms,Pairs),memberchk(Lvl-Clr,LvlClrs),between(1,Tms,_)), Clrs ),
    Df <- 'data.frame'( occ = factor( Occs, levels= Lvls ), ClrsTitle=Clrs ),
    % here: options( keep_order(Kord), Opts ),
    % GGbase = ggplot( Df, aes(factor(occ), fill=as.factor(ClrsLbl)) ).
    GGbase = ggplot( Df, aes(factor(occ), fill=ClrsTitle) ).

/*
gg_bar_plot_progressive_colours( [], _Acc, _Mid, [] ).
gg_bar_plot_progressive_colours( [_Occ-Times|T], Acc, Mid, Clrs ) :-
    integer( Times ),
    0 =< Times,
    gg_bar_plot_progressive_colours_times( Times, Acc, Mid, Nxt, Clrs, TClrs ),
    gg_bar_plot_progressive_colours( T, Nxt, Mid, TClrs ).
    */

 gg_bar_plot_progressive_colours_times( 0, Acc, _Mid, Nxt, Clrs, TClrs ) :-
    !,
    Acc = Nxt,
    Clrs= TClrs.
 gg_bar_plot_progressive_colours_times( I, Acc, Mid, End, [Clr|Clrs], TClrs ) :-
    Nxt is Acc + 1,
    (Nxt =< Mid -> Clr is 1; Clr is 2),
    H is I - 1,
    gg_bar_plot_progressive_colours_times( H, Nxt, Mid, End, Clrs, TClrs ).

gg_bar_plot_base_x_order( false, _Pairs, _Df ).
gg_bar_plot_base_x_order( true, Pairs, Df ) :-
    kv_ks( Pairs, Ks ),
    Df$clu <- factor( Df$clu, levels=Ks ).

gg_bar_plot_kvs( list, _Poss, Clust-Pops, DataKVs ) :-
    findall( Clust-Pos-Pop, nth1(Pos,Pops,Pop), DataKVs ).
gg_bar_plot_kvs( compound, _Poss, Clust-Pops, DataKVs ) :-
    !, 
    functor( Pops, _, Arity ),
    findall( Clust-Pos-Pop, (between(1,Arity,Pos),arg(Pos,Pops,Pop)), DataKVs ).

gg_bar_plot_leg_reverse( true, GG, (GG + guides(fill = guide_legend(reverse='TRUE'))) ).
gg_bar_plot_leg_reverse( false, GG, GG ).

gg_bar_plot_flip_term( true, GG, (GG + 'coord_flip()') ).
gg_bar_plot_flip_term( false, GG, GG ).

gg_bar_plot_geom_bar( true, Bar, Pos, Clr, _FClrs, GG, GGGeom ) :-
    gg_bar_plot_geom_bar( Bar, Pos, Clr, GG, GGGeom ).
gg_bar_plot_geom_bar( false, _Bar, _Pos, Clr, FClrs, GG, (GG+Geom) ) :-
    ( compound(FClrs) ->
        Geom = geom_bar(colour=+Clr, fill=FClrs )
        ;
        Geom = geom_bar(colour=+Clr)
    ).

gg_bar_plot_geom_bar( fill, _Pos, Clr, GG, (GG+Geom) ) :-
    % % Geom = geom_bar( position="fill", drop='F', stat=+identity, color=+Clr ).
    Geom = geom_bar( position="fill", stat=+identity, color=+Clr ).
gg_bar_plot_geom_bar( true, Pos, Clr, GG, (GG+Geom) ) :-
    % fixme: Clr = false, then remove +Clr
    % % Geom = geom_bar( position=+Pos, drop='F', stat=+identity, color=+Clr ).
    Geom = geom_bar( position=+Pos, stat=+identity, color=+Clr ).
    % Geom = geom_bar( position=+Pos, stat=+identity ).
gg_bar_plot_geom_bar( false, _Pos, _Clr, GG, GG ).
gg_bar_plot_geom_bar( empty, _Pos, _Clr, GG, (GG+geom_bar()) ).

% gg_bar_plot_fill_colours( true, GG, GG ).
% gg_bar_plot_fill_colours( true, Title, Len, GG, (GG + scale_fill_manual(+Title,values=Vals)) ) :-
gg_bar_plot_fill_colours( CTerm, Title, _Len, Lbls, GG, (GG + scale_fill_manual(+Title,values=Vals,labels=FullLbls)) ) :-
    compound( CTerm ),
    functor( CTerm, c, Arity ),
    !,
    CTerm = Vals,
    ( Lbls == [] -> numlist( 1, Arity, FullLblsL ); Lbls = FullLblsL ),
    maplist( atom_string, FullLblsL, FullLblsS ),
    FullLbls =.. [c|FullLblsS].
% gg_bar_plot_fill_colours( true, Title, _Len, Lbls, GG, (GG + scale_fill_discrete(+Title,positions=Lbls)) ).
% gg_bar_plot_fill_colours( true, Title, Len, InLbls, GG, (GG + scale_fill_discrete(+Title,values=Clrs,labels=Lbls)) ) :-
    % gg_hue_colour_strings( 30, Clrs ),
gg_bar_plot_fill_colours( true, Title, _Len, _InLbls, GG, (GG + scale_fill_discrete(name= +Title)) ).
gg_bar_plot_fill_colours( false, _Title, _Len, _Lbls, GG, GG ).
gg_bar_plot_fill_colours( [H|T], Title, _Len, Lbls, GG, (GG + scale_fill_manual(+Title,values=Vals,labels=FullLbls)) ) :-
    Vals =.. [c,H|T],
    ( Lbls == [] -> length( [H|T], Len ), numlist( 1, Len, FullLblsL ); Lbls = FullLblsL ),
    maplist( atom_string, FullLblsL, FullLblsS ),
    FullLbls =.. [c|FullLblsS].

gg_bar_plot_rvar_remove( false, _Df ).
gg_bar_plot_rvar_remove( true, Df ) :-
    <- remove( Df ).

gg_bar_plot_opts( [], GG, GG ).
gg_bar_plot_opts( [H|T], GGbase, GGfull ) :-
    gg_bar_plot_opt( H, Hgg ),
    gg_bar_plot_opts( T, (GGbase + Hgg), GGfull ).

gg_bar_plot_opt( H, Hgg ) :-
    compound( H, Name, Args ),
    !,
    maplist( gg_bar_plot_gg_arg, Args, GGArgs ),
    compound( Hgg, Name, GGArgs ).

gg_bar_plot_opt( Pla, Gga ) :-
    ( \+compound(Pla); is_list(Pla); Pla = (+ _); compound(Pla,c,_) ),
    !,
    Gga = Pla.
gg_bar_plot_opt( Pla, Gga ) :-
    compound( Pla, Name, Args ),
    maplist( gg_bar_plot_gg_arg, Args, GGArgs ),
    compound( Gga, Name, GGArgs ).

gg_bar_plot_gg_arg( Pla, Gga ) :-
    ( \+compound(Pla); is_list(Pla); Pla = (+ _); compound(Pla,c,_) ),
    !,
    Gga = Pla.
gg_bar_plot_gg_arg( Pla, Gga ) :-
    compound( Pla, Name, [Arg] ),
    !,
    gg_bar_plot_opt( Arg, Sub ),
    Gga = ( Name = Sub ).
gg_bar_plot_gg_arg( Name = Value, (Name = ValArg) ) :-
    !,
    gg_bar_plot_gg_arg_val( Value, ValArg ).
gg_bar_plot_gg_arg( Pla, Gga ) :-
    gg_bar_plot_opt( Pla, Gga ).

gg_bar_plot_gg_arg_val( Value, ValArg ) :- 
    compound( Value, Name, Args ),
    !,
    maplist( gg_bar_plot_gg_arg, Args, GGArgs ),
    ValArg =.. [Name|GGArgs].
gg_bar_plot_gg_arg_val( Value, Value ).

pl_terms_gg_pairs( Arg, Pair ) :-
    compound( Arg, Name, [SubArg] ),
    ( \+ compound( SubArg ) ; is_list( SubArg ) ),
    !,
    Pair = (Name = SubArg).
pl_terms_gg_pairs( Arg, Pair ) :-
    compound( Arg, Name, Args ),
    maplist( pl_terms_gg_pairs, Args, Pairs ),
    Pair = (Name=Pairs). %

gg_to_string( Either, String ) :-
    ( atomic(Either) -> 
        atom_string(Either,String)
        ; 
        Either = String
    ).
