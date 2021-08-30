
:- use_module(library(lib)).
:- use_module(library(sgml)).
:- use_module(library(lists)).

:- lib(mlu).
:- lib(mtx).
:- lib(real).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib).

pack_dnloads_defaults( Defs ) :-
     % absolute_file_name( user_profile(pack_dnloads_tracker.csv), TrackerF, [] ),
     TrackerF = user_profile('pack_dnloads_tracker.csv'),
     Defs = [debug(true), mode(current), tracker(TrackerF)].
              % pack(bims), pack(b_real), pack(bio_analytics)

/** pack_dnloads( Opts ).

Plot the downloads of a bunch of packs.

Opts
 * debug(Dbg=true)
   see options_append/4
 * mode(Mode=current)
   one of _current_, _update_ and _tracked_. See Modes below
 * pack(Pack)
   multiple can be given. 
 * tracker(TrackerF=user_profile(pack_dnloads_tracker.csv))
   Tracking file- passed through absolute_file_name/2.

Modes
 * _current_
   plots static bars for each provided pack
 * _update_
   adds a new line to TrackerF which for current date gives frequencies of tracked packs
 * _tracked_
   plots contents of TrackerF
     
==
% pupsh pack_dnloads pack=by_unix pack=bims pack=prosqlite pack=real
% pupsh pack_dnloads.pl mode=update tracker=user_profile=pack_dnloads_daily.csv

==

@author nicos angelopoulos
@version  0:1 2021/08/25
@tbd add to lib(lib) catch failure of the ???

*/
pack_dnloads( Args ) :-
     Self = pack_dnloads,
     options_append( Self, Args, Opts ),
     options( mode(Mode), Opts ),
     pack_dnloads_opts( Mode, Self, Opts ).

pack_dnloads_opts( current, Self, Opts ) :-
     !,
     findall( Pack, member(pack(Pack),Opts), Packs ),
     pack_freqs( Packs, Self, Pairs ),
     GGt = theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
     mlu_frequency_plot( Pairs, gg_terms(GGt) ),
     date_two_digit_dotted( Dotted ),
     at_con( [Dotted,'pplot.pdf'], -, PlotF ),
     <- ggsave( +PlotF ),
     sleep( 10 ),
     debuc( Self, stop, true ).

pack_dnloads_opts( update, Self, Opts ) :-
     !,
     pack_dnloads_tracker_file( TrackF, Self, Opts ),
     findall( Pack, member(pack(Pack),Opts), Packs ),
     ( exists_file(TrackF) ->
          mtx( TrackF, Mtx ),
          pack_dnloads_pack_columns( Mtx, Packs, Ftx, Facks )
          ;
          Facks = Packs,
          Hdr =.. [hdr,''|Facks],
          Ftx = [Hdr]
     ),
     pack_freqs( Facks, Self, Fairs ),
     kv_decompose( Fairs, _AllPacks, Freqs ),
     date_two_digit_dotted( Dotted ),
     New =.. [row,Dotted|Freqs],
     once( append(Gtx,[Last],Ftx) ),
     ( arg(1,Last,Dotted) ->
          Atx = Gtx,
          debuc( Self, 'Current date existed: ~w, so replacing it.', [Dotted] )
          ;
          Atx = Ftx
     ),
     append( Atx, [New], Ttx ),
     mtx( TrackF, Ttx ).

pack_dnloads_opts( tracked, Self, Opts ) :-
     pack_dnloads_tracker_file( TrackF, Self, Opts ),
     findall( Pack, member(pack(Pack),Opts), Packs ),
     debuc( Self, 'Packs: ~w', [Packs] ),
     Xlbl = pack, Ylbl = date, Vlbl = dnloads, Clbl=dnpack,
     Df = gdf,
     mtx_r_df( TrackF, Xlbl, Ylbl, Vlbl, Clbl, Df ),
     % nv <- as.character(1:29),
     % nv <- cbind(nv,nv),
     % <- print(length(nv)),
     % <- nv,
     % trace,
     p <- ggplot(gdf, aes(x=Ylbl, y=Vlbl, group=Xlbl)) + geom_line(aes(color=Clbl))
                        % + geom_point(aes(color=Xlbl,shape=Xlbl)) 
                        + geom_point(aes(color=Clbl)) 
                    + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
     <- print(p),
     date_two_digit_dotted( Dotted ),
     at_con( [Dotted,'tracked_pplot.pdf'], -, PlotF ),
     <- ggsave( +PlotF ).

pack_freqs( Packs, Self, Pairs ) :-
     load_html(  'http://eu.swi-prolog.org/pack/list', Content, [] ),
     Content = [element(html,_,Html)],
     memberchk( element(body,_,Body), Html ),
     Body = [element(div,_,Outer)],
     memberchk( element(div,[class='inner-contents pack'], [element(_,_,Inner)] ), Outer ),
     memberchk( element(table,[class=packlist],PackList), Inner ),
     ( Packs == [] -> throw(no_packs_given); true ),
     findall( Pack-Tms, (member(Pack,Packs),
                         member(element(tr,_,TR), PackList),
                         member(element(td,[],[element(a,_,[Pack])]), TR),
                         member(element(td,[class='pack-downloads'],[Dnloads|_]),TR),
                         atom_number(Dnloads,Tms)
                        ),
                              Pairs ),
     debuc( Self, 'Frequencies: ~w', [Pairs] ).

pack_dnloads_tracker_file( TrackF, Self, Opts ) :-
     options( tracker(OptF), Opts ),
     absolute_file_name( OptF, TrackF ),
     debuc( Self, 'Using tracker file: ~p', [TrackF] ).

pack_dnloads_pack_columns( Mtx, Packs, Ftx, Facks ) :-
     mtx_lists( Mtx, MLs ),
     MLs = [Cdr|Mists],
     findall( Mack, member([Mack|_], Mists), Macks ),
     append( Macks, Packs, Aacks ),
     sort( Aacks, Facks ),
     length( Cdr, LenCdr ),
     NofDates is LenCdr - 1,
     findall( 0, between(1,NofDates,_), Zeros ),
     findall( Fist, (  member(Pack,Facks),
                       ( member([Pack|Pdata],Mists) ->
                              Fist = [Pack|Pdata]
                              ;
                              Fist = [Pack|Zeros]
                       )
                    ),
                         Fists ),
     mtx_lists( Ftx, [Cdr|Fists] ).

% 
mtx_r_df( MtxIn, Xlbl, Ylbl, Vlbl, Clbl, Df ) :-
     mtx( MtxIn, [Hdr|Rows] ),
     % maplist( arg(1), Rows, Ys ),
     Hdr =.. [_,_|Xs],
     % set up the colours according to the last row, which we assume is latest date.
     % 
     once( append(_,[LastRow],Rows) ),
     LastRow =.. [_,_|Lals],
     findall( Y-X-V-C, ( member(Row,Rows),
                       Row =.. [_,Y|Vals],
                       nth1(N,Xs,X),
                       nth1(N,Vals,V),
                       nth1(N,Lals,Lv),
                       n_digits_min(3,Lv,PadV),
                       at_con([PadV,X],'.',C)
                    ),
                         Qrips ),
     kv_decompose( Qrips, Trips, CElems ),
     kv_decompose( Trips, Pairs, VElems ),
     kv_decompose( Pairs, YElems, XElems ),
     write( xelems(XElems) ), nl,
     write( yelems(YElems) ), nl,
     write( velems(VElems) ), nl,
     write( celems(CElems) ), nl,
     ( var(Df) -> 
          Df = [Xlbl=XElems,Ylbl=YElems,Vlbl=VElems,Clbl=CElems]
          ;
          atomic(Df),
          Df <- [Xlbl=XElems,Ylbl=YElems,Vlbl=VElems,Clbl=CElems],
          Df <- data.frame(sapply(Df,c)),
          Df$Vlbl <- as.integer(Df$Vlbl)
     ).
