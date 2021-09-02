
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
     Defs = [  
               append_profile(true),
               debug(true), 
               mode(current), 
               plot_all(true),
               stem_tracked(false),
               stem(pack_downloads),
               tracker(user_app_data('stoics/pack_dnloads/pack_dnloads_tracker.csv'))
               ].

/** pack_dnloads( Opts ).

Plot and track the number of downloads of a bunch of packs.

The script has three main functionalities with regard to a number of target packs:
(a) barplot the current number of downloads, (b) add the number of current downloads
to a dated tracker file, and (c) plot the information in tracker files.


Opts
  * append_profile(AppP=true)
     set to _false_ if you do not want user_profile(stoics/options/pack_dnloads.pl) to provide 
     extra options (see options_append/4)
  * debug(Dbg=true)
     see options_append/4
  * mode(Mode=current)
     one of _current_, _update_ and _tracked_. See section Modes below.
  * pack(Pack)
     multiple can be given
  * plot_all(Pall=true)
     in Mode=tracked, plots all packs in tracker. When Pall=false, only plot those in pack/1 options (and profile depending on AppP).
  * stem_tracked(StemTrc=false)
     stem for when Mode=tracked, by default (StemTrc=false), stem is taken by basename of TrackerF
  * stem(Stem=pack_dnloads)
     stem to use for output file when Mode=current
  * tracker(TrackerF=user_app_data('stoics/pack_dnloads/pack_dnloads_tracker.sv'))
     Tracking file- passed through absolute_file_name/2

Modes
  * _current_
     plots static bars for each provided pack
  * _update_
     adds a new line to TrackerF which for current date gives frequencies of tracked packs
  * _tracked_
     plots contents of TrackerF
     
==
% Plot the current number of downloads for packs: bims, prosqlite and real

?- [pack('b_real/scripts/pack_dnloads')].
?- pack_dnloads([pack(bims),pack(prosqlite),pack(real)]).
% produces file: 21.09.01-pack_downloads.pdf

% The line below is equivalent to the call above, but ran from linux command line via pack(upsh)
> upsh pack_dnloads pack=bims pack=prosqlite pack=real

% You can put your packs of interest (and other options) to file user_profile(stoics/options/pack_dnloads.pl)
%  - see options_append/4
% eg file:
pack(bims).
pack(prosqlite).
pack(real).

% then the first example can be reduced to:
?- pack_dnloads([]).

% The following ignores any info in user_profile(stoics/options/pack_dnloads.pl)
?-  pack_dnloads([pack(bims),pack(prosqlite),pack(stoics_lib),append_profile(false)]).

==

Update mode

==
?- pack_dnloads(mode(update)).
% Using tracker file: '/home/nicos/.local/share/swi-prolog/stoics/pack_dnloads/pack_dnloads_tracker.csv'
% Frequencies: [b_real-35,bims-63,bio_analytics-17,bio_db-77,bio_db_repo-48,by_unix-69,chess_db-34,db_facts-192,debug_call-49,disp_bn-2,gbn-3,lib-33,mlu-61,mtx-41,options-86,os_lib-78,pack_errors-80,pepl-25,pfd_meta-11,prosqlite-655,pub_graph-12,pubmed-35,r_session-16,real-527,spuds-32,stoics_lib-69,svg-3,upsh-21,wgraph-64]
true.

?- pack_dnloads(mode(update)).
% Using tracker file: '/home/nicos/.local/share/swi-prolog/stoics/pack_dnloads/pack_dnloads_tracker.csv'
% Frequencies: [b_real-35,bims-63,bio_analytics-17,bio_db-77,bio_db_repo-48,by_unix-69,chess_db-34,db_facts-192,debug_call-49,disp_bn-2,gbn-3,lib-33,mlu-61,mtx-41,options-86,os_lib-78,pack_errors-80,pepl-25,pfd_meta-11,prosqlite-655,pub_graph-12,pubmed-35,r_session-16,real-527,spuds-32,stoics_lib-69,svg-3,upsh-21,wgraph-64]
% Current date existed: 21.09.01, so replacing it.
true.

> cat /home/nicos/.local/share/swi-prolog/stoics/pack_dnloads/pack_dnloads_tracker.csv
,b_real,bims,bio_analytics,bio_db,bio_db_repo,by_unix,chess_db,db_facts,debug_call,disp_bn,gbn,lib,mlu,mtx,options,os_lib,pack_errors,pepl,pfd_meta,prosqlite,pub_graph,pubmed,r_session,real,spuds,stoics_lib,svg,upsh,wgraph
21.09.01,35,63,17,77,48,69,34,192,49,2,3,33,61,41,86,78,80,25,11,655,12,35,16,527,32,69,3,21,64

==

You can keep a number of tracker files
==
> upsh pack_dnloads.pl mode=update tracker=user_app_data=stoics/pack_dnloads/pack_dnloads_daily.csv
==

These can be kept updated via cron jobs. An example bash script can be found at b_real/scripts/pack_dnloads/pack_dnloads.sh

Finally, tracked mode produces a plot (on screen and saved on file), from a tracker file. 
Assuming your tracker file has more than one tracking line, eg:
==

> cat .local/share/swi-prolog/stoics/pack_dnloads/pack_dnloads_tracker.csv 
date,b_real,bims,bio_analytics,bio_db,bio_db_repo,by_unix,chess_db,db_facts,debug_call,disp_bn,gbn,lib,mlu,mtx,options,os_lib,pack_errors,pepl,pfd_meta,prosqlite,pub_graph,pubmed,r_session,real,spuds,stoics_lib,svg,upsh,wgraph
,b_real,bims,bio_analytics,bio_db,bio_db_repo,by_unix,chess_db,db_facts,debug_call,disp_bn,gbn,lib,mlu,mtx,options,os_lib,pack_errors,pepl,pfd_meta,prosqlite,pub_graph,pubmed,r_session,real,spuds,stoics_lib,svg,upsh,wgraph
21.09.01,35,63,17,77,48,69,34,192,49,2,3,33,61,41,86,78,80,25,11,655,12,35,16,527,32,69,3,21,64
21.09.02,37,64,20,87,50,70,44,210,69,8,6,43,69,43,96,92,83,28,18,755,15,45,19,547,38,77,8,27,69

?-  pack_dnloads(mode(tracked)).
?-  pack_dnloads([mode(tracked),pack(bims),pack(prosqlite),pack(real),append_profile(false)]).
==


To plot only specific packs from a tracker file, while ignoring any packs defined in profile file (user_profile(stoics/options/pack_dnloads.pl)), use:
==
?- pack_dnloads( [mode(tracked),pack(bims),pack(prosqlite),pack(real),append_profile(false),plot_all(false)] ).
==

Dependencies, stoics packs:
  * pack(lib)
  * pack(mlu)
  * pack(mtx)
  * pack(real)
  * pack(options)
  * pack(debug_call)
  * r_lib(ggplot2)

You only need to install pack(b_real), with
==
?- pack_install(b_real). 
==
This will also install pack(lib) which is a registered dependency.
The remaining packs will be installed interactively on first loading of library(b_real).


@author nicos angelopoulos
@version  0:1 2021/9/2
@tbd  date constraining and name altering goals
@tbd per month/per week, yearly pre-canned plots

*/
pack_dnloads( Args ) :-
     Self = pack_dnloads,
     pack_dnloads_opts( Args, Self, Opts ),
     options( mode(Mode), Opts ),
     pack_dnloads_mode( Mode, Self, Opts ).

pack_dnloads_mode( current, Self, Opts ) :-
     !,
     findall( Pack, member(pack(Pack),Opts), Packs ),
     pack_freqs( Packs, Self, Pairs ),
     GGt = theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
     mlu_frequency_plot( Pairs, [gg_terms(GGt)|Opts] ),
     date_two_digit_dotted( Dotted ),
     options( stem(Stem), Opts ),
     at_con( [Dotted,'-',Stem,'.pdf'], '', PlotF ),
     debuc( Self, 'Output file: ~p', [PlotF] ),
     <- ggsave( +PlotF ),
     sleep( 10 ),
     debuc( Self, stop, true ).

pack_dnloads_mode( update, Self, Opts ) :-
     !,
     pack_dnloads_tracker_file( TrackF, Self, Opts ),
     findall( Pack, member(pack(Pack),Opts), Packs ),
     ( exists_file(TrackF) ->
          mtx( TrackF, Mtx ),
          pack_dnloads_pack_columns( Mtx, Packs, Ftx, Facks )
          ;
          directory_file_path( TrackP, _, TrackF ),
          make_directory_path( TrackP ),
          Facks = Packs,
          Hdr =.. [hdr,date|Facks],
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

pack_dnloads_mode( tracked, Self, Opts ) :-
     pack_dnloads_tracker_file( TrackF, Self, Opts ),
     findall( Pack, member(pack(Pack),Opts), Packs ),
     debuc( Self, 'Packs: ~w', [Packs] ),
     Xlbl = pack, Ylbl = date, Vlbl = dnloads, Clbl=dnpack,
     Df = gdf,
     mtx( TrackF, TrackMtx ), 
     ( options(plot_all(true),Opts) ->
          mtx_r_df( TrackMtx, Xlbl, Ylbl, Vlbl, Clbl, Df )
          ;
          mtx_column_select( TrackMtx, [date|Packs], _, PacksLs ),
          findall( Row, (member(PacksL,PacksLs),Row=..[row|PacksL]), PacksMtx ),
          mtx_r_df( PacksMtx, Xlbl, Ylbl, Vlbl, Clbl, Df )
     ),

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
     options( stem_tracked(StemPrv), Opts ),
     ( StemPrv = false ->
          file_base_name( TrackF, TrackB ),
          file_name_extension( Stem, _, TrackB )
          ;
          Stem = StemPrv
     ),
     at_con( [Dotted,'-',Stem,'.pdf'], '', PlotF ),
     debuc( Self, 'Output file: ~p', [PlotF] ),
     <- ggsave( +PlotF ).

pack_dnloads_opts( Args, Self, Opts ) :-
     ( ((is_list(Args),memberchk(append_profile(AppP),Args));Args=append_profile(AppP)) ->
          options_append( Self, Args, Opts, append_profile(AppP) )
          ;
          options_append( Self, Args, Opts )
     ).

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
     ( var(Df) -> 
          Df = [Xlbl=XElems,Ylbl=YElems,Vlbl=VElems,Clbl=CElems]
          ;
          atomic(Df),
          Df <- [Xlbl=XElems,Ylbl=YElems,Vlbl=VElems,Clbl=CElems],
          Df <- data.frame(sapply(Df,c)),
          Df$Vlbl <- as.integer(Df$Vlbl)
     ).
