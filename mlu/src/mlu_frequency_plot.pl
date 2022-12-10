
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:kv_decompose/3).
:- lib(stoics_lib:current_call/1).
:- lib(stoics_lib:list_frequency/3).

:- lib(suggests(real) ).
:- lib(suggests(b_real)).

mlu_frequency_plot_defaults( Defs ) :-
    ( current_predicate(r_version/3) -> 
        Ifce = gg_bar
        ;
        ( current_predicate(b_real_version/2) ->
            Ifce = gg_bar
            ;
            throw( either_of_required(lib(real),lib(b_real)) )

        )
    ),
    Defs = [ interface(Ifce), sort(false), pop_line(false), pop_breaks(false) ].

/** mlu_frequency_plot( +FreqOrVec, +Opts ).

Make a plot for pairlist or vector, Data. 

Data is one of
  * pairlist
    of the form, Item-Times
  * a list
    that is passed, with Opts, to list_frequency/3
  * a vector
    the values of which are retrieved with b_real:pl_vector/3, and then\br
    passed, with Opts, to list_frequency/3

Opts
  * interface(Iface)
    _barplot_, or _gg_bar_ interfaces are supported. The first requires lib(real), 
    in addition the latter also requires lib(b_real).
    If b_real is present, the second interface becomes the default.

  * pop_breaks(Breaks=false)
    colour population groups according to given break points. Splits are done with 
    =< so break points go to the left partition. (Currently only for _gg_bar_ interface.)

  * pop_line(PlineAt=false)
    if integer draws a vertical line separating columns with counts less than PlineAt to those with more.
    Only makes sense if Sort is set to _frequency_. (Currently only for _gg_bar_ interface.)

  * sort(Sort=false)
    alternatives to not sorting (default)
     * elem/element
	  sort by element
     * true/freq/frequency
	  sort on frequency

Other options are passed to either gg_bar_plot/2 (if Iface == gg_bar) or to r_call/2 (if Iface == barplot).

==
?- lib(pepl).
?- sload_pe( coin ).


?- [pack(mlu/examples/grouped_freqs)].
?- grouped_freqs.
% a plot with 9 bars and 3 groups should appear on screen

?- mlu_frequency_plot( [1,1,1,2,2,3], true ).
?- mlu_frequency_plot( [1,1,1,2,2,3], interface(barplot) ).
==

==
?- mlu_frequency_plot( [1,1,2,11,12,21,31,33,41], [bins([10,20]),interface(gg_bar)] ).
==

The plot produced has binned Data into 3 bins.

==
?- lib(pepl).
?- sload_pe( coin ).
?- mlu_sample( scall(coin(Side)), 100, Side, Freqs ), mlu_frequency_plot( Freqs, [interface(barplot),outputs([svg]),las=2] ).
==
Produces file: real_plot.svg

[[doc/html/images/real_plot.svg]]

==
?- mlu_sample( scall(coin(Side)), 100, Side, Freqs ), mlu_frequency_plot( Freqs, [interface(gg_bar),output(png("naku.png"))] ).
==
Produces file: naku.png

[[doc/html/images/naku.png]]

@author nicos angelopoulos
@version  0.1 2016/8/31
@version  0.2 2017/1/13, added option sort(false)
@version  0.3 2017/8/29, added vectors as inputs via b_real:pl_vector/3

*/

mlu_frequency_plot( InFreqPrv, Args ) :-
    options_append( mlu_frequency_plot, Args, Opts ),
    mlu_frequency_plot_pairs( InFreqPrv, InFreq, Opts ),
    options( sort(Sort), Opts ),
    ground( Sort ),  % fixme: type check
    mlu_frequency_plot_sort( Sort, InFreq, Freq ),
    options( interface(Iface), Opts, rem_opts(Ropts) ),
    mlu_frequency_interface( Iface, Freq, Ropts ).

mlu_frequency_plot_pairs( InFreqPrv, InFreq, _Opts ) :-
    \+ var( InFreqPrv ),
    InFreqPrv = [_-_|_],
    !,
    InFreqPrv = InFreq.
mlu_frequency_plot_pairs( InFreqPrv, InFreq, Opts ) :-
     is_list( InFreqPrv ),
     !,
     list_frequency( InFreqPrv, InFreq, Opts ).
mlu_frequency_plot_pairs( InFreqPrv, InFreq, Opts ) :-
    current_call( mlu:pl_vector(InFreqPrv,InVect,Opts) ),
    !,
    list_frequency( InVect, InFreq, Opts ).
mlu_frequency_plot_pairs( InFreqPrv, _InFreq, _Opts ) :-
    throw( pack_error(mlu,mlu_requency_plot/2,arg_enumerate(1,[pairlist,pl_vector],InFreqPrv)) ).

mlu_frequency_interface( barplot, Freqs, Opts ) :-
    kv_decompose( Freqs, Lbls, Vals ),
    % ( select(plot_args(PargS),Opts,CallOpts) -> PargS = [] ; CallOpts = Opts ),
    % en_list( PargS, Pargs ),
    % findall( Name=Val, (member(Parg,Pargs),Parg=..[Name,Val]), Cargs ),
    % Call =.. [barplot,Vals,names,arg=Lbls|Cargs],
    r_call( barplot(Vals,names.arg=Lbls), Opts ).
mlu_frequency_interface( gg_bar, Freqs, Opts ) :-
    options( pop_line(Pline), Opts ),
    mlu_freq_interface_gg_bar_pop_line( Pline, Freqs, Opts, POpts ),
    options( pop_breaks(Pbreaks), Opts ),
    mlu_freq_interface_gg_bar_pop_groups( Pbreaks, Freqs, POpts, GOpts ),
    gg_bar_plot( Freqs, GOpts ).

mlu_freq_interface_gg_bar_pop_groups( false, _Freqs, Opts, GOpts ) :-
    !,
    GOpts = Opts.
mlu_freq_interface_gg_bar_pop_groups( BrkS, Freqs, Opts, GOpts ) :-
    reverse( Freqs, Rreqs ),
    en_list( BrkS, Brks ),
    sort( Brks, Orks ),
    % reverse( Orks, Srks ),
    Orks = [Brk|Tlk],
    atom_concat( '# =< ', Brk, ClrAtm ),
    atom_string( ClrAtm, Clr ),
    mlu_freq_interface_gg_bar_pop_breaks( Rreqs, Brk, Tlk, Clr, Plrs ),
    append( Opts, [level_colours(Plrs),level_colours_title('counts')], GOpts ).

mlu_freq_interface_gg_bar_pop_breaks( [], _Brk, _Brks, _Clr, [] ).
mlu_freq_interface_gg_bar_pop_breaks( [Lvl-Tms|T], Brk, Brks, Clr, Plrs ) :-
    Tms =< Brk,
    !,
    Plrs = [Lvl-Clr|Tlrs],
    mlu_freq_interface_gg_bar_pop_breaks( T, Brk, Brks, Clr, Tlrs ).
mlu_freq_interface_gg_bar_pop_breaks( [H|T], Brk, Brks, _Clr, Plrs ) :-
    mlu_freq_interface_gg_bar_pop_breaks_cont( Brks, Brk, [H|T], Plrs ).

mlu_freq_interface_gg_bar_pop_breaks_cont( [], LastBrk, Freqs, Plrs ) :-
    atom_concat( '#  > ', LastBrk, ClrAtm ),
    atom_string( ClrAtm, Clr ),
    findall( Lvl-Clr, member(Lvl-_Tms,Freqs), Plrs ).
mlu_freq_interface_gg_bar_pop_breaks_cont( [Brk|Brks], _LastBrk, Freqs, Plrs ) :-
    atom_concat( '# =< ', Brk, ClrAtm ),
    atom_string( ClrAtm, Clr ),
    mlu_freq_interface_gg_bar_pop_breaks( Freqs, Brk, Brks, Clr, Plrs ).

mlu_freq_interface_gg_bar_pop_line( false, _Freqs, Opts, POpts ) :- 
    !,
    POpts = Opts.
mlu_freq_interface_gg_bar_pop_line( PlineAt, Freqs, Opts, POpts ) :- 
    integer( PlineAt ),
    nth1( NCnt, Freqs, _-Cnt ),
    Cnt < PlineAt,
    !,
    At is NCnt - 0.5,
    Vline = geom_vline(xintercept = At),
    ( select(gg_terms(GTins),Opts,ROpts) ->
        en_list( GTins, GTinsL ),
        % HalfToneDown is PlineAt - 0.5,
        atom_concat( 'y<', PlineAt, XeqText ),
        findall( ACnt, member(_-ACnt,Freqs), AllCnts ),
        max_list( AllCnts, MaxCnt ),
        Xpos is At + (MaxCnt / 400),
        % Vtext = annotate(+text, x=Xpos, y=MaxCnt, hjust=0, label= +XeqText),
        Ypos is 0 - (MaxCnt / 50),
        Vtext = annotate(+text, x=Xpos, y= Ypos, hjust=0, vjust=1, label= +XeqText),
        POpts = [gg_terms([Vline,Vtext|GTinsL])|ROpts]
        ;
        POpts = [gg_terms([Vline])|Opts]
    ).
mlu_freq_interface_gg_bar_pop_line( PopLine, Freqs, _Opts, _POpts ) :- 
    !,
    throw( fixme(pop_line_failure(PopLine,Freqs)) ).

mlu_frequency_plot_sort( false, InFreq, Freq ) :-
	Freq = InFreq.
mlu_frequency_plot_sort( frequency, InFreq, Freq ) :-
	findall( Freq-Elem, member(Elem-Freq,InFreq), Trans ),
	keysort( Trans, Srans ),
	findall( Elem-Freq, member(Freq-Elem,Srans), FreqRev ),
	reverse( FreqRev, Freq ).
mlu_frequency_plot_sort( freq, InFreq, Freq ) :-
	mlu_frequency_plot_sort( frequency, InFreq, Freq ).
mlu_frequency_plot_sort( true, InFreq, Freq ) :-
	mlu_frequency_plot_sort( frequency, InFreq, Freq ).
mlu_frequency_plot_sort( element, InFreq, Freq ) :-
	keysort( InFreq, Freq ).
mlu_frequency_plot_sort( elem, InFreq, Freq ) :-
	mlu_frequency_plot_sort( element, InFreq, Freq ).
