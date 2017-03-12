
:- lib(real).
:- lib(options).

:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:compound/3).
:- lib(stoics_lib:message_report/3).

:- lib(select_nf/3).
:- lib(name_term/2).

plot_on_defaults( [post(true),prolog(false),debug(none)] ).

%% plot_on( Plot, Opts ).
%
%  Plot this on a number of devices as described in Opts.
%  Listens to debug(plot_on).
%
%  Opts, term(s) in:
%    
%   * dev     multiple entries of the same dev will be ignored. This library uses Cairo. none is a special
%             device that allows caller to print nothing (if the only device) without the library complaining about it.
%             Dev opened, just plots without opening or closing anything.
%
%   * debug(Dbg=none)  call debug(plot_on) if _true_, or turn off if _false_
%
%   * dev(Dev,DOpts)  DOpts are passed to Dev's cairo creation call. Additionally,
%
%	    * x11_devoff   x11 device will be turned off after printing on screen
%
%	    * devoff   if =|false|= the devise is not deved off. 
%
%   * order   if _true_ order of producing the plots is preserved. When *false*, dev options
%             are sorted, which means dublicates are removed.
%
%   * post(true)    Goal to call before dev.off
%
%   * prolog(false)   is Plot a Prolog call. The default is that it is a Real call, in which case =|<- Plot|= is called.
% 
%   * stem    of the filename to be used if dev is not x11. The full filename
%             is constructed with .dev as an extension for each given _dev_.
%             To write on full filenames use DOpts in dev+2 option.
%             If both _stem_ from Opts and _file_ from DOpts are missing, the stem =|plot_on|= is used.
%
% == 
% plot_on( plot(c(1,2,3)), dev(x11) ).
% plot_on( plot(c(1,2,3)), x11 ).      % prints a warning as there are no devices defined.
% 
% debug( plot_on ).
% plot_on( plot(c(1,2,3)), dev(x11) ).
% plot_on( plot(c(1,2,3)), dev(png) ).
% plot_on( plot(c(1,2,3)), dev(ps,[height(10),width(10)]) ).
% plot_on( plot(c(1,2,3)), dev(ps,height(10)) ).
% ==
%
% @author  nicos angelopoulos
% @version   0.1 2014/03/04.
% Topme: took 1 morning's work.
%
plot_on( Plot, Args ) :-
	options_append( plot_on, Args, Opts, process(debug) ),
	findall( Dev-[], member(dev(Dev),Opts), Unary ),
	findall( Dev-Dopts, (member(dev(Dev,DoptsPrv),Opts),en_list(DoptsPrv,Dopts)), Binary ),
	append( Unary, Binary, DevsAll ),
	( memberchk(order(true),Opts) -> 
		Devs = DevsAll
		;
		sort( DevsAll, Devs )
	),
	findall( Dev, member(Dev-_,Devs), ActDevsAll ),
	sort( ActDevsAll, ActDevs ),
	plot_on_init( ActDevs ),
	maplist( plot_on_dev(Plot,Opts), Devs ).

plot_on_dev( Plot, Opts, Dev-Dopts ) :-
	partition( name_term(par), Dopts, Pars, Popts ),
	device_on( Dev, Popts, Opts ),
	findall(_, ( member(par(Par),Pars),<- par(Par) ),_),
	% <- par(mar=c(5,18,4,2)),
	en_list( Plot, Plots ),
	memberchk( post(Post), Opts),
	memberchk( prolog(IsPGoal), Opts ),
	maplist( dev_plot(Dev,Post,IsPGoal), Plots ),
	device_off( Dev, Popts ).
	
dev_plot( none, _Post, _IsPGoal, _Plot ) :- !.
dev_plot( _Dev, Post, IsPGoal, Plot ) :-
	debug( plot_on, 'Sending plot: ~p', [Plot] ),
	dev_plot_call( IsPGoal, Plot ),
	call( Post ).

dev_plot_call( false, Plot ) :-
	<- Plot.
dev_plot_call( true, Plot ) :-
	call( Plot ).

device_on( opened, _Dopts, _Opts ) :- !.
device_on( none, _Dopts, _Opts ) :- !.
device_on( x11, Dopts, _Opts ) :-
	!,
	device_on_opts( x11, Dopts ).
device_on( Dev, Dopts, Opts ) :-
	( memberchk(file(_File),Dopts) -> 
		Fopts = Dopts
		;
		( memberchk(stem(Stem),Opts) ->
			true
			;
			Stem = plot_on
		),
		device_default_extension( Dev, Ext ),
		file_name_extension( Stem, Ext, File ),
		append( Dopts, [file(+File)], Fopts )
	),
	device_on_opts( Dev, Fopts ).

device_on_opts( Dev, Fopts ) :-
	findall( Name=Val, (member(Opt,Fopts),functor(Opt,Name,1),arg(1,Opt,Val)), Args ),
	upcase_atom( Dev, Upc ),
	atom_concat( 'Cairo', Upc, Cairo ),
	compound( Open, Cairo, Args ),
	debug( plot_on, 'Opening, dev: ~w, with call: ~p', [Dev,Open] ),
	<- Open.

/*
r_dev_wait( Opts ) :-
	memberchk( dev(Dev), Opts ),
	Dev == x11,
	!,
	r_wait.
r_dev_wait( _ ).
*/

device_off( opened, _Opts ) :- !.
device_off( none, _Opts ) :- !.
device_off( x11, Opts ) :-
	\+ memberchk( x11_devoff(true), Opts ),
	!.
device_off( _, Opts ) :-
	\+ memberchk( devoff(false), Opts ),
	r_devoff.

plot_on_init( ActDevs ) :-
	plot_on_lib( ActDevs ),
	plot_on_warning( ActDevs ).

plot_on_lib( ActDevs ) :-
	% select_nf( x11, ActDevs, Act1 ), % i think we use CairoX11 now
	select_nf( none, ActDevs, Act2 ),
	Act2 \== [],
	<- library("Cairo"),
	!.
plot_on_lib( _ActDevs ).

plot_on_warning( [] ) :-
	!,
	message_report( 'There are no devices in your options to plot_on/2.', [], warning ).
plot_on_warning( _ActDevs ).

% device_default_extension( x11, '' ) :- !.
device_default_extension( Dev-_, Ext ) :-   % fixme:
	!,
	downcase_atom( Dev, Ext ).
device_default_extension( Dev, Ext ) :-   % fixme:
	downcase_atom( Dev, Ext ).
