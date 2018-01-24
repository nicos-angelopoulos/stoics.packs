
:- lib(real).
% :- lib(suggests(term_type)).  % term_atom() -> term_to_atom()

%% palette_n_colours( Palette, N, Colours ).
% 
% Expands Palette mnemonics to N Colours.
%
%==
% ?- palette_n_colours( ramp("red","black","green"), 7, Clrs ).
% Clrs = ['#FF0000', '#AA0000', '#550000', '#000000', '#005400', '#00A900', '#00FF00'].
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/8/10    used to be palette_colours/3
%
palette_n_colours( brewer(Bpal), K, PalClrs ) :-
	PalClrs <- brewer.pal(K,+Bpal).
palette_n_colours( ramp(A,B,C), K, PalClrs ) :-
	CRP = colorRampPalette,
	maplist( wrap_string_as_atom, [A,B,C], [Ar,Br,Cr] ),
	term_to_atom( c(Ar,Br,Cr), ABC ),
	atomic_list_concat( [CRP,'(',ABC,')(',K,')'], Rcall ),
	PalClrs <- Rcall.
palette_n_colours( ramp(A,B), K, PalClrs ) :-
	CRP = colorRampPalette,
	maplist( wrap_string_as_atom, [A,B], [Ar,Br] ),
	term_to_atom( c(Ar,Br), ABC ),
	atomic_list_concat( [CRP,'(',ABC,')(',K,')'], Rcall ),
	PalClrs <- Rcall.

wrap_string_as_atom( String, Atom ) :-
	string( String ),
	!,
	atomic_list_concat( ['"',String,'"'], Atom ).
wrap_string_as_atom( Other, Other ).
