
% :- lib( expects(mtx,'wgraphs <-> csv') ).
% :- expects( lib(mtx), 'wgraphs <-> csv' ).

%% wgraph( +G ).
%
% True iff G is a weighted graph. That is, a list of elements
% of the form: N1-N2:W, where W must be numeric. 
% Atom entries (N1) are alllowed and denotate orphan nodes.
% 
% This is mostly a documentation predicate. Link here any predicates
% that work with this representation.
%
%==
% ?- wgraph( [nd1-nd2:2,nd3] ).
% true.
% ?- wgraph( [nd1-nd2:x] ).
% false.
%==
% @author nicos angelopoulos
% @version  0.1 2014/11/14
%
wgraph( Var ) :-
	var( Var ),
	!,
	fail.
wgraph( Graph ) :-
	wgraph_non_var( Graph ).

wgraph_non_var( []  ).
wgraph_non_var( [H|T] ) :-
	wgraph_element( H ),
	!,
	wgraph_non_var( T ).

wgraph_element( H ) :-
	H = _-_:W,
	number( W ).
wgraph_element( H ) :-
	atomic( H ).

wgraph_defaults( Args, Defs ) :-
	( memberchk(stem(Stem),Args) -> 
		Save = true
		;
		Save = false,
		Stem = wgraph
	),
	Defs = [save(Save),stem(Stem)].

%% wgraph( +InGraph, -Graph ).
%% wgraph( +OutF, +Graph ).
%% wgraph( +OutF, +Graph, +Opts ).
%
% Graph is the wgraph/1 canonical representation of InGraph. 
% InGraph, can be either a wgraph or a csv file that has columns
% 'from', 'to' and 'weight'. The latter case needs pack(mtx).
% When Graph is given, OutF is taken to be a file to store Graph in.
% When Opts are given they can change the default column names of OutF.
% 
% Opts
% * cnm_from(From=from)
%   name for from column
% * cnm_to(To=to)
%   name for to column
% * cnm_weight(Weight=weight)
%   name for weight column
% * save(Save=false)
%   to save wgraph in csv file, defaults to true if Stem is given
% * stem(Stem=wgraph)
%   stem for saving
%==
% ?- wgraph( 'wgraph_ex.csv', [a-b:1,a-c:2,b-c:3] ).
% ?- wgraph( 'wgraph_ex.csv', G ), write( graph(G) ), nl.
% ?- wgraph( File, [a-b:1,a-c:2,b-c:3], [stem(wgraph_ex)] ).
% ?- wgraph( File, [a-b:1,a-c:2,b-c:3], [stem(wgraph_ex1)] ).
%==
% Requires library(mtx) iff you need to save/restore from csv.
%
wgraph( In, Graph ) :-
	% var( Graph ),
	wgraph( In ),
	!,
	Graph = In.
wgraph( In, Graph ) :-
	var( Graph ),
	!,
	wgraph( In, Graph, [] ).

wgraph( In, Graph ) :-
	\+ var(Graph),
	ground(In),
	findall( row(F,T,W), (member(G,Graph),(G=F-T:W;(atomic(G),F=G,T='',W=''))), Rows ),
	wgraph_columns( WCnms ),
	Hdr =.. [row|WCnms], 
	csv_write_file( In, [Hdr|Rows] ).

wgraph( In, Graph, Args ) :-
	options_append( wgraph, Args, Opts ),
	wgraph_opts( In, Graph, Opts ).

wgraph_opts( In, Graph, Opts ) :-
	var( Graph ),
	!,
	wgraph_read_mtx( In, Mtx ),
	wgraph_columns_options( Mtx, WClms, Opts ),
	% findall( F-T:W, (member(row(F,T,W),wgraph_element(F-T:W)),Rows), Graph ).
	wgraph_columns_graph( WClms, Graph ).

wgraph_opts( OutF, Graph, Opts ) :-
	\+ var( Graph ),
	options( stem(Stem), Opts ),
	options( save(Save), Opts ),
	wgraph_save( Save, Graph, Stem, OutF, Opts ).

wgraph_read_mtx( In, Mtx ) :-
	(current_predicate(wgraph:mtx/2);current_predicate(mtx/2)),
	!,
	mtx( In, Mtx ).
wgraph_read_mtx( In, Mtx ) :-  % then lib(mtx) is not installed
	csv_read_file( In, Mtx ).

wgraph_save( false, _Graph, _Stem, _OutF, _Opts ).
wgraph_save( true, Graph, Stem, OutF, Opts ) :-
	findall( row(F,T,W), ( member(F-T:W,Graph)
	                       ;(member(F,Graph),atomic(F),T='',W='')
					 ), Rows ),
	wgraph_column_names( [From,To,Weight], Opts ),
	file_name_extension( Stem, csv, OutF ),
	csv_write_file( OutF, [row(From,To,Weight)|Rows] ).

wgraph_columns_graph( [[],[],[]], [] ) :- !.
wgraph_columns_graph( [[F|Fs],[T|Ts],[W|Ws]], [E|Es] ) :-
	( T='' ->
		E = F
		;
        ( number(W) -> Wnum = W; atom_number(W,Wnum) ),
		E = F-T:Wnum
	),
	wgraph_columns_graph( [Fs,Ts,Ws], Es ).

wgraph_column_names( WCnms, Opts ) :-
	wgraph_columns( WStdCnms ),
	maplist( wgraph_column_name(Opts), WStdCnms, WCnms ).

wgraph_column_name( Opts, Std, Cnm ) :-
	mtx_column_name_options( Std, Cnm, Opts ).

wgraph_columns_options( Mtx, WClms, Opts ) :-
	wgraph_columns( WCnms ),
	maplist( wgraph_mtx_column(Mtx,Opts), WCnms, WClms ).

wgraph_columns( [from,to,weight] ).

wgraph_mtx_column( Mtx, Opts, Cnm, Clm ) :-
	% mtx_column_options( Mtx, Cnm, Cnm, Clm, Opts ).
	mtx_column_name_options( Mtx, Cnm, Cnm, Clm, Opts ).
