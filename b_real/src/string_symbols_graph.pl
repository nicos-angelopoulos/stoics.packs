
:- lib(suggests(bio_db)).     % edge_string_hs_symb/3
:- lib(options).

string_symbols_graph_defaults( Defs ) :-
	Defs = [
	           min_w(500),
			 catenator(';')
	].

/** string_symbols_graph( +Symbols, -Graph, +Opts ).

Collect weight bearing edges among all Symbols as per string database.
string_graph/3 will become a generalisation of this
This version uses, bio_db for the string data.

Opts
 * catenator(Cat=';')
    atom to use as catenator in multi-symbol splits

 * min_w(MinW=500)
    threshold below or at which edges are ignored (0 =< MinW =< 1000).

@tbd cite string database location, and in the intereactions file we need meta-info
@see string_graph/3.
*/

string_symbols_graph( Symbols, Graph, Args ) :-
	options_append( string_symbols_graph, Args, Opts ),
	options( min_w(MinW), Opts ),
	sort( Symbols, Order ),
	options( catenator(Ctn), Opts ),
	string_symbols_order_graph( Order, MinW, Ctn, [], Graph ).

string_symbols_order_graph( [], _MinW, _Ctn, _All, [] ).
string_symbols_order_graph( [Symb|Symbs], MinW, Ctn, Seen, Graph ) :-
	findall( Symb-Neigh:W, ( member(Neigh,[Symb|Symbs]),
						  findall( Aw, ( 
						              symbol(Symb,Ctn,ASymb),
						              symbol(Neigh,Ctn,ANeigh),
								    Symb \== Neigh,
								    edge_string_hs_symb(ASymb,ANeigh,Aw),
						              MinW =< Aw 
								   ),   Ws ),
						  max_list(Ws,W)
					   ), 
							EdgesPrv ),
	( EdgesPrv == [] -> 
		( memberchk(_-Symb:_,Seen) ->
			Edges = []
			;
			Edges = [Symb]
		)
		;
		Edges = EdgesPrv 
	),
	once( append(Edges,Tail,Graph) ),
	append( Edges, Seen, Already ),
	string_symbols_order_graph( Symbs, MinW, Ctn, Already, Tail ).

symbol( Symb, Ctn, ASymb ) :-
	atomic_list_concat( Symbs, Ctn, Symb ),
	member( ASymb, Symbs ).
