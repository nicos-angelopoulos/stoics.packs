% :- lib( options ).

:- lib( stoics_lib:list_frequency/3 ).

:- lib( vector_factor_indices/3 ).
:- lib( freq_sort/3 ).
%
% in b_real interface:
:- lib( pl_vector/3 ).
:- lib( gg_bar_plot/2 ).

vectors_subed_gg_bar_plot_defaults( Defs ) :-
	Defs = [ sort_x(false), x_axis_colour(false), colours(Clrs)
	       ],
	       Clrs = ["#E32636","#FF7E00","#89CFF0","#5D8AA8"].

/** vectors_subed_gg_bar_plot(+Vectors,+Val,+Vect2,+Opts).

Plot frequencies of a value in Vectors with subdivisions for each of the 
values apppearing in Vect2. All vectors are defined relative to Mtx.

Opts 
  * sort(Sort=false)
     else true (lexicographical), or totals for total counts

  * mtx(Mtx)
     matrix to extract the vectors from

==
Mtx = [  row(v,x,y,z),
         row(1,0,0,a),
         row(0,0,1,a),
	    row(0,1,1,b),
	    row(1,1,1,b),
	    row(0,1,0,c)
      ],
assert( ex1(Mtx) ).

?- ex1(Mtx), vectors_subed_gg_bar_plot( [v,x,y], 1, z, mtx(Mtx) ).
[[1,1,0],[0,2,1],[1,2,0]]

==

@author nicos angelopoulos
@version  0.1 2015/11/25
@see pl_vector/3
@see vector_factor_indices/3

*/
vectors_subed_gg_bar_plot( Vecs, Val, VecSpecA, Args ) :-
	options_append( vectors_subed_gg_bar_plot, Args, Opts ),
	% pl_vector( VecSpecA, VecA, Opts ),
	VFopts = [cnm(Cnm),vals(ByFactor)|Opts],
	vector_factor_indices( VecSpecA, Idxs, VFopts ),
	sort( Idxs, Udxs ),
	maplist( vector_val_subed_freqs(Val,Idxs,Udxs,Opts), Vecs, Freqs ),
     % XaxisTxt = theme( axis.text.x( element_text(angle(-90), hjust(0), color(XxClrs))) ),
	options( sort_x(Sort), Opts ),
	freq_sort( Sort, Freqs, Orted ),

	options( x_axis_colour(XaClrG), Opts ),
	vectors_subed_gg_bar_plot_x_axis_theme( XaClrG, Orted, XaxisThm, Xleg ),
	options( colours(Clrs), Opts ),
	append( Opts, [legend_title(Cnm)], Lopts ),
	BoldTitle = theme(plot.title(element_text(face(+"bold")))),
	( memberchk(gg_terms(GGoTermS),Opts) -> en_list(GGoTermS,GGoTerms); GGoTerms = [] ),
	GGos = [ geom_bar_position(stack), flip(false), 
	         fill_colours(Clrs),       
		    legend_labels(ByFactor),
		    % legend_labels(["a","f","c","D","E"]),
		    gg_terms([BoldTitle,XaxisThm|GGoTerms]),
		    % gg_terms([XaxisThm]),
		    extra_legend(Xleg),
		    keep_order(true)
		    | Lopts
		    ],
	gg_bar_plot( Orted, GGos ).

vectors_subed_gg_bar_plot_x_axis_theme( false, _Orted, XaxisTxt, false ) :-
	!,
     XaxisTxt = theme( axis.text.x( element_text(angle(-90), hjust(0))) ).
vectors_subed_gg_bar_plot_x_axis_theme( Goal, Orted, XaxisTxt, Xleg ) :-
	call( Goal, Orted, XaClrs, Xleg ),
     XaxisTxt = theme( axis.text.x( element_text(angle(-90), hjust(0), color(XaClrs))) ).

vector_val_subed_freqs( Val, Idxs, Udxs, Opts, VecSpec, Cnm-Counts ) :-
	pl_vector( VecSpec, Vec, [cnm(Cnm)|Opts] ),
	findall( Idx, (nth1(N,Vec,Val),nth1(N,Idxs,Idx)), IList ),
	list_frequency( IList, Udxs, Freqs ),
	findall( Count, member(_-Count,Freqs), Counts ).
