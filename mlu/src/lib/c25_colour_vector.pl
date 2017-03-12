
:- lib( c25/1 ).

%% c25_colour_vector( Nr, Cols ).
%
%  Sets c25 into an order and then chops the first Nr.
%
%==
% ?- requires( colours_pie/1 ).
% ?- c25_colour_vector( 2, Clrs ), colours_pie( Clrs ).
% Clrs = ["gold1", "#E31A1C"].
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/14   added example
%
c25_colour_vector( Nr, Cols ) :-
	number( Nr ),
	!,
	c25_number_colour_vector( Nr, Cols ).
c25_colour_vector( InList, Cols ) :-
	length( InList, Nr ),
	c25_number_colour_vector( Nr, Cols ).

c25_number_colour_vector( Nr, Cols ) :-
	c25( C25 ),
	c25_order( C25ord ),
	length( C25Idx, Nr ),
	( append(C25Idx,_,C25ord) -> 
			findall( Col, (member(Idx,C25Idx),arg(Idx,C25,Col)), Cols )
			; 
			Cols = C25ord 
	).

c25_order( [7,2,18,3,8,9,14,4,20,23,25,15,19,24,11] ).
