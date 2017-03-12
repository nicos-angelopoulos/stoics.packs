% vectors_subed_sort_x( false, Freqs, Freqs ).

freq_sort( false, Freqs, Freqs ).
freq_sort( true, Freqs, Orted ) :-
	sort( Freqs, Orted ).
freq_sort( totals, Freqs, Orted ) :-
	freq_sort_sum_totals( Freqs, 1, Totals ),
	sort( 0, @>=, Totals, ByTotal ),
	findall( Pair, (member(_-I,ByTotal),nth1(I,Freqs,Pair)), Orted ).

freq_sort_sum_totals( [], _I, [] ).
freq_sort_sum_totals( [_Lbl-Counts|T], I, [Total-I|R] ) :-
	sum_list( Counts, Total ),
	J is I + 1,
	freq_sort_sum_totals( T, J, R ).
