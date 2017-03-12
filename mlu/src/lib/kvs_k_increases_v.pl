/** kvs_k_increases_v( +KVs, +K, -IncKVs ).

Increase the counter (V) for key K. If K-Kcounter is not present, add it with Kcounter = 1.
KVs are ordered.
==
?- kvs_k_increases_v( [], a, KVs1 ), 
   kvs_k_increases_v( KVs1, c, KVs2 ),
   kvs_k_increases_v( KVs2, a, KVs3 ),
   kvs_k_increases_v( KVs3, b, KVs4 ),
   kvs_k_increases_v( KVs4, a, KVs5 ).

KVs1 = [a-1],
KVs2 = [a-1, c-1],
KVs3 = [a-2, c-1],
KVs4 = [a-2, b-1, c-1],
KVs5 = [a-3, b-1, c-1].

==

*/
kvs_k_increases_v( [], K, [K-1] ).
kvs_k_increases_v( [K1-V1|T], K, IncKVs ) :-
	compare( Kop, K1, K ),
	kvs_k_op_increases_v( Kop, K1, V1, T, K, IncKVs ).

kvs_k_op_increases_v( =, K1, V1, T, _K, [K1-V2|T] ) :- 
	V2 is V1 + 1.
kvs_k_op_increases_v( <, K1, V1, T, K, [K1-V1|Rest] ) :- 
	kvs_k_increases_v( T, K, Rest ).
kvs_k_op_increases_v( >, K1, V1, T, K, [K-1,K1-V1|T] ).
	
