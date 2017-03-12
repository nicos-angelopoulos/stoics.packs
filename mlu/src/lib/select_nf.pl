%% select_nf( ?Elem, +List, -Rem ).
%
% As select/3, but does not fail if Elem is not in List.
% It also works for variable Elem, but makes no much sense then.
%
%==
%   select_nf( a, [b,c,d], BCD ).
%   select( a, [b,c,d], BCD ).
%   select_nf( A, [b,c,d], BCD ).
%
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/03/10
%
select_nf( Elem, List, Rem ) :-
	\+ var( Elem ),
	!,
	select_nf_ground( Elem, List, Rem ).
select_nf( Elem, List, Rem ) :-
	select( Elem, List, Rem ).

select_nf_ground( Elem, List, Rem ) :-
	select( Elem, List, Rem ),
	!.
select_nf_ground( _Elem, List, List ).
