% need to robustify this
% add options for balance, I have this some where ... 
%
heatmap_breaks( Rv, BsOpt, Opts ) :-
	options( centre(Centre), Opts ),
	options( breakpoints(Bkps), Opts ),
	heatmap_breaks( Centre, Bkps, Rv, BsOpt ).

heatmap_breaks( Centre, Bps, Rv, BsOpt ) :-
	number( Centre ),
	!,
	Max <- max(Rv),
	Min <- min(Rv),
	Sid is floor(Bps / 2) - 1,
	Div is Sid + 1,
	findall( B, (between(1,Sid,I),B is (I * ((Centre-Min)/Div))+Min), Bs1 ), 
	findall( B, (between(1,Sid,I),B is Max - (I * ((Max-Centre)/Div))), Bs2Rev ), 
	reverse( [Max|Bs2Rev], Bs2 ),
	( Centre =:= 1 ->
	    % i am not at all sure about this:
	    ( Bs1 = [0.0,HBs1|TBs1] -> Mid is HBs1 / 2, Bs1Z = [0.0,Mid,HBs1|TBs1]
	                         ; Bs1Z = [0.0|Bs1] ),
		append( Bs1Z, Bs2, Bs )
		;
		Centre =:= 0,
		append( [Min|Bs1], [0.0|Bs2], Bs )
	),
	debug( b_real, 'Breaks: :~w', [Bs] ),
	BsOpt = (breaks = Bs).
heatmap_breaks( _Centre, _Bps, _Rv, true ).
