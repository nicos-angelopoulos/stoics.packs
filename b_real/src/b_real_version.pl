/** b_real_version( -Version, -Date ).

Version (Mj:Mn:Fx) and date and of publication (date(Y,M,D))>

==
?- version( Vers, Date ).
Vers = 0:4:0,
Date = date(2017, 4, 25).

==
*/
b_real_version( 0:4:0, date(2017,4,25) ).
