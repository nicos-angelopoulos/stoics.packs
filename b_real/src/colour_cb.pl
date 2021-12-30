
:- lib(real).

/** colour_cb( -Cb ).

Colour blind palette with grey. Cb is a c/8 term.

==
?- lib(b_real:colours_pie/1).
?- colour_cb(Cb), colours_pie(Cb).
Cb = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
?- lib(real).
?- colour_cb(Cb), <- svg("clr_cb.svg"), colours_pie(Cb), r_devoff.
==
Produces file: clr_cb.svg

[[doc/html/images/clr_cb.svg]]

@author nicos angelopoulos
@version 0.1 2014/02/10
@see http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
@see http://jfly.iam.u-tokyo.ac.jp/color/
*/

colour_cb( Cb ) :-
	Cb = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").

/** colour_cb_r.

 Colour blind palette with grey is set in R variable cbPalette. 

@author nicos angelopoulos
@version 0.1 2014/02/10
@see colour_cb/1.
*/
colour_cb_r :-
	colour_cb( Cb ),
	cbPalette <- Cb.

/** colour_cbb( -Cbb ).

 Colour blind palette with black. Cb is a c/8 term.

==
?- lib(b_real:colours_pie/1).
?- colour_cbb(Cbb), colours_pie(Cbb).
==
@author nicos angelopoulos
@version 0.1 2014/02/10
@see colour_cb/1

*/
colour_cbb( Cbb ) :-
	Cbb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").

/** colour_cbb_r.

 Colour blind palette with black is set in R variable cbbPalette. 

@author nicos angelopoulos
@version 0.1 2014/02/10
@see colour_cbb/1.
*/
colour_cbb_r :-
	colour_cbb( Cbb ),
	cbbPalette <- Cbb.
