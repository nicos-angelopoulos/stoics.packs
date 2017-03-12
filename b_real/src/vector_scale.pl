vector_scale_defaults( scale(false) ).

/** vector_scale( +VectSpec, -Scaled, +Opts ).

This is a funnel predicate that can be used to scale a vector (VectSpec)
through options. Scaled should be something Real (pack(real)) understands as a LHS to <- assignment.
That is, if it is a Prolog variable, it will be instantiated to the vector as a list,
or a ground term, will translate that to an R expression (typically an R variable) to which
the scaled vector is assigned to.

For details on VectSpec, see pl_vector/3 and options affecting this.

Opts
  * scale(Scale=false)
      or true (full scale, subtract mean and divide by stadard deviation)
      or centre (subtract mean)

See R's scale().

==
?- Mtc <- as.vector( mtcars[*,3] ), Mean <- mean(Mtc), vector_scale( Mtc, Sca, [scale(true)] ), 
   ScaMean <- mean(Sca), ScaVrc <- var(Sca).

Mtc = ...
Mean = 230.721875,
Sca = ...
ScaMean = -9.084936579070724e-17,
ScaVrc = [[1.0]].

?- Mtc <- as.vector( mtcars[*,3] ), Mean <- mean(Mtc), vector_scale( Mtc, Sca, [scale(centre)] ), 
   ScaMean <- mean(Sca), ScaVrc <- var(Sca).

Mtc = ...
Mean = 230.721875,
Sca = ...
ScaMean = -1.199040866595169e-14,
ScaVrc = [[15360.799828629033]].

==

@author nicos angelopoulos
@version  0.1 2015/7/17

*/

vector_scale( VectIn, Scaled, Args ) :-
	options_append( vector_scale, Args, Opts ),
	pl_vector( VectIn, Vector, Opts ),
	options( scale(Scale), Opts ),
	vector_scale_opt( Scale, Vector, Scaled ).

vector_scale_opt( false, Vector, Scaled ) :-
	scale_opt_false( Scaled, Vector ).
vector_scale_opt( true, Vector, Scaled ) :-
	Scaled <- as.vector(scale( Vector ) ).
vector_scale_opt( centre, Vector, Scaled ) :-
	Scaled <- as.vector(scale( Vector, scale='FALSE')).
	
% this can be of more general use. Add to Real itself.
%
vector_scale_opt_false( Scaled, Vector ) :-
	var( Scaled ),
	!,
	Scaled = Vector.
vector_scale_opt_false( Scaled, Vector ) :-
	Scaled <- Vector.
