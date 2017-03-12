/** r_data_frame( +DfIn, +Df ).

Convert DfIn to an R data frame.

If DfIn, is an R variable, and Df is atomic it is assumed to be a data frame
and Df <- DfIn, is called (this can be elaborated upon later), whereas
if Df is variable then Df = DfIn is called.

If DfIn is not an R variable, it is passed through mtx/2 and
the result is passed through mtx_df/2. When Df is a variable in this context,
a unique variable is generated that looks like tmp.df.N where N is an 
integer.

@ see mtx/2
@ see mtx_df/2
@ see r_mtx/3

*/
r_data_frame( In, Df ) :-
	r_is_var( In ),
	!,
	r_data_frame_rv( Df, In ).
r_data_frame( In, Df ) :-
	mtx( In, Mtx ),
	r_data_frame_ground_var( Df ),
	mtx_df( Mtx, Df ).

r_data_frame_rv( Df, In ) :-
	var( Df ),
	!,
	Df = In.
r_data_frame_rv( Df, In ) :-
	Df <- In.

r_data_frame_ground_var( Df ) :-
	\+ var( Df ),
	!.
r_data_frame_ground_var( Df ) :-
	var( Df ),
	r_unique( tmp.df, Df ).
