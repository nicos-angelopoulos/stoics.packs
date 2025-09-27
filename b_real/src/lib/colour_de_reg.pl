
:- ensure_loaded(library(real)).
:- lib(promise(r('brewer.pal'),'RColorBrewer')).

%% colour_de_reg( -DeReg ).
%% colour_de_reg_3( -DeReg3 ).
% 
%  Standard colour for de regulation: DeReg = up_down(Up,Down)
%  of DeReg3 = up_down(Up,Level,Down)
%  Colours for up and down (de) regulation.
%
%==
% ?- colour_de_reg( up_down(Up,Down) ), colours_pie( [Up,Down] ).
%==
%  @author nicos angelopoulos
%  @version 0.1 2014/02/12
%  @version 0.2 2014/02/12,  switched the order, now: up = Red, down = Green.
%  @Version 0.3 2025/09/27,  dependancy to RColorBrewer is promised now, also exposed preds to module interface
%
colour_de_reg( up_down(Red,Green) ) :-
     lib_r_promised( 'brewer.pal' ),
	Set1 <- 'brewer.pal'(9,"Set1"), 
	Set1 = [Red,_Blue,Green|_].

colour_de_reg_3( up_down(Red,Orange,Green) ) :-
     lib_r_promised( 'brewer.pal' ),
	Set1 <- brewer.pal(9,"Set1"), 
	Set1 = [Red,_Blue,Green,_Mauve,Orange|_].
