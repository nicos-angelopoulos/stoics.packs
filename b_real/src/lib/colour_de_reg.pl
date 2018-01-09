
:- ensure_loaded( library(real) ).
:- <- library("RColorBrewer").

%% colour_de_reg( -DeReg ).
%% colour_de_reg_3( -DeReg3 ).
% 
%  Standard colour for de regulation: DeReg = up_down(Up,Down)
%  of deReg3 = up_down(Up,Level,Down)
%  Colours for up and down (de) regulation.
%
%==
% ?- requires( colours_pie/1 ).
% ?- colour_de_reg( up_down(Up,Down) ), colours_pie( [Up,Down] ).
%==
%  @author nicos angelopoulos
%  @version 0.1 2014/02/12
%  @version 0.2 2014/02/12,  switched the order, now: up = Red, down = Green.
%
%
colour_de_reg( up_down(Red,Green) ) :-
	Set1 <- brewer.pal(9,"Set1"), 
	Set1 = [Red,_Blue,Green|_].
% colour_de_reg( up_down(Green,Red) ) :-

colour_de_reg_3( up_down(Red,Orange,Green) ) :-
	Set1 <- brewer.pal(9,"Set1"), 
	Set1 = [Red,_Blue,Green,_Mauve,Orange|_].
