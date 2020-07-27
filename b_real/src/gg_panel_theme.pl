
/** gg_panel_theme( +Panel, -GgTerms ).

Expand a theme panel token to its corresponding ggplot terms.

Panel
  * standard
  standard panel decorations (empty list)

  * blank
  produces a panel with white background, no grid lines and no axes lines

  * axes
  as blank theme, but with black axes lines

==
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, true ).
    % shows a plot with grid lines and axis lines
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, panel_theme(blank) ).
    % shows a plot with neither grid lines and axis lines
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, panel_theme(axes) ).
    % shows a plot with no-background colour and no grid lines, but with lines on both axes
==


@author nicos angelopoulos
@version  0:2 2020/7/27, renamed and added to b_real (from hrmn), new theme

*/
gg_panel_theme( standard, [] ).
gg_panel_theme( blank, List ) :-
    List = [ theme(panel.background(element_blank()),
                   panel.grid.major(element_blank()),
                   panel.grid.minor(element_blank())
                  ) 
           ].
gg_panel_theme( axes, List ) :-
    List = [ theme(panel.background(element_blank()),
                   panel.grid.major(element_blank()),
                   panel.grid.minor(element_blank()),
                   axis.line.x = element_line(colour = "black", size=0.5, linetype="solid"),
                   axis.line.y = element_line(colour = "black", size=0.5, linetype="solid")
                  ) 
           ].
