
/** gg_theme( +Panel, -GgTerms ).

Expand a theme panel token to its corresponding ggplot terms.

Panel
  * axes
  as blank theme, but with black axes lines

  * blank
  produces a panel with white background, no grid lines and no axes lines

  * false
  no theme, (empty list of terms)

  * lolli
  theme suitable for lollipop plots (gg_lollipop/2) in normal orientation, light theme + no borders, no major and ticks on y 

  * lolly
  theme suitable for lollipop plots (gg_lollipop/2) in flipped orientation, light theme + no borders, no major and ticks on y 

  * standard
  standard panel decorations, i.e. no theme (empty list)

==
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, true ).
    % shows a plot with grid lines and axis lines
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, theme(blank) ).
    % shows a plot with neither grid lines and axis lines
?- Pairs = [a-[1,2,3],b-[2,4,6]], gg_bar_plot( Pairs, theme(axes) ).
    % shows a plot with no-background colour and no grid lines, but with lines on both axes
==


@author nicos angelopoulos
@version  0:2 2020/7/27, renamed and added to b_real (from hrmn), new theme
@version  0:3 2023/8/31, added lolli,lolly themes (and aliased false to [])

*/
gg_theme( axes, List ) :-
    List = [ theme(panel.background=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   axis.line.x = element_line(colour = "black", linewidth=0.5, linetype="solid"),
                   axis.line.y = element_line(colour = "black", linewidth=0.5, linetype="solid")
                  ) 
           ].
gg_theme( blank, List ) :-
    List = [ theme(panel.background=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank()
                  ) 
           ].
% default theme for lollipop plots: see 
gg_theme( false, [] ).
gg_theme( lolli, List ) :-
     List = [ theme_light(),
              theme( 'panel.grid.major.x'=element_blank(),
                     'panel.border'=element_blank(),
                     'axis.ticks.x'=element_blank()
                   )
            ].
gg_theme( lolly, List ) :-
     List = [ theme_light(),
              theme( 'panel.grid.major.y'=element_blank(),
                     'panel.border'=element_blank(),
                     'axis.ticks.y'=element_blank()
                   )
            ].
gg_theme( standard, [] ).
