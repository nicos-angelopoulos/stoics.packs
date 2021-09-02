/** <module> Interface predicates to commonly used R functions.

Interface predicates to commonly used R functions. This library allows
Prolog users to do same simple tasks in R with out writing any R code
or call any R functions explicitly.

Dependancies
  * pack(real)
     obviously
  * aheatmap/2 
     depends on R library "NMF"
  * pack(mtx)
     some predicates depend on pack _mtx_ which is useful if working with matrices

@author nicos angelopoulos
@version  0.1.0 2015/6/17
@version  0.1.1 2015/7/24, added gg_bar_plot/2, then on 12/16 added pl_plot_on/2.
@version  0.2   2016/1/23  
@version  0.3   2017/3/11, works with lib 2.0 and stoics_lib
@version  0.4   2017/4/25, fixes + r_hist/2.
@version  0.7   2021/9/2,  scripts/pack_dnloads.pl

*/

/** b_real.

Documentation predicate. Anchor reference to the package to this predicate.

*/
b_real.
