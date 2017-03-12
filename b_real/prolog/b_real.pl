:- module( b_real, [
                    b_real_version/2,
                    aheatmap/1, aheatmap/2,
                    lm_plot/3,
                    gg_bar_plot/2,
                    vectors_subed_gg_bar_plot/4,
                    mtx_pheatmap/2,
                    mtx_column_pheatmap/3,
                    options_rvar_rmv/2,
                    pl_vector/3, vector_scale/3,
                    pl_plot_on/2,
                    r_mtx/2, r_mtx/3, r_data_frame/2, r_unique/2,
                    columns_fisher_test/5,
                    dot/3,
                    mtx_df/2, mtx_df/3
                ] 
    ).

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
     some side predicates depend on private pack _mtx_ which should become public soon

@author nicos angelopoulos
@version  0.1.0 2015/6/17
@version  0.1.1 2015/7/24, added gg_bar_plot/2, then on 12/16 added pl_plot_on/2.
@version  0.2   2016/1/23  
@version  0.3   2017/3/11, works with lib 2.0 and stoics_lib

*/

/** b_real_version( -Version, -Date ).

Version (Mj:Mn:Fx) and date and of publication (date(Y,M,D))>

==
?- version( Vers, Date ).
Vers = 0:3:0,
Date = date(2017, 3, 11).

==
*/
b_real_version( 0:3:0, date(2017,3,11) ).

:- use_module( library(lib) ).
:- lib(source(b_real), homonyms(true)).

:- lib(real).
:- lib(stoics_lib).
:- lib(options).
:- lib(suggests(mtx)).

:- lib(options_rvar_rmv/2).
:- lib(mtx_pheatmap/2).
:- lib(mtx_column_pheatmap/3). % requires( library(mtx) ).
:- lib(lm_plot/3).
:- lib(pl_vector/3).
:- lib(r_mtx/3).
:- lib(r_data_frame/2).
:- lib(mtx_df/2).
:- lib(mtx_df/3).
:- lib(aheatmap/2).
:- lib(vector_scale/3).
:- lib(gg_bar_plot/2).
:- lib(columns_fisher_test/5).
:- lib(pl_plot_on/2).
:- lib(vectors_subed_gg_bar_plot/4).
:- lib(dot/3).
:- lib(r_unique/2).

:- lib(end(b_real)).
