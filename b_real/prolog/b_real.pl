:- module( b_real, [
                    b_real/0, b_real_version/2,
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
                    r_hist/2,              % +Vect, +Opts 
                    columns_fisher_test/5,
                    dot/3,
                    mtx_df/2, mtx_df/3,
                    quantiles/3
                ] 
    ).


:- use_module( library(lib) ).
:- lib(source(b_real), homonyms(true)).

:- lib(real).
:- lib(stoics_lib).
:- lib(options).
:- lib(suggests(mtx)).

:- lib(b_real/0).
:- lib(b_real_version/2).
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
:- lib(r_hist/2).
:- lib(quantiles/3).

:- lib(end(b_real)).
