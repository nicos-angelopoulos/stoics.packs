:- module( mlu, [
                    % k_fold_learn/5,
                    k_fold_pairwise_comparisons/6,
                    k_fold_pairwise_predictions/6,
                    k_fold_segments/4,
                    mlu_sample/4, mlu_sample/5,
                    mlu_frequency_plot/2,
                    mlu_errors/0,
                    mlu_version/2
    ] ).

:- use_module( library(lib) ).
:- lib(source(mlu),homonyms(true)).

:- lib(os_lib).
:- lib(debug_call).
:- lib(pack_errors).

:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:arity/3).
:- lib(stoics_lib:holds/2).

:- lib(k_fold_learn/5).
:- lib(k_fold_pairwise_comparisons/6).
:- lib(k_fold_pairwise_predictions/6).
:- lib(k_fold_segments/4).
:- lib(mlu_errors/0).
:- lib(mlu_sample/4).
:- lib(mlu_frequency_plot/2).
:- lib(end(mlu)).

/** <module> Machine learning utilities

A menagerie of machine learning utilities.

Currently implements k_fold learning and k_fold comparative performance plots via Real.

It is likely that bootstrapping will be added soon and also a couple of additional types of comparative plots.

---++ Pack info 

@author   nicos angelopoulos
@version    0.1 2016/3/5
@version    0.2 2017/3/11
@version    0.3 2021/12/31
@version    0.4 2022/1/2
@see http://stoics.org.uk/~nicos/sware/mlu
@see http://stoics.org.uk/~nicos/sware/mlu/doc/html/mlu.html
@see pack(mlu/examples/stoic.pl)

*/

/** mlu_version( -Version, -Date ).

Current version and release date for the library.

==
?- mlu_version( V, D ).
V = 0:4:0,
D = date(2022, 1, 2).
==
*/
mlu_version( 0:4:0, date(2022,1,2) ).
