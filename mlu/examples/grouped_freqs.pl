:- use_module( library(mlu) ).

/** grouped_freqs.

Test predicate for mlu_frequency_plot/2.

Displays a 9 bar plot grouped by colour of the bars according to 
population. 

Assumes packs Real and b_real are installed.

@author nicos angelopoulos
@version  0.1 2017/1/17
*/
grouped_freqs :-
    findall( Atm-Tms, ( between(1,9,I),
                        Code is 64 + I,
                        atom_codes(Atm,[Code]),
                        Tms is I 
                      ),
                         Pairs ),
    Opts = [sort(true),pop_breaks([3,5]),level_colours_title(bands)],
    mlu_frequency_plot( Pairs, Opts ).
