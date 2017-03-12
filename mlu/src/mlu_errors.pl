
/** mlu_errors.

Documentation predicate.

Pack mlu uses pack pack_errors for throwing errors. 

File mlu_errors.pl defines local errors within the pack_errors infrastructure.
*/

mlu_errors.

:- multifile( pack_errors:message/3 ).

pack_errors:message( fold_data_insufficient(Dlen,N) ) -->
	['Insufficient length of data (~d) as ~d folds are required'-[Dlen,N]].
pack_errors:message( fold_data_residual(Dlen) ) -->
	['Residual data of length: ~d while splitting folds'-[Dlen]].
pack_errors:message( learners_predictors_unequal_length(Llen,Plen) ) -->
	['Learners and predictors have different lengths: ~d and ~d respectively'-[Llen,Plen]].

/*
pack_errors:message( missing_alias(Os) ) -->
	['OS entity: ~w, looks like aliased but alias does not exist.'-[Os]].

pack_errors:message( os_postfix_lists(Sc,Fr) ) -->
	['os_postfix/4: only one of args 2 and 4 should be a list (options).\nFound,\n2nd: ~w\n4th:~w'-[Sc,Fr]].
	*/
