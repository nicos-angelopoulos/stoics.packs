% b_errors.
% place here all pack_errors:message//1 for the library.
b_errors.


:- multifile( pack_errors:message/3 ).

pack_errors:message( too_few_class_colors(ClrsLen,ClassesLen) ) -->
     ['There are not enough colours: ~d, for the existing classes: ~d' - [ClrsLen,ClassesLen] ].
pack_errors:message( mis_classes_in_cvs(Cs,Classes) ) -->
     ['Internal error, accumulator returned classes: ~w, expected: ~w' - [Cs,Classes] ].
