
:- use_module(library(os_lib)).
:- use_module(library(options)).
:- use_module(library(by_unix)).

xgrep_defaults( [sub(true)] ).

/** xgrep(+Opts).

Extension grep,  

Opts
  * ext(Ext)
     extensions for files on which to grep 

  * sub(Sub=true)
     descent to subdirectories looking for matching files

  * pattern(Pat)
     grep pattern 

Depends on stoics libraries: options, os_lib and by_unix.

==
?- cd(pack(by_unix)).
true.

?- pwd.
% /usr/local/users/na11/local/git/lib/swipl-7.7.15/pack/by_unix/
?-  xgrep( [ext(pl),debug(true),pattern(library)] ).
% output as below:


> cd /usr/local/users/na11/local/git/lib/swipl-7.7.15/pack/by_unix/
> upsh xgrep ext=pl pattern=library debug=true

%  /home/na11/.rcpl compiled 0.00 sec, 8 clauses
% Grepping: 'scripts/xgrep.pl'
scripts/xgrep.pl::- use_module(library(os_lib)).
scripts/xgrep.pl::- use_module(library(options)).
scripts/xgrep.pl::- use_module(library(by_unix)).
....  % matches of these example outputs
% Grepping: pack.pl
% Grepping: 'prolog/by_unix.pl'
prolog/by_unix.pl::- ensure_loaded( library(process) ).
prolog/by_unix.pl::- ensure_loaded( library(debug) ).
prolog/by_unix.pl:This library provides primitives that allow programmers and users
prolog/by_unix.pl:The library uses two local flags if they are defined.
prolog/by_unix.pl:The simplest example of how to use the library is:
prolog/by_unix.pl:?- [library(by_unix)].
prolog/by_unix.pl:@tbd pick process_create options from the library declarations

==

@author nicos angelopoulos
@version  0.1 2018/7/23
@see http://stoics.org.uk/~nicos/sware
@see http://www.swi-prolog.org/pack/list

*/
xgrep( Args ) :-
    options_append( xgrep, Args, Opts ),
    findall( Ext, member(ext(Ext),Opts), Exts ),
    options( sub(Sub), Opts ),
    options( pattern(Pat), Opts ),
    xgrep_pattern( Pat, Exts, Sub ).

xgrep_pattern( Pat, Exts, Sub ) :-
    os_file( File, sub(Sub) ),
    os_ext( OsExt, File ),
    memberchk( OsExt, Exts ),
    debug( xgrep, 'Grepping: ~p', File ),
    catch( @ grep( -'H', Pat, File ), _, fail ),
    fail.
xgrep_pattern( _Pat, _Exts, _Sub ).
