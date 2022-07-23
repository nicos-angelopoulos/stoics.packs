:- module( by_unix, [    
					(@)/1,
					(&)/1,
					(@@)/2,
					(@@)/3,
					which/2,
					cd/2,
					by_unix_retract/0,
					by_unix_assert/0,
					by_unix_version/2,
					by_unix_term_to_serial/2,
					op( 200, fy, @ ),
					op( 200, fy, & ),
					op( 200, yfx, @@ ),
					op( 200, fy, -- ),
					op( 400, fx, / ),
					op( 400, fx, './' ),
   					op( 400, fx, '../' ),
					op( 600, yfx, '/../' )
				] ).

:- use_module(library(lists)).      % flatten/2, ...
:- use_module(library(apply)).      % partition/4, maplist/3, ...
:- use_module(library(debug)).
:- use_module(library(process)).
:- use_module(library(readutil)).   % read_line_to_codes/2.

/** <module> by_unix

An elegance layer to calling unix commands.

This library provides primitives that allow programmers and users
to embed calls to process_create/3. The aim is to keep application
code clear and succinct. This is achieved by (a) reducing the call to
process_create/3 to its essential constituents and (b) allowing for 
term structures in the arguments of the call. 

The library uses two local flags if they are defined.

==
    current_prolog_flag( by_unix_shell, Shell ).
    current_prolog_flag( by_unix_shell_com_arg, CmA ).
==

Shell should be a Unix shell such as _tcsh_ and CmA argument should be the 
shell's way of saying that what ever follows is for shell to execute.
When Shell is present it is used to start the commands; this allows
for including user personalisations via their usual shell.
When CmA is missing, -c is used (set it to '' for none at all).

On loading, the file ~/.pl/by_unix.pl will be consulted if it exists.
You can add user preferences there, such as 

	:- set_prolog_flag( by_unix_shell, tcsh ).

The simplest example of how to use the library is:

== 
  ?- @ ls.
==
This lists all files by translating _ls_ to process_create( path(ls), [], [] ).

To list all details (long list): 
==
	?- @ ls(-l).
==

which demonstrates translation of terms in arguments. 

By_unix looks at the arguments of the terms right of an @ to decide which ones are options
(3rd argument of process_create/3) assuming the rest to be arguments to the call (2nd argument of process_create/3).
Command arguments can be terms which will be serialised, so that a/b becomes 'a/b'. 
The argument * is special and it is expanded fed into expand_file_name/2.

With SWI 7 you can now have '.' in atoms, making interactions with the OS even smoother. 

==
?- @ mkdir( -p, /tmp/test_by_unix ).
?- @ cd( /tmp/test_by_unix ).
?- @ touch( empty.pl ).
?- @ rm( -f, empty.pl ).

?- @ cd( pack(by_unix) ).
?- Wc @@ wc( -l, pack.pl ).

Wc = ['10 pack.pl'].

?- @ cd( @ '$HOME' ).
?- [Pwd] @@ pwd.
Pwd = '/home/nicos'.


?- Pass @@ bash( -c, 'read -s -p password: pass; echo $pass' ), nl.
% Sending, name: bash, args: [-c,read -s -p password: pass; echo $pass], opts:[stdout(pipe(_G1360))].
password:
Pass = [word].

==

The main objective of by_unix is achieved by what has been described so far. 
We have found that more than 90 percent of the its uses are to produce elegant Goals that 
are clear to read and construct their arguments within Prolog. 
We provide some more features, which are described in what follows, but they should be seen as marginal.

Changing directory is not supported via cd in process_create as =|which cd|= fails to return
an executable in bash. That is, 

== 
?- process_create( path(cd), ['/'], [] ).
ERROR: source_sink `path(cd)' does not exist
==

As oppose to: 

==
?- [library(by_unix)].

?- @ cd( / ).
true.

?- @ ls.
bin   dev  home        initrd.img.old  lib64	   media  opt	root  sbin     srv  tmp  var
boot  etc  initrd.img  lib	       lost+found  mnt	  proc	run   selinux  sys  usr  vmlinuz
true.

?- @ cd( /home/nicos ).
?- @ cd( pack(by_unix) ).

== 

which/2 provides a locator for executables. by_unix_term_to_serial/2 serialises Prolog terms
to process_create/3 atoms, and by_unix_assert/0 allows for doing away with the @.

As process_create/3 quotes special characters, for instance 
==
?- process_create( path(ls), ['*'], [] ).
/bin/ls: cannot access *: No such file or directory
ERROR: Process "/bin/ls": exit status: 2
==

By_unix allows for in-argument file name expansions via expand_file_name/2. 

?- @ ls( -l, @('*.pl') ).

@@/2 provides dual functionality: either picking up output lines from the calling command
or for maplist/2 applications. See @@/2 and @@/3.

 A lines example 
==
?- @ cd( @ '$HOME' ).
?- Pwd @@ pwd.
Pwd = ['/home/nicos'].
==

 A maplist example 
==
?- @ cd( pack(by_unix) ).
?- Files = ['prolog/by_unix.pl'], maplist( @@(wc(-l)), Files, Wcs ).

==

As of version 0.1.7 (a) failure to locate a suitable executable results to an informational message,
(b) non-zero exit on the process_create/3 does no longer produce an error, but an informational message
followed by failure.

==
?- @what(else).
% No 'what' executable could be found.
false.

?- @ ls(who).
/bin/ls: cannot access 'who': No such file or directory
% Failed shell call: ls with args: [who].
false.
==

@author Nicos Angelopoulos
@version 0.1.7   2014/06/09
@version 0.2     2020/09/18
@see http://stoics.org.uk/~nicos/sware/by_unix
@tbd error handling via message/3.
@tbd aliases in .pl/by_unix.pl
@tbd pick process_create options from the library declarations
@tbd ?- Files = ['by_unix.pl'], maplist( @@ wc(-l) , Files, Wcs ).  ie. fix syntax error on this

*/

%% @@( -Lines, +Comm ).
%% @@( +Comm, -Arg ).
%
%  If first argument is a variable or list, it is interpretted to be the Lines invocation,
%  Output from Comm instantiated in Lines. Attach Arg to Comm before processing as a 
%  Unix command.
%
@@(Lines,Goal) :-
	once( var(Lines); is_list(Lines) ),
	!,
	by_unix_separate( Goal, Name, TArgs, Opts ),
	\+ memberchk( stdout(_), Opts  ),
	unix_process( Name, Goal, TArgs, [stdout(pipe(Out))|Opts] ),
	read_lines(Out, Lines),
	close( Out ).
@@(Goal,Arg) :-
	by_unix_separate( Goal, Name, Args, Opts ),
	by_unix_term_to_serial( Arg, Serial ),
	to_list( Serial, Serials ),
	append( Args, Serials, All ),
	% process_create( path(Name), All, Opts ).
	unix_process( Name, Goal, All, Opts ).
% this is suitable for meta calls, with output
%% @@( +Comm, +Arg, -Lines ).
%
% Attach Arg to Comm before processing as a Unix command and provide output to Lines.
% Works with maplist/3 but Lines will be triply nested.
%
@@(Goal,Arg,Lines) :-
	by_unix_separate( Goal, Name, Args, Opts ),
	by_unix_term_to_serial( Arg, Serial ),
	to_list( Serial, Serials ),
	append( Args, Serials, All ),
	Aug = [stdout(pipe(Out))|Opts],
	unix_process( Name, Goal, All, Aug ),
	% process_create( path(Name), All, [stdout(pipe(Out))|Opts] ),
	read_lines(Out, Lines ),
	close( Out ).

%% @(+Goal).
%
%  This is the main predicate of by_unix.
%  See module documentation for examples. 
%
%  For @cd( Arg ) see documentation of cd/2.
% ==
%    ?- @ mkdir( -p, /tmp/test_by_unix ).
%    ?- @ cd( /tmp/test_by_unix ).
%    ?- @ touch( empty.pl ).
%    ?- @ rm( -f, empty.pl ).
% ==
% 
@(Goal) :-
	by_unix_separate( Goal, Name, TArgs, Opts ),
	unix_process( Name, Goal, TArgs, Opts ).

%% &(+Goal).
%
%  As @(Goal) but runs process in the background.
%  This should be achivable via detached(true) option of process_create/3
%  but this does not seem to work on linux (tested on Mint 16).
%  Here we use an (experimental) implementation based on threads.
%==
%==
% 
&(Goal) :-
	by_unix_separate( Goal, Name, TArgs, Opts ),
	Gcreate = unix_process_thread( Name, Goal, TArgs, Opts ),
	thread_create( Gcreate, Id, [] ),
	sleep( 1 ), % is this an SWI bug ?
	thread_signal( Id, thread_exit(exited) ).

by_unix_separate( Goal, Name, TArgs, Opts ) :-
	% Goal =.. [Name|GArgs],
	compound( Goal, Name, GArgs ),
	% which_cmd( Name ),
	( which(Name,_Wch) ->
        true
        ;
        by_unix_message( exec_miss(Name) ),
        fail
    ),
	partition( pc_option, GArgs, Opts, ArgsNest ),
    flatten( ArgsNest, Args ),
	maplist( by_unix_term_to_serial, Args, NesTArgs ),
	flatten( NesTArgs, TArgs ).

which_cmd( _Name ) :-
	current_prolog_flag( by_unix_shell, _ ),
	% fixme: for now don't check, but we will probably have to
	% fix the general call for shelled executions anyway, so hook to that.
	!.
which_cmd( Name ) :-
	which( Name, _Wch ). %fixme add error

unix_process( Cd, Goal, [_Arg], [] ) :-
	Cd == cd,
	!,
	arg( 1, Goal, Garg ),
	cd( Garg ).
unix_process( Name, _Goal, Args, Opts ) :-
	current_prolog_flag( by_unix_shell, ByShell ),
	!,
	abs_shell_location( ByShell, Shell ),
	shell_process( Shell, Name, Args, Opts ).
unix_process( Name, _Goal, Args, Opts ) :-
	debug( by_unix, 'Sending, name: ~w, args: ~w, opts:~w.', [Name,Args,Opts] ),
	catch( process_create( path(Name), Args, Opts ), _, fail),
    !.
unix_process( Name, _Goal, _Args, _Opts ) :-
    AbsOpts = [access(execute),file_errors(fail)],
    \+ absolute_file_name( path(Name), _AbsExec, AbsOpts ),
    by_unix_message( exec_miss(Name) ),
    !,
    fail.
unix_process( Name, _Goal, Args, _Opts ) :-
    by_unix_message( exec_fail(Name,Args) ),
    fail.
    
unix_process_thread( Name, Goal, TArgs, Opts ) :-
	unix_process( Name, Goal, TArgs, Opts ),
	uniprocess_thread_loop.

uniprocess_thread_loop :-
	sleep( 1 ),
	uniprocess_thread_loop.


shell_process( Shell, Name, Args, Opts ) :-
	current_prolog_flag( by_unix_shell_com_arg, CmA ),
	!,
	shell_arged_process( CmA, Shell, Name, Args, Opts ).
shell_process( Shell, Name, Args, Opts ) :-
	shell_arged_process( '-c', Shell, Name, Args, Opts ).

% untested- i don't know any shell that does this.
shell_arged_process( '', Shell, Name, Args, Opts ) :-
	!,
	shelled_name_args( Name, Args, Nargs ),
	% send_process( Shell, NArgs, Opts ).
	Process =.. [Shell|Nargs],
	send_process( Process, Opts ).

shell_arged_process( CmA, Shell, Name, Args, Opts ) :-
	shelled_name_args( Name, Args, Nargs ),
	% send_process( Shell, [CmA|NArgs], Opts ). % see if we need ""
	Process =.. [Shell,CmA|Nargs],
	send_process( Process, Opts ).

send_process( Process, Opts ) :-
	debug( by_unix, 'Sending process:process_create, 1: ~w, 2: ~w', [Process,Opts] ),
	process:process_create( Process, Opts ).

% temporary?
shelled_name_args( Name, Args, [Narg] ) :-
	maplist( dquote, Args, DqArgs ),
	atomic_list_concat( [Name|DqArgs], ' ', Narg ).

dquote( X, Dq ) :-
	atomic_list_concat( ['"',X,'"'], Dq ).

/** which( +Which, -This ).

	Expand Which as a unix command in the path and return its absolute_file_name/3 in This.
	When Which is =|cd|=, variable This is also bound to =|cd|=. cd is handled separately as =|which cd|= 
	fails in bash, as does process_create(path(cd), ['/'], [] ).

*/
which( Which, This ) :- 
	Which == cd,
	!, 
	This = cd.
% which( which, which ) :- !.
which( Which, This ) :-
	current_prolog_flag( by_unix_shell, ByShell ),
	!,
	abs_shell_location( ByShell, Shell ),
	by_unix_shell_com_args( Args ),
	shelled_name_args( which, [Which], WchWhich ),
	append( Args, WchWhich, WchArgs ),
	Proc =.. [Shell|WchArgs],
	process:process_create( Proc, [stdout(pipe(Out))] ),
	read_lines(Out, Lines),
	close( Out ),
	Lines = [This|_].

which( Which, This ) :-
	by_which( Which, This ).

by_which( Which, This ) :-
     absolute_file_name( path(Which), This,
			 [ extensions(['',exe]),
			   file_errors(fail),
			   access(exist) % shouldn't this be execute ?
			 ] ).  % does not succeed for built-ins !!!

by_unix_shell_com_args( Args ) :-
	current_prolog_flag( by_unix_shell_com_arg, CmA ),
	!,
	by_unix_shell_com_arg_args( CmA, Args ).
by_unix_shell_com_args( ['-c'] ).

by_unix_shell_com_arg_args( '', [] ) :- !.
by_unix_shell_com_arg_args( CmA, [CmA] ).

pc_option( Term ) :-
    \+ is_list( Term ),
	compound( Term, Name, Args ),
	length( Args, 1 ),
	% functor( Term, Name, 1 ),
	known_pc_options( OptNames ), %fixme:
	memberchk( Name, OptNames ).

%% by_unix_term_to_serial( Term, Serial ).
%
%  Term is serialised into an atomic Serial. When Term is *, Serial is the list of current files,
%  while when Term = @ Atom, Serial is the result of applying expand_file_name( Atom, Serial ).
%
by_unix_term_to_serial( *, Files ) :-
	!,
	expand_file_name('*',Files).
by_unix_term_to_serial( @(Inner), Files ) :-
	by_unix_term_to_serial( Inner, Atit ),
	!,
	expand_file_name(Atit,Files).

by_unix_term_to_serial( Term, Serial ) :-
	with_output_to( atom(Serial), write_term(Term,[quoted(false)]) ).

%% by_unix_version( -Version, -Date ).
%
%  Provides version and date of current release.
%
%==
% by_unix_version( 0:1:6, date(2013,12,26) ). 
%==
%
%@author nicos angelopoulos
%@version  0:2 2020/9/18
%
by_unix_version( 0:2:0, date(2014,6,9) ).

%% by_unix_retract.
%
%  Retract all user:goal_expansion(_,_). 
%  
by_unix_retract :-
	Head = user:goal_expansion(_,_),
	retractall( Head ).

%% by_unix_assert.
%
%   Allows for goal expansion of Com to @ Com, when Com is a which-able Unix command.
%   
% ==
%   ?- by_unix_asert. 
%   ?- mkdir( -p, /tmp/test_by_unix ).
%   ?- cd( /tmp/test_by_unix ).
%   ?- touch( empty.pl ).
%   ?- ls( -l ).
%   ?- rm( -f, empty.pl ).
%   ?- ls( -a ).
% ==
by_unix_assert :-
	Head = user:goal_expansion(Term1,Term2),
	Body = (  (atomic(Term1) -> UnixCom = Term1
				; compound_name_arity(Term1,UnixCom,_Arity) ),
			which(UnixCom,_),
			Term2= @(Term1)
		  ),
	assert( (Head :- Body) ).

known_pc_options( [stdin,stdout,stderr,cwd,env,process,detached,window] ).

%% cd( +New ).
%% cd( -Old, +New ).
%
%  Similar to working_directory/2, but in addition New can be 
%  a search path alias or the 1st argument of absolute_file_name/2.
%  This is also the case when @ cd( New ) is called.
% 
% == 
% ?- @ cd( pack ).
% true.
% 
% ?- @ pwd.
% /usr/local/users/nicos/local/git/lib/swipl-7.1.4/pack
% true.
% 
% ?- @ cd( @ '$HOME' ).
% true.
% 
% ?- @ pwd.
% /home/nicos
% true.
% == 
% 
cd( Old, New ) :-
	working_directory( Old, Old ),
	cd( New ).

cd( Spec ) :-
	atomic( Spec ),
	expand_file_name( Spec, [Fst|_] ),
	catch(absolute_file_name(Fst,Dir),_,fail),
	exists_directory( Dir ),
	!,
	working_directory( _, Dir ).
cd( Dir ) :-
	ground( Dir ),
	cd_ground( Dir ),
	!.
cd( Atom ) :-
	atom( Atom ),
	!,
	working_directory( _, Atom ).
cd( Term ) :-
	compound( Term ),
	by_unix_term_to_serial( Term, Serial ),
	to_list( Serial, [Dir|_T] ), %fixme: warn if T \== [] 
	working_directory( _, Dir ).

cd_ground( Dir ) :-
	user:file_search_path( Dir, TermLoc ),
	expand_file_search_path( TermLoc, Loc ),
	exists_directory( Loc ),
	working_directory( _, Loc ).
cd_ground( Dir ) :-
	Opts = [file_type(directory),file_error(fail),solutions(first),access(execute)],
	catch( absolute_file_name(Dir,Loc,Opts), _, fail ),
	working_directory( _, Loc ).

abs_shell_location( ByShell, Shell ) :-
	exists_file( ByShell ),
	access_file( ByShell, execute ),
	!,
	debug( by_unix, 'Using shell given by absolute path, at:~w', ByShell ),
	Shell = ByShell.
abs_shell_location( ByShell, Shell ) :-
	by_which( ByShell, Shell ),
	debug( by_unix, 'Using shell given by relative:~w, locate by which , at:~w', [ByShell,Shell] ),
	!.
% fixme: add errors like Real
abs_shell_location( ByShell, _Shell ) :-
	debug( by_unix ),
	debug( by_unix, 'Cannot locate shell given by relative:~w', [ByShell] ),
	fail.

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).
read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).

compound( Term, Name, Args ) :-
	current_predicate( compound_name_arguments/3 ),
	compound( Term ),  % in real this is after the cut, here we are less strict we allows ls = ls()
	!,
	compound_name_arguments( Term, Name, Args ).
compound( Term, Name, Args ) :-
	Term =.. [Name|Args].

to_list( Serial, Serials ) :-
	is_list( Serial ),
	!,
	Serial = Serials.
to_list( Serial, [Serial] ).

by_unix_load_user_file :-
	expand_file_name( '~/.pl/by_unix.pl', [ByUnix] ),
	exists_file( ByUnix ),
	ensure_loaded( ByUnix ),
	!.
by_unix_load_user_file.

                 /*******************************
                 *            MESSAGES          *
                 *******************************/
% These print messages that are always on.
% Different colour to debugging is used by the system (when colour in terminal is enabled).
%
by_unix_message( Mess ) :-
    print_message( informational, by_unix(Mess) ).
    
:- multifile prolog:message//1.

prolog:message(by_unix(Message)) -->
    message(Message).

:- discontiguous
    message//1.

message( exec_miss(Exec) ) -->
    ['No \'~w\' executable could be found.'-[Exec] ].
message( exec_fail(Exec,Args) ) -->
    ['Failed shell call: ~w with args: ~w.'-[Exec,Args] ].

:- initialization( by_unix_load_user_file, now ).
