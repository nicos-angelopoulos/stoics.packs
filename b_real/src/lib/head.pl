%% head( +List, -Head ).
%% head( +List, -Head, -Tail ).
%
% Head is the first element of the non var List and
% Tail is its remainder.
%
% head/3 is useful for chopping heads off.
%
% ==
%   maplist( head, [[1,2,3],[4,5,6]], Heads, Tails ).
%
%  Heads = [1, 4],
%  Tails = [[2, 3], [5, 6]].
%   
% ==
%
head( List, Head ) :-
	\+ var( List ),
	List = [Head|_].

head( List, Head, Tail ) :-
	\+ var( List ),
	List = [Head|Tail].
