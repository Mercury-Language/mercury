% This is a regression test for two bugs.
%
% The first bug is that earlier versions of the compiler did not make sure
% that the sets of variables in resume_points satisfy the requirements of
% typeinfo liveness (i.e. they included variables in the resume point set
% without also including the typeinfos describing their types). This lead
% to a compiler abort when compiling with options (e.g. --trace deep)
% that imply typeinfo liveness.

% The second bug is that stack_layout.m, when constructing the list of tvars
% live at a label, would forget to include tvar N in this list if tvar N-1
% isn't live at that label. Since in introduce_new_typeinfo/2 below, tvar U
% is numbered 1 while tvar T is numbered 2, and at the call event only T is
% live, the layout for the call event was incomplete, leading to an internal
% error when stopping at this event.

:- module resume_typeinfos.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list.

main -->
	( { test([1, 2], Result) } ->
		io__write_int(Result),
		io__write_string("\n")
	;
		io__write_string("no solution.\n")
	).

:- some [U] pred introduce_new_typeinfo(list(T)::in, list(U)::out) is det.
:- pragma no_inline(introduce_new_typeinfo/2).

introduce_new_typeinfo(_, ["fortytwo"]).

:- pred test(list(T)::in, int::out) is semidet.

test(TestList, Result) :-
	introduce_new_typeinfo(TestList, NewList),
	(
		list__length(TestList, Length),
		Length > 5
	->
		Result = 10
	;
		% The code here does not need the typeinfo for the
		% elements of NewList, but typeinfo liveness requires
		% this typeinfo to be in the resume point established
		% for the condition, since the debugger may need it to
		% print the value of NewList at the else event.

		NewList = [],
		Result = 42
	).
