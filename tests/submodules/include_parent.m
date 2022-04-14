%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Used by use_submodule.m

:- module include_parent.

:- interface.

:- import_module io.
:- include_module separate, separate2.

:- pred hello(io::di, io::uo) is det.

  :- module include_parent.nested.
  :- interface.
  :- pred hello(io::di, io::uo) is det.
  :- end_module include_parent.nested.

:- implementation.

hello(!IO) :-
    io.write_string("include_parent: hello\n", !IO).

%---------------------------------------------------------------------------%

:- module include_parent.nested.
:- implementation.

hello(!IO) :-
    io.write_string("include_parent__nested: hello\n", !IO).

:- end_module include_parent.nested.

%---------------------------------------------------------------------------%
