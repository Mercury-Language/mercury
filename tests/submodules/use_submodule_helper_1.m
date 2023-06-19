%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Used by use_submodule.m

:- module use_submodule_helper_1.

:- interface.

:- import_module io.
:- include_module use_submodule_helper_2, use_submodule_helper_3.

:- pred hello(io::di, io::uo) is det.

  :- module use_submodule_helper_1.nested.
  :- interface.
  :- pred hello(io::di, io::uo) is det.
  :- end_module use_submodule_helper_1.nested.

:- implementation.

hello(!IO) :-
    io.write_string("use_submodule_helper_1: hello\n", !IO).

%---------------------------------------------------------------------------%

    :- module use_submodule_helper_1.nested.
    :- implementation.

    hello(!IO) :-
        io.write_string("use_submodule_helper_1__nested: hello\n", !IO).

    :- end_module use_submodule_helper_1.nested.

%---------------------------------------------------------------------------%
