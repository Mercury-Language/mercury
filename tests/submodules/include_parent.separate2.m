%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Used by use_submodule.m

:- module include_parent__separate2.

:- interface.

% The parent module includes io.

:- pred hello(io::di, io::uo) is det.

  :- module include_parent__separate2__nested.
  :- interface.
  :- pred hello(io::di, io::uo) is det.
  :- end_module include_parent__separate2__nested.

:- implementation.

hello(!IO) :-
    io.write_string("include_parent__separate2: hello\n", !IO).

  :- module include_parent__separate2__nested.
  :- implementation.
  hello(!IO) :-
    io.write_string("include_parent__separate2__nested: hello\n", !IO).
  :- end_module include_parent__separate2__nested.
