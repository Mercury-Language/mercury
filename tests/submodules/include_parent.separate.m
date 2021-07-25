%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Used by use_submodule.m

:- module include_parent__separate.

:- interface.

% The parent module includes io.

:- pred hello(io::di, io::uo) is det.

:- pred hello2(io::di, io::uo) is det.

  :- module include_parent__separate__nested.
  :- interface.
  :- pred hello3(io::di, io::uo) is det.
  :- end_module include_parent__separate__nested.

:- implementation.

hello(!IO) :-
    io.write_string("include_parent__separate: hello\n", !IO).

hello2(!IO) :-
    io.write_string("include_parent__separate: hello2\n", !IO).

  :- module include_parent__separate__nested.
  :- implementation.
  hello3(!IO) :-
    io.write_string("include_parent__separate__nested: hello\n", !IO).
  :- end_module include_parent__separate__nested.
