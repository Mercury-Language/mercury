%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_selection.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module require.

% Currently (May 2001) we don't pass this test case,
% because the compiler's expression flattening puts
% the sub-goals in top-down order, rather than
% (as the language reference manual requires)
% ordering them bottom-up. This means that the call to
% func2 gets flattened as
%   { V_1 = func2(In, V_2) },
%   { V_2 = In },
%   { print(V_1) }
% rather than as
%   { V_1 = func2(In, V_2) },
%   { V_2 = In },
%   { print(V_1) }
% which causes mode analysis to select the wrong mode in the
% call to func2, which in turn causes a determinism error.
%
% The rationale for keeping the current behaviour is that
% the naive fix of just flattening to bottom-up order
% causes performance problems in type checking, in particular
% when compiling compiler/options.m.
% Eventually we ought to change the type checker to use
% a different algorithm that doesn't have this performance
% problem, then we can fix flattening, and this test case
% will then pass.

main(!IO) :-
    In = 42,
    io.print_line(func2(In, In), !IO).

:- func func2(int, int) = string.
:- mode func2(in, in) = out is det.
:- mode func2(in, out) = out is det.
:- mode func2(out, in) = out is det.
:- mode func2(out, out) = out is det.

func2(_, _) = _ :-
    error("called func2/2").
