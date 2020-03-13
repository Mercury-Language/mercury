%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test error messages for the `:- finalise finalpredname' directive.

:- module bad_finalise_decl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- finalise i2/2.

%---------------------------------------------------------------------------%

:- implementation.

:- finalise i1/2.
:- finalise i3/2.
:- finalise i4.
:- finalise i5/6.

:- pred i1(T::di, T::uo) is det.
i1(X, X).

:- pred i2(io::in, io::out) is det.
i2(!IO).

:- pred i4(io::di, io::uo) is det.
:- pred i5(io::di, io::uo) is det.

i4(!IO).
i5(!IO).

main(!IO) :-
    io.print("This is main/2.\n", !IO).

%---------------------------------------------------------------------------%
