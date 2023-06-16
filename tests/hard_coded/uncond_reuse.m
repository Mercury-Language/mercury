%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that an unconditional reuse opportunity is actually taken.

:- module uncond_reuse.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

:- type f
    --->    f(int, int).

main(!IO) :-
    copy(f(1, 2), F0),
    addr(F0, Addr0),
    F0 = f(A, B),
    F = f(A + 1, B + 1),    % unconditional reuse here
    addr(F, Addr),

    ( if capable_grade($grade) then
        ( if Addr0 = Addr then
            io.write_string("same address (good)\n", !IO)
        else
            io.write_string("different addresses (bad)\n", !IO)
        )
    else
        io.write_string("grade probably doesn't support reuse\n", !IO)
    ).

% Only C grades for now.
:- pred capable_grade(string::in) is semidet.

capable_grade(Grade) :-
    string.prefix(Grade, Prefix),
    ( Prefix = "none"
    ; Prefix = "reg"
    ; Prefix = "jump"
    ; Prefix = "asm"
    ; Prefix = "fast"
    ; Prefix = "hl"
    ),
    not string.sub_string_search(Grade, "debug", _),
    not string.sub_string_search(Grade, "profdeep", _).

:- pred addr(T::in, int::out) is cc_multi.

:- pragma foreign_proc("C",
    addr(T::in, Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe, no_sharing],
"
    Addr = (MR_Word) T;
").
