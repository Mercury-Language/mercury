%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module heap_ref_mask_tag.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type length
    --->    points(float)
    ;       font_em(float)
    ;       font_ex(float)
    ;       pixels(float).

%---------------------------------------------------------------------------%

main(!IO) :-
    X = pixels(1.0),
    F = magnitude(X),
    write_float(F, !IO),
    io.nl(!IO).

:- func magnitude(length) = float.
:- pragma no_inline(func(magnitude/1)).

% dupelim.m will combine the following cases, which differ only in the tag to
% remove, into a single code sequence. We must be careful to mask off the tag
% bits in the combined code sequence.

magnitude(points(M)) = M.
magnitude(font_em(M)) = M.
magnitude(font_ex(M)) = M.
magnitude(pixels(M)) = M.
