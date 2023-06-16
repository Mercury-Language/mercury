%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module transitive_inst_type.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module transitive_inst_type_helper_1.

main(!IO) :-
    new_sequence(Seq),
    sequence_length(Seq, Size),
    io.write_int(Size, !IO),
    io.nl(!IO).
