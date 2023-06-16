%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module transitive_inst_type_helper_1.
:- interface.

:- import_module transitive_inst_type_helper_2.

:- type sequence == my_list(int).
:- inst sequence == my_list(ground).

:- pred sequence_length(sequence, int).
:- mode sequence_length(in(sequence), out) is det.

:- pred new_sequence(sequence).
:- mode new_sequence(out) is det.

:- implementation.

sequence_length(Seq, Len) :-
    length(Seq, Len).

new_sequence([]).
