:- module transitive_inst_type2.
:- interface.

:- import_module transitive_inst_type3.

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
