:- module aditi_state_errors.

:- interface.

:- import_module aditi.

:- pred no_aditi_state(int::out, int::out) is det.
:- pragma base_relation(no_aditi_state/2).

:- pred output_aditi_state(aditi__state::out, int::out, int::out) is det.
:- pragma base_relation(output_aditi_state/3).

