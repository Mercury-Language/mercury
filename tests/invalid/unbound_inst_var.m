:- module unbound_inst_var.

:- interface.

:- import_module io.

:- pred main(io__state,io__state).
:- mode main(di,uo) is det.

:- implementation.

:- import_module char.

:- type all(X) ---> a(X) ; b ; c ; d.

:- inst all(X) ---> a(X) ; ground.

:- pred test(all(char)).
:- mode test(in(all(_))) is det.

test(_) :- true.

%:- pred main(io__state,io__state).
%:- mode main(di,uo) is det.

main(IO,IO) :-
  true.

:- end_module unbound_inst_var.
