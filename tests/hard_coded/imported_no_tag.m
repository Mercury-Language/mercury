% Test handling of imported no_tag types.
:- module imported_no_tag.
:- interface.

:- type printable(T) ---> class(pred(T)).
:- inst printable_data == bound(class(pred(in) is semidet)).
:- mode printable == in(printable_data).
:- mode new_printable == out(printable_data).

:- pred pwrite(printable(T)::printable, T::in) is semidet.

:- implementation.

pwrite(C, Data)  :-
 C = class(Test),
 call(Test, Data).

