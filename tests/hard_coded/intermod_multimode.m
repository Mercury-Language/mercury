%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module intermod_multimode.
:- interface.

:- func func0 = string.
:- mode func0 = out is det.

:- func func1(int) = string.
:- mode func1(in) = out is det.
:- mode func1(out) = out is det.

:- func func2(int, int) = string.
:- mode func2(in, in) = out is det.
:- mode func2(in, out) = out is det.
:- mode func2(out, in) = out is det.
:- mode func2(out, out) = out is det.

:- impure pred test0.
:- mode test0 is det.

:- impure pred test1(int).
:- mode test1(in) is det.
:- mode test1(out) is det.

:- impure pred test2(int, int).
:- mode test2(in, in) is det.
:- mode test2(in, out) is det.
:- mode test2(out, in) is det.
:- mode test2(out, out) is det.

:- impure pred puts(string::in) is det.

:- type determinism
    --->    det
    ;       semidet
    ;       cc_multi
    ;       cc_nondet
    ;       multi
    ;       nondet
    ;       erroneous
    ;       failure.

:- pred get_determinism(pred(T), determinism).
:- mode get_determinism(pred(out) is det,     out(bound(det)))     is det.
:- mode get_determinism(pred(out) is semidet, out(bound(semidet))) is det.
:- mode get_determinism(pred(out) is multi, out(bound(multi)))     is det.
:- mode get_determinism(pred(out) is nondet, out(bound(nondet)))   is det.
:- mode get_determinism(pred(out) is cc_multi, out(bound(cc_multi))) is det.
:- mode get_determinism(pred(out) is cc_nondet, out(bound(cc_nondet))) is det.

:- implementation.

func0 = ("func0 = out" :: out).

:- pragma promise_pure(func1/1). % XXX technically this is a lie
func1(_::in) = ("func1(in) = out"::out).
func1(0::out) = ("func1(out) = out"::out).

:- pragma promise_pure(func2/2). % XXX technically this is a lie
:- pragma inline(func2/2).
func2(_::in, _::in) = (R::out) :-
    R = "func2(in, in) = out".
func2(_::in, 0::out) = (R::out) :-
    R = "func2(in, out) = out".
func2(0::out, _::in) = (R::out) :-
    R = "func2(out, in) = out".
func2(0::out, 0::out) = (R::out) :-
    R = "func2(out, out) = out".

test0 :-
    impure puts("test0").

test1(_::in) :-
    impure puts("test1(in)").
test1(0::out) :-
    impure puts("test1(out)").

:- pragma inline(test2/2).
test2(_::in, _::in) :-
    impure puts("test2(in, in)").
test2(_::in, 0::out) :-
    impure puts("test2(in, out)").
test2(0::out, _::in) :-
    impure puts("test2(out, in)").
test2(0::out, 0::out) :-
    impure puts("test2(out, out)").

:- pragma foreign_proc("C",
    puts(S::in),
    [will_not_call_mercury],
"
    puts(S)
").
:- pragma foreign_proc("C#",
    puts(S::in),
    [],
"
    System.Console.WriteLine(S);
").
:- pragma foreign_proc("Java",
    puts(S::in),
    [will_not_call_mercury],
"
    System.out.println(S);
").

:- pragma promise_pure(get_determinism/2).
:- pragma inline(get_determinism/2).
get_determinism(Pred::(pred(out) is det), Det::out(bound(det))) :-
    get_determinism_2(Pred, Det).
get_determinism(Pred::(pred(out) is semidet), Det::out(bound(semidet))) :-
    get_determinism_2(Pred, Det).
get_determinism(_Pred::(pred(out) is cc_multi), cc_multi::out(bound(cc_multi))).
get_determinism(_Pred::(pred(out) is cc_nondet), cc_nondet::out(bound(cc_nondet))).
get_determinism(_Pred::(pred(out) is multi), multi::out(bound(multi))).
get_determinism(_Pred::(pred(out) is nondet), nondet::out(bound(nondet))).

:- inst bound_to_det == bound(det).
:- inst bound_to_semidet == bound(semidet).

:- pred get_determinism_2(pred(T), determinism).
:- mode get_determinism_2(pred(out) is det,     out(bound_to_det))     is det.
:- mode get_determinism_2(pred(out) is semidet, out(bound_to_semidet)) is det.
:- pragma promise_pure(get_determinism_2/2).
:- pragma inline(get_determinism_2/2).

get_determinism_2(_Pred::(pred(out) is det), det::out(bound_to_det)).
get_determinism_2(_Pred::(pred(out) is semidet),
        semidet::out(bound_to_semidet)).
