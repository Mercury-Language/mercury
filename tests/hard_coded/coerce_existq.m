%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_existq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module type_desc.

:- type foo
    --->    foo(int)
    ;       foo2.

:- type maybe_box
    --->    some [A, B, C, D] box(foo, A, B, C, D) => (tc(B), tc(C))
    ;       none.

:- type box =< maybe_box
    --->    some [X3, X2, X1, X0] box(foo, X3, X2, X1, X0) => (tc(X2), tc(X1)).

:- typeclass tc(T) where [
    func reverse(T) = string
].
:- instance tc(float) where [
    reverse(X) = reverse_string(string.from_float(X))
].
:- instance tc(string) where [
    reverse(X) = reverse_string(X)
].

:- func reverse_string(string) = string.

reverse_string(S) =
    string.from_char_list(list.reverse(string.to_char_list(S))).

main(!IO) :-
    Zero = foo(0),
    A0 = 'new box'(Zero, 1, "two", 3.333, {"four", 5}) : maybe_box,
    A = coerce(A0),
    print_box(A, !IO),
    call_methods(A, !IO),

    % Same but with 'ground' arguments, instead of bound.
    Int = string.det_to_int("1"),
    Str = reverse_string("owt"),
    B0 = 'new box'(Zero, Int, Str, 3.333, {"four", 5}) : maybe_box,
    B = coerce(B0),
    print_box(B, !IO),
    call_methods(B, !IO).

%---------------------------------------------------------------------------%

:- pred print_box(box::in, io::di, io::uo) is det.

print_box(X, !IO) :-
    deconstruct(X, do_not_allow, _Functor, Arity, _Args),
    io.print_line(X, !IO),
    print_args(X, 0, Arity, !IO),
    io.nl(!IO).

:- pred print_args(T::in, int::in, int::in, io::di, io::uo) is det.

print_args(X, Index, Arity, !IO) :-
    ( if Index < Arity then
        ( if arg(X, do_not_allow, Index, Arg) then
            TypeDesc = type_of(Arg),
            io.format("    arg %d: ", [i(Index + 1)], !IO),
            print_type_desc(TypeDesc, !IO),
            io.nl(!IO)
        else
            io.print_line("deconstruct failed", !IO)
        ),
        print_args(X, Index + 1, Arity, !IO)
    else
        true
    ).

:- pred print_type_desc(type_desc::in, io::di, io::uo) is det.

print_type_desc(TypeDesc, !IO) :-
    type_ctor_and_args(TypeDesc, TypeCtor, TypeArgs),
    type_ctor_name_and_arity(TypeCtor, _ModuleName, TypeCtorName,
        _TypeCtorArity),
    io.write_string(TypeCtorName, !IO),
    (
        TypeArgs = []
    ;
        TypeArgs = [_ | _],
        io.write_string("(", !IO),
        io.write_list(TypeArgs, ", ", print_type_desc, !IO),
        io.write_string(")", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred call_methods(box::in, io::di, io::uo) is det.

call_methods(X, !IO) :-
    X = box(_Foo, _A, B, C, _D),
    io.write_string("Calling methods: ", !IO),
    io.write_string(reverse(B), !IO),
    io.write_string(", ", !IO),
    io.write_string(reverse(C), !IO),
    io.write_string("\n\n", !IO).

%---------------------------------------------------------------------------%
