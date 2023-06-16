%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module follow_code_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    create_exist_data(int_data, IntData),
    output_exist_data(IntData, !IO),
    create_exist_data(string_data, StringData),
    output_exist_data(StringData, !IO),
    create_exist_data(foo_data, FooData),
    output_exist_data(FooData, !IO).

:- type data_type
    --->    int_data
    ;       string_data
    ;       foo_data.

:- type foo
    --->    foo.

:- some [T] pred create_exist_data(data_type::in, T::out) is det => data(T).

create_exist_data(Type, ExistData) :-
    (
        Type = int_data,
        Data = 'new data'(41)
    ;
        Type = string_data,
        Data = 'new data'("forty-one")
    ;
        Type = foo_data,
        Data = 'new data'(foo)
    ),
    Data = data(ExistData).

:- pred output_exist_data(T::in, io::di, io::uo) is det <= data(T).

output_exist_data(ExistData, !IO) :-
    Str = to_string(ExistData),
    io.write_string(Str, !IO),
    io.nl(!IO).

:- type data
    --->    some [T] data(T) => data(T).

:- typeclass data(T) where [
    func to_string(T) = string
].

:- instance data(int) where [
    (to_string(N) = int_to_string(N))
].

:- instance data(string) where [
    (to_string(S) = S)
].

:- instance data(foo) where [
    (to_string(_) = "foo")
].
