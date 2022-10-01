%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% This code is from a bug report in m-users on 2022 sep 23.
%---------------------------------------------------------------------------%

:- module mode_error_arg_number.
:- interface.

:- import_module list.

:- pred split_into_fragments(
    pred(list(P), list(P), list(P))::
         pred(in(non_empty_list), out(non_empty_list), out) is det,
    list(P)::in(non_empty_list),
    list(list(P))::in, list(list(P))::out(non_empty_list)) is det.

:- func split_into_fragments_func(
    pred(list(P), list(P), list(P))::
         pred(in(non_empty_list), out(non_empty_list), out) is det,
    list(P)::in(non_empty_list),
    list(list(P))::in) = (list(list(P))::out(non_empty_list)) is det.

:- implementation.

split_into_fragments(Pred, Paras @ [_ | _], Akku, Frags) :-
    Pred(Paras, Frag, Rest),
    append(Akku, [Frag], Akku1),
    (
        Rest = [],
        Frags = Akku1
    ;
        Rest = [_ | _],
        % The following call used to get an error message about
        % argument five not getting sufficiently instantiated, even though
        % it has only four arguments. The problem was that the code generating
        % the error message was counting compiler-visible arguments, which
        % include the type_info argument for P added by polymorphism, when
        % it should have been counting only the user-visible arguments.
        split_into_fragments(Pred, Rest, Akku1, Frags)
    ).

split_into_fragments_func(Pred, Paras @ [_ | _], Akku) = Frags :-
    Pred(Paras, Frag, Rest),
    append(Akku, [Frag], Akku1),
    (
        Rest = [],
        Frags = Akku1
    ;
        Rest = [_ | _],
        % Test whether, when we report the mode error, the compiler
        % refers to "argument 4" or "function result".
        Frags = split_into_fragments_func(Pred, Rest, Akku1)
    ).

:- pred append1(list(T), list(T), list(T)).
:- mode append1(in(I), in(non_empty_list), out(non_empty_list)) is det.

append1([], Ys, Ys).
append1([X | Xs], Ys, [X | Zs]) :-
    append1(Xs, Ys, Zs).
