%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ground_terms.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module bool.

main(!IO) :-
    list_maybe_bool_term(A1),
    io.write_line(A1, !IO),

    notag_list_maybe_bool_term(B1),
    io.write_line(B1, !IO),
    list_notag_maybe_bool_term(B2),
    io.write_line(B2, !IO),
    list_maybe_notag_bool_term(B3),
    io.write_line(B3, !IO),

    da_i_mb_ls_term_1(C1),
    io.write_line(C1, !IO),
    da_i_mb_ls_term_2a(C2),
    io.write_line(C2, !IO),
    da_i_mb_ls_term_2b(C3),
    io.write_line(C3, !IO),
    da_i_mb_ls_term_6(C4),
    io.write_line(C4, !IO),
    da_i_mb_ls_term_8(C5),
    io.write_line(C5, !IO),
    da_i_mb_ls_term_9(C6),
    io.write_line(C6, !IO),

    da_f_mb_ls_term_1(D1),
    io.write_line(D1, !IO),
    da_f_mb_ls_term_2a(D2),
    io.write_line(D2, !IO),
    da_f_mb_ls_term_2b(D3),
    io.write_line(D3, !IO),
    da_f_mb_ls_term_6(D4),
    io.write_line(D4, !IO),
    da_f_mb_ls_term_8(D5),
    io.write_line(D5, !IO),
    da_f_mb_ls_term_9(D6),
    io.write_line(D6, !IO).

%---------------------------------------------------------------------------%

:- pred list_maybe_bool_term(list(maybe(bool))::out) is det.

list_maybe_bool_term([yes(yes), yes(no), no, yes(no), yes(yes), yes(yes)]).

%---------------------------------------------------------------------------%

:- type notag(T)
    --->    notag(T).

:- pred notag_list_maybe_bool_term(notag(list(maybe(bool)))::out) is det.

notag_list_maybe_bool_term(notag([yes(yes), yes(no), no, yes(no), yes(yes),
    yes(yes)])).

:- pred list_notag_maybe_bool_term(list(notag(maybe(bool)))::out) is det.

list_notag_maybe_bool_term([notag(yes(yes)), notag(yes(no)), notag(no),
    notag(yes(no)), notag(yes(yes)), notag(yes(yes))]).

:- pred list_maybe_notag_bool_term(list(maybe(notag(bool)))::out) is det.

list_maybe_notag_bool_term([yes(notag(yes)), yes(notag(no)), no,
    yes(notag(no)), yes(notag(yes)), yes(notag(yes))]).

%---------------------------------------------------------------------------%

:- type datag(T1, T2, T3)
    --->    da1(T1)
    ;       da2(T2)
    ;       da3(T3)
    ;       da4(T1)
    ;       da5(T2)
    ;       da6(T3)
    ;       da7(T1)
    ;       da8(T2)
    ;       da9(T3).

:- type da_i_mb_ls ==
    datag({int, int, int, int}, maybe({bool, bool, bool}), list(string)).

:- pred da_i_mb_ls_term_1(da_i_mb_ls::out) is det.
:- pred da_i_mb_ls_term_2a(da_i_mb_ls::out) is det.
:- pred da_i_mb_ls_term_2b(da_i_mb_ls::out) is det.
:- pred da_i_mb_ls_term_6(da_i_mb_ls::out) is det.
:- pred da_i_mb_ls_term_8(da_i_mb_ls::out) is det.
:- pred da_i_mb_ls_term_9(da_i_mb_ls::out) is det.

da_i_mb_ls_term_1(da1({1, 2, 3, 4})).
da_i_mb_ls_term_2a(da2(no)).
da_i_mb_ls_term_2b(da2(yes({no, yes, no}))).
da_i_mb_ls_term_6(da6(["a", "bb", "ccc", "dddd"])).
da_i_mb_ls_term_8(da8(yes({yes, yes, no}))).
da_i_mb_ls_term_9(da6(["A", "BB", "CCC", "DDDD"])).

%---------------------------------------------------------------------------%

:- type da_f_mb_ls ==
    datag(float, maybe({bool, bool, bool}), list(string)).

:- pred da_f_mb_ls_term_1(da_f_mb_ls::out) is det.
:- pred da_f_mb_ls_term_2a(da_f_mb_ls::out) is det.
:- pred da_f_mb_ls_term_2b(da_f_mb_ls::out) is det.
:- pred da_f_mb_ls_term_6(da_f_mb_ls::out) is det.
:- pred da_f_mb_ls_term_8(da_f_mb_ls::out) is det.
:- pred da_f_mb_ls_term_9(da_f_mb_ls::out) is det.

da_f_mb_ls_term_1(da1(56.78)).
da_f_mb_ls_term_2a(da2(no)).
da_f_mb_ls_term_2b(da2(yes({no, yes, no}))).
da_f_mb_ls_term_6(da6(["a", "bb", "ccc", "dddd"])).
da_f_mb_ls_term_8(da8(yes({yes, yes, no}))).
da_f_mb_ls_term_9(da6(["A", "BB", "CCC", "DDDD"])).

%---------------------------------------------------------------------------%
