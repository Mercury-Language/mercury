%-----------------------------------------------------------------------------%
%
% Linear congruence pseudo-random number generator.
%
%-----------------------------------------------------------------------------%

:- module lcg.
:- interface.

:- type lcg_state.
:- type rs == lcg_state.

:- func init(int) = rs.
:- pred random(int::out, rs::in, rs::out) is det.
:- pred random(int::in, int::in, int::out, rs::in, rs::out) is det.

%-----------------------------------------------------------------------------%

:- module lcg.io.
:- interface.

:- import_module io.

:- pred init(int::in, io::di, io::uo) is det.
:- pred get_supply(rs::out, io::di, io::uo) is det.
:- pred set_supply(rs::in, io::di, io::uo) is det.
:- pred random(int::out, io::di, io::uo) is det.
:- pred random(int::in, int::in, int::out, io::di, io::uo) is det.

:- end_module lcg.io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

:- type lcg_state
    --->    lcg_state(
                mult    :: int,
                add     :: int,
                seed    :: int
            ).

init(Seed) = lcg_state(50331499, 29230937, Seed).

random(Seed, lcg_state(Mult, Add, Seed0), lcg_state(Mult, Add, Seed)) :-
    Seed = Seed0 * Mult + Add.

random(Low, Range, Num, !RS) :-
    random(R, !RS),
    Num = Low + (R `mod` Range).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lcg.io.
:- implementation.

:- mutable(localsupp, rs, init(4357), ground,
    [attach_to_io_state, untrailed]).

init(Seed, !IO) :-
    set_localsupp(init(Seed), !IO).

get_supply(Supply, !IO) :-
    get_localsupp(Supply, !IO).

set_supply(Supply, !IO) :-
    set_localsupp(Supply, !IO).

random(Num, !IO) :-
    get_localsupp(RS0, !IO),
    random(Num, RS0, RS),
    set_localsupp(RS, !IO).

random(Low, Range, Num, !IO) :-
    get_localsupp(RS0, !IO),
    random(Low, Range, Num, RS0, RS),
    set_localsupp(RS, !IO).

:- end_module lcg.io.

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
