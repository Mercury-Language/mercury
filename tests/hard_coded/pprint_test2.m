%-----------------------------------------------------------------------------%
% pprint_test2.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Jan 21 16:36:49 EST 2003
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module pprint_test2.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, solutions, std_util, pprint.

:- type t
    --->    a
    ;       f(t)
    ;       - t             % fx
    ;       ~ t             % fy
    ;       ( :- t )        % fx, high priority
    ;       t ++ t          % xfy
    ;       t -- t          % yfx
    ;       t == t          % xfx
    ;       ( t -> t )      % xfy, high priority
    ;       { some t t }.   % fxx



:- pred t(int, t  ).
:- mode t(in,  out) is multi.

t(N, X) :-
    ( if N =< 0 then
        X = a
      else
        t(N - 1, Y),
        (   X = f(Y)
        ;   X = - Y
        ;   X = (~ Y)
        ;   X = ( :- Y )
        ;   (
                t(N - 1, Z),
                (   X = Y ++ Z
                ;   X = Y -- Z
                ;   X = (Y == Z)
                ;   X = (Y -> Z)
                ;   X = (some Y Z)
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_list(solutions(t(2)), "", write_t, !IO).

%-----------------------------------------------------------------------------%

:- pred write_t(t, io, io).
:- mode write_t(in, di, uo) is det.

write_t(T, !IO) :-
    pprint.write(80, to_doc(T), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
