%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% query
%
% David H. D. Warren
%
% query population and area database to find countries of
% approximately equal population density.

:- module query.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module prolog.

:- type quad
    --->    quad(string, int, string, int).

main(!IO) :-
    ( if query(Out) then
        Out = quad(C1, D1, C2, D2),
        io.write_string(C1, !IO),
        io.write_string(" has density ", !IO),
        io.write_int(D1, !IO),
        io.write_string(" while ", !IO),
        io.write_string(C2, !IO),
        io.write_string(" has density ", !IO),
        io.write_int(D2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("No solutions\n", !IO)
    ).

:- pred query(quad::out) is nondet.

query(quad(C1, D1, C2, D2)) :-
    density(C1, D1),
    density(C2, D2),
    D1 > D2,
    T1 = 20 * D1,
    T2 = 21 * D2,
    T1 < T2.

:- pred density(string, int).
:- mode density(out, out) is nondet.

density(C, D) :-
    pop(C, P),
    area(C, A),
    P100 = P * 100,
    D = P100 // A.

:- pred pop(string::out, int::out) is multi.

% populations in 100000s
pop("china",        8250).
pop("india",        5863).
pop("ussr",         2521).
pop("usa",          2119).
pop("indonesia",    1276).
pop("japan",        1097).
pop("brazil",       1042).
pop("bangladesh",    750).
pop("pakistan",      682).
pop("w_germany",     620).
pop("nigeria",       613).
pop("mexico",        581).
pop("uk",            559).
pop("italy",         554).
pop("france",        525).
pop("philippines",   415).
pop("thailand",      410).
pop("turkey",        383).
pop("egypt",         364).
pop("spain",         352).
pop("poland",        337).
pop("s_korea",       335).
pop("iran",          320).
pop("ethiopia",      272).
pop("argentina",     251).

:- pred area(string::in, int::out) is semidet.

% areas in 1000s of square miles
area("china",       3380).
area("india",       1139).
area("ussr",        8708).
area("usa",         3609).
area("indonesia",    570).
area("japan",        148).
area("brazil",      3288).
area("bangladesh",    55).
area("pakistan",     311).
area("w_germany",     96).
area("nigeria",      373).
area("mexico",       764).
area("uk",            86).
area("italy",        116).
area("france",       213).
area("philippines",   90).
area("thailand",     200).
area("turkey",       296).
area("egypt",        386).
area("spain",        190).
area("poland",       121).
area("s_korea",       37).
area("iran",         628).
area("ethiopia",     350).
area("argentina",   1080).
