%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: ztoken.m
% main author: philip

:- module ztoken.
:- interface.

:- import_module list.
:- import_module pair.
:- import_module string.
:- import_module word.

:- type ztoken_list == list(pair(ztoken, zcontext)).

:- type ztoken
    --->    name(ident)
    ;       op(op, ident)
    ;       number(string)
    ;       decoration(decoration)
    ;       string(string)
    ;       'Delta'
    ;       'Xi'
    ;       bind
    ;       bsup
    ;       caret
    ;       defs
    ;       esup
    ;       inrel
    ;       langle
    ;       lbag
    ;       left_brace
    ;       limg
    ;       mid         % delete this token???
    ;       minus
    ;       newline
    ;       pipe
    ;       rangle
    ;       rbag
    ;       right_brace
    ;       rimg
    ;       underscore

% C.3.1  Simple tokens
    ;       zAND
    ;       zBRA
    ;       zCOLON
    ;       zCOMMA
    ;       zCOMPOSE
%   ;       zCONJ   (newline)
    ;       zCROSS
    ;       zDEFINEEQUAL
    ;       zDOT
    ;       zELSE
    ;       zEQUALS
    ;       zEXISTS
    ;       zEXISTS1
    ;       zFALSE
%   ;       zFIXITY
    ;       zFORALL
    ;       zFREEBRA
    ;       zFREEEQUALS
    ;       zFREEKET
    ;       zHIDING
    ;       zIF
    ;       zIFF
    ;       zIMPLIES
    ;       zKET
    ;       zLAMBDA
%   ;       zLEFTFUN
    ;       zLET
    ;       zMEMBER
    ;       zMU
%   ;       zNORMAL
    ;       zNOT
    ;       zOR
    ;       zPRESCH
    ;       zPROJECTION
    ;       zPSET
%   ;       zREL
    ;       zRENAME
%   ;       zRIGHTFUN
    ;       zSELECT
    ;       zSEMICOLON
%   ;       zSEQUENCE
    ;       zSETBRA
    ;       zSETKET
    ;       zSQBRA
    ;       zSQKET
    ;       zTHEN
    ;       zTHETA
    ;       zTRUE
    ;       zTURNSTILE
%   ;       zTYPE
    ;       zVBAR

% C.3.2  Box tokens
    ;       pragma(pragma)
    ;       zAX
    ;       zSCH
    ;       zIS
%   ;       zBAR
    ;       zBEGIN
    ;       zGEN
    ;       zST
    ;       zEND.
%   ;       zENDAX

:- type (pragma)
    --->    abbrev
    ;       monot
    ;       syntax
    ;       loglib
    ;       logicgen
    ;       nologicgen.
    % ;     email.

:- pred ztokenPortrayL(ztoken_list, list(string)).
:- mode ztokenPortrayL(in, out) is det.

:- pred ztokenPortray(ztoken, string).
:- mode ztokenPortray(in, out) is det.

:- pred pragma(string, pragma).
:- mode pragma(in, out) is semidet.
:- mode pragma(out, in) is det.

:- type direction
    --->    f   % f = forward only
    ;       b.  % b = both forward and backward

:- pred keyword(direction, ident, ztoken).
:- mode keyword(out, in, out) is semidet.
%:- mode keyword(out, in) is semidet.
:- mode keyword(in(bound(b)), out, in) is semidet.

:- pred reverse_removing_soft_newlines(ztoken_list, ztoken_list).
:- mode reverse_removing_soft_newlines(in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- implementation.

:- import_module maybe.

pragma("ABBREV", abbrev).
pragma("MONOT", monot).
pragma("SYNTAX", syntax).
pragma("LOGLIB", loglib).
pragma("LOGICGEN", logicgen).
pragma("NOLOGICGEN", nologicgen).
% pragma("EMAIL", email).

:- import_module require.

ztokenPortrayL([],  []).
ztokenPortrayL([H0-_|T0], [H|T]) :- ztokenPortray(H0, H), ztokenPortrayL(T0, T).

ztokenPortray(T, S) :-
    ( if T = name(I) then S = identPortray(I)
    else if T = op(_, I) then S = identPortray(I)
    else if T = number(S0) then S = S0
    else if T = decoration(D) then string.append_list(strokeLPortray(D), S)
    else if T = string(S0) then S = S0
    else if T = pragma(P) then pragma(S0, P), string.append("%%", S0, S)
    else if keyword(b, I, T) then S = identPortray(I)
    else error("impossible token in ztokenPortray")
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

keyword(b, id(no, "\\Delta", []),   'Delta').
keyword(b, id(no, "\\Xi", []),      'Xi').
keyword(b, id(no, "\\bind", []),    bind).
keyword(b, id(no, "\\bsup", []),    bsup).
keyword(b, id(no, "^", []), caret).
keyword(b, id(no, "\\defs", []),    defs).
keyword(b, id(no, "\\esup", []),    esup).
keyword(b, id(no, "\\inrel", []),   inrel).
keyword(b, id(no, "\\langle", []),  langle).
keyword(b, id(no, "\\lbag", []),    lbag).
keyword(b, id(no, "{", []), left_brace).
keyword(b, id(no, "\\limg", []),    limg).
%keyword(b, id(no, "\\mid", []),    mid).
keyword(b, id(no, "-", []), minus). %special token since both infix + prefix
keyword(b, id(no, "\\\\", []), newline).
%keyword(b, id(no, "\\also", []),   newline).   % \\ also maps to newline
keyword(b, id(no, "\\pipe", []),    pipe).
keyword(b, id(no, "\\rangle", []),  rangle).
keyword(b, id(no, "\\rbag", []),    rbag).
keyword(b, id(no, "}", []), right_brace).
keyword(b, id(no, "\\rimg", []),    rimg).
keyword(b, id(no, "\\_", []), underscore).
keyword(b, id(no, "\\land", []),    zAND).
keyword(b, id(no, "(", []), zBRA).
keyword(b, id(no, ":", []), zCOLON).
keyword(b, id(no, ",", []), zCOMMA).
keyword(b, id(no, "\\semi", []),    zCOMPOSE).
% zCONJ
keyword(b, id(no, "\\cross", []),   zCROSS).
keyword(b, id(no, "==", []),    zDEFINEEQUAL).
keyword(b, id(no, "@", []), zDOT).
keyword(f, id(no, "\\spot", []),    zDOT).  % TESTING
keyword(b, id(no, "\\ELSE", []),    zELSE).
keyword(b, id(no, "=", []), zEQUALS).
keyword(b, id(no, "\\exists", []),  zEXISTS).
keyword(b, id(no, "\\exists", [subscript("1")]), zEXISTS1).
keyword(b, id(no, "false", []), zFALSE).
% zFIXITY
keyword(b, id(no, "\\forall", []),  zFORALL).
keyword(b, id(no, "\\ldata", []),   zFREEBRA).
keyword(b, id(no, "::=", []),   zFREEEQUALS).
keyword(b, id(no, "\\rdata", []),   zFREEKET).
keyword(b, id(no, "\\hide", []),    zHIDING).
keyword(b, id(no, "\\IF", []),  zIF).
keyword(b, id(no, "\\iff", []), zIFF).
keyword(b, id(no, "\\implies", []),zIMPLIES).
keyword(b, id(no, ")", []), zKET).
keyword(b, id(no, "\\lambda", []),  zLAMBDA).
% zLEFTFUN
keyword(b, id(no, "\\LET", []), zLET).
keyword(b, id(no, "\\in", []),  zMEMBER).
keyword(b, id(no, "\\mu", []),  zMU).
% zNORMAL
keyword(b, id(no, "\\lnot", []),    zNOT).
keyword(b, id(no, "\\lor", []), zOR).
keyword(b, id(no, "\\pre", []), zPRESCH).
keyword(b, id(no, "\\project", []),zPROJECTION).
keyword(b, id(no, "\\power", []),   zPSET).
% zREL
keyword(b, id(no, "/", []), zRENAME).
% zRIGHTFUN
keyword(b, id(no, ".", []), zSELECT).
keyword(b, id(no, ";", []), zSEMICOLON).
% zSEQUENCE
keyword(b, id(no, "\\{", []),   zSETBRA).
keyword(b, id(no, "\\}", []),   zSETKET).
keyword(b, id(no, "[", []), zSQBRA).
keyword(b, id(no, "]", []), zSQKET).
keyword(b, id(no, "\\THEN", []),    zTHEN).
keyword(b, id(no, "\\theta", []),   zTHETA).
keyword(b, id(no, "true", []),  zTRUE).
keyword(b, id(no, "\\vdash", []),   zTURNSTILE).
% zTYPE
keyword(b, id(no, "|", []), zVBAR).
keyword(f, id(no, "\\mid", []), zVBAR). % TESTING

keyword(b, id(no, "\\where", []),   zST).

% These are only used in [out, in] mode
keyword(b, id(no, "\\begin{axdef}", []), zAX).
keyword(b, id(no, "\\begin{schema}", []), zSCH).
keyword(b, id(no, "\\begin{zed}", []), zBEGIN).
% keyword(b, id(no, "\\begin{syntax}", []), zBEGIN).
keyword(b, id(no, "\\begin{gendef}", []), zGEN).
keyword(b, id(no, "\\end", []), zEND).
%keyword(b, id(no, "\\begin", []), begin).
%keyword(b, id(no, "\\end", []), end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C.3.1 Simple Tokens

:- type newline_category
    --->    both
    ;       after
    ;       before
    ;       neither.

:- pred newline(ztoken::in, newline_category::out) is det.

newline(langle, after).
newline(rangle, before).
newline(lbag, after).
newline(rbag, before).
newline(newline, both).

% C.3.1  Simple tokens
newline(zAND, both).
newline(zBRA, after).
newline(zCOLON, both).
newline(zCOMMA, both).
newline(zCOMPOSE, both).
% newline(zCONJ, both).
newline(zCROSS, both).
newline(zDEFINEEQUAL, both).
newline(zDOT, both).
newline(zEQUALS, both).
newline(zELSE, both).
newline(zEXISTS, after).
newline(zEXISTS1, after).
newline(zFALSE, neither).
% newline(zFIXITY, both).
newline(zFORALL, after).
newline(zFREEBRA, both).
newline(zFREEEQUALS, both).
newline(zFREEKET, before).
newline(zHIDING, both).
newline(zIF, after).
newline(zIFF, both).
newline(zIMPLIES, both).
newline(zKET, before).
newline(zLAMBDA, after).
% newline(LEFTFUN, both).
newline(zLET, after).
newline(zMEMBER, both).
newline(zMU, after).
% newline(NORMAL, both).
newline(zNOT, after).
newline(zOR, both).
newline(zPRESCH, after).
newline(zPROJECTION, both).
newline(zPSET, after).
% newline(zREL, both).
newline(zRENAME, both).
% newline(zRIGHTFUN, both).
newline(zSEMICOLON, both).
% newline(zSEQUENCE, both).
newline(zSELECT, both).
newline(zSETBRA, after).
newline(zSETKET, before).
newline(zSQBRA, after).
newline(zSQKET, before).
newline(zTHEN, both).
newline(zTHETA, after).
newline(zTRUE, neither).
newline(zTURNSTILE, both).
% newline(zTYPE, both).
newline(zVBAR, both).

% C.3.2 Box tokens

newline(pragma(_), after).
newline(zBEGIN, after).
newline(zAX, after).
newline(zSCH, after).
newline(zGEN, after).
newline(zIS, both).
%newline(zBAR, both).
newline(zST, both).
newline(zEND, before).
%newline(zENDAX, before).

% C.3.3 Mixfix token categories

newline(op(O, _), C) :-
    ( O = infun(_), C = both
    ; O = postfun, C = before
    ; O = inrel, C = both
    ; O = prerel, C = after
    ; O = ingen,  C = both
    ; O = pregen, C = after
    ).

% C.3.4 Decoration, name and number tokens

newline(name(_), neither).
newline(decoration(_), neither).
newline(number(_), neither).
newline(string(_), neither).
newline(underscore, neither).
newline('Delta', neither).
newline('Xi', neither).

newline(caret, both).
newline(left_brace, both).
newline(right_brace, both).

newline(bind, both).
newline(bsup, both).
newline(defs, both).
newline(esup, both).
newline(inrel, both).
newline(limg, both).
newline(mid, both).
newline(minus, after).
newline(pipe, both).
newline(rimg, before).

reverse_removing_soft_newlines([], []).
reverse_removing_soft_newlines([H | L0], L) :-
    reverse_removing_soft_newlines_2(L0, [H], L).

:- pred reverse_removing_soft_newlines_2(ztoken_list, ztoken_list, ztoken_list).
:- mode reverse_removing_soft_newlines_2(in, in, out) is det.

reverse_removing_soft_newlines_2([], L, L).
reverse_removing_soft_newlines_2([This|Xs], L0, L) :-
    ( soft_newline(This, Xs, L0) -> L1 = L0 ; L1 = [This|L0] ),
    reverse_removing_soft_newlines_2(Xs, L1, L).

:- pred soft_newline(pair(ztoken, zcontext)::in,
    ztoken_list::in, ztoken_list::in) is semidet.

soft_newline(newline-_, [Prev|_], [Next|_]) :-
    (soft_newline_after(Prev) ; soft_newline_before(Next) ).

:- pred soft_newline_after(pair(ztoken, zcontext)::in) is semidet.

soft_newline_after(Token-_) :-
    newline(Token, Category),
    (Category = both ; Category = after).

:- pred soft_newline_before(pair(ztoken, zcontext)::in) is semidet.

soft_newline_before(Token-_) :-
    newline(Token, Category),
    (Category = both ; Category = before).
