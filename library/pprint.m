%------------------------------------------------------------------------------%
% pprint.m
% Copyright (C) 2000 Ralph Becket <rbeck@microsoft.com>
% Wed Mar 22 17:44:32  2000
% vi: ts=4 sw=4 et tw=0 wm=0
%
% Main author: rwab1
% Stability: medium
%
% This file is hereby contributed to the University of
% Melbourne Mercury Project to be released under whatever
% licence the current project management deems fit.
%
%
% ABOUT
% -----
%
% This is pretty much a direct transliteration of Philip
% Wadler's Haskell pretty printer described in "A Prettier
% Printer", available at
% http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html
%
% Wadler's approach has three main advantages:
% 1. the layout algebra is small and quite intuitive (more
%    so than Hughes');
% 2. the pretty printer is optimal in the sense that it will
%    never generate output that over-runs the specified width
%    unless that is unavoidable; and
% 3. the pretty printer is bounded in that it never needs to
%    look more than k characters ahead to make a formatting
%    decision (although see the XXX comments below).
%
% I have made three small changes:
%
% (a) rather than having group/1 as a non-primitive function
% (for allowing line-breaks to be converted into spaces at
% the pretty printer's discretion) over docs, I have
% extended the doc type to include a `GROUP' constructor and
% altered flatten/1 and be/3 appropriately.  Because `UNION'
% only arises as a consequence of processing a 'GROUP' it
% turns out to be simpler to do away with `UNION' altogether
% and convert clauses that process `UNION' terms to
% processing `GROUP's.
%
% (b) The second change is that flattened `line' breaks
% become empty strings rather than spaces.
%
% (c) The third change is the introduction of the `LABEL'
% constructor, which acts much like `NEST', except that
% indentation is defined using a string rather than a number
% of spaces.  This is useful for, e.g., multi-line compiler
% errors and warnings that should be prefixed with the
% offending source file and line number.
%
% Performance problems due to the current lack of support
% for laziness in Mercury has meant that the formatting
% decision procedure has had to be recoded to preserve
% linear runtime behaviour in an eager language.
%
% I have also added several obvious general purpose
% formatting functions.
%
%
% USAGE
% -----
%
% There are two stages in pretty printing an object of some
% type T:
% 1. convert the object to a pprint__doc using the
%    constructor functions described below or by simply
%    calling pprint__to_doc/[1,2];
% 2. call pprint__write/[4,5] or pprint__to_string/2
%    passing the display width and the doc.
%
%
% EXAMPLES
% --------
%
% Below are some docs followed by the ways they might be
% displayed by the pretty printer given various line widths.
%
% 1. text("Hello ") `<>` line `<>` text("world")
% 
%   Hello
%   world
%
% 2. group(text("Hello ") `<>` line `<>` text("world"))
%
%   Hello world
%
%   Hello
%   world
%
% 3. group(text("Hello ") `<>` nest(3, line `<>` text("world")))
%
%   Hello world
%
%   Hello
%      world
%
% 4. group(
%   text("Goodbye ") `<>`
%   nest(3, line `<>` text("cruel ") `<>` line `<>` text("world")
% )
%
%   Goodbye cruel world
%
%   Goodbye
%      cruel
%      world
%
% 5. group(
%   text("Goodbye ") `<>`
%   nest(3, line `<>`
%     group(text("cruel ") `<>` line `<>` text("world"))
%   )
% )
%
%   Goodbye cruel world
%
%   Goodbye
%      cruel world
%
%   Goodbye
%      cruel
%      world
%
% 6. label("Look! ",
%   line `<>`
%   group(
%     text("Goodbye ") `<>`
%     nest(3, line `<>`
%       group(text("cruel ") `<>` line `<>` text("world"))
%     )
%   )
% )
%
%   Look! Goodbye cruel world
%
%   Look! Goodbye
%   Look!    cruel world
%
%   Look! Goodbye
%   Look!    cruel
%   Look!    world
%
%------------------------------------------------------------------------------%

:- module pprint.

:- interface.

:- import_module int, string, list, io.

    % Clients must translate data structures into docs for
    % the pretty printer to display.
    %
:- type doc.

    % The empty document corresponding to the null string.
    %
:- func nil                 = doc.

    % The document consisting of a single string.
    %
:- func text(string)        = doc.

    % The composition of two docs with no intervening space.
    %
:- func doc `<>` doc        = doc.

    % The new-line document.  In a group doc (see below) the
    % pretty printer may choose to instead `flatten' all
    % line docs into nil docs in order to fit a doc on a
    % single line.
    %
:- func line                = doc.

    % Any `line' docs in the body that are not flattened out
    % by the pretty printer are followed by the given number
    % of spaces (nested `nest's add up).
    %
:- func nest(int, doc)      = doc.

    % Identical to a nest doc except that indentation is
    % extended with a string label rather than some number
    % of spaces.
    %
:- func label(string, doc)  = doc.

    % A group doc may be flattened out in the sense
    % described for line, above, at the discretion of the
    % pretty printer.  A group doc, therefore, defines a
    % choice point for the pretty printer.
    %
:- func group(doc)          = doc.

    % This function can be used to convert strings, chars,
    % ints and floats to their text doc equivalents.
    %
:- func poly(string__poly_type) = doc.

    % Shorthand for doc `<>` line `<>` doc.
    %
:- func doc `</>` doc       = doc.

    % Various bracketing functions.
    %
    %   bracketed(L, R, Doc) = text(L) `<>` Doc `<>` text(R)
    %       parentheses(Doc) = bracketed("(", ")", Doc)
    %          brackets(Doc) = bracketed("[", "]", Doc)
    %            braces(Doc) = bracketed("{", "}", Doc)
    %
:- func bracketed(string, string, doc)  = doc.
:- func parentheses(doc)                = doc.
:- func brackets(doc)                   = doc.
:- func braces(doc)                     = doc.

    % separated(PP, Sep, [X1,...,Xn]) =
    %   PP(X1) `<>` (Sep `<>` ... (Sep `<>` PP(Xn)) ... )
    %
    % Note that if you want to pack as many things on one
    % line as possible with some sort of separator, the
    % following example illustrates a suitable idiom:
    %
    %   separated(PP, group(comma_space_line), Xs)
    %
:- func separated(func(T) = doc, doc, list(T)) = doc.

    % Handy punctuation docs and versions with following
    % spaces and/or line breaks.
    %
:- func comma               = doc.
:- func semic               = doc.      % Semicolon.
:- func colon               = doc.
:- func space               = doc.
:- func comma_space         = doc.
:- func semic_space         = doc.
:- func colon_space         = doc.
:- func comma_line          = doc.
:- func semic_line          = doc.
:- func colon_line          = doc.
:- func space_line          = doc.
:- func comma_space_line    = doc.
:- func semic_space_line    = doc.
:- func colon_space_line    = doc.

    % Example: if one wanted to pretty print a Mercury list
    % then one might write
    %
    %   brackets(nest(2, separated(MyPP, comma_space_line, MyList)))
    %
    % where `MyPP' would be a function from MyList members
    % to docs.

    % Performs word wrapping at the end of line, taking
    % whitespace sequences as delimiters separating words.
    %
:- func word_wrapped(string) = doc.

    % Convert arbitrary terms to docs.  This requires
    % std_util__functor/3 to work on all components of the
    % object being converted.  The second version places a
    % maximum depth on terms which are otherwise truncated;
    % when the depth limit is reached, all arguments of a
    % functor are replaced by `/<arity>' where <arity> is
    % the number of arguments.
    %
    % This may throw an exception or cause a runtime abort
    % if the term in question has user-defined equality.
    %
:- func to_doc(T)           = doc.
:- func to_doc(int, T)      = doc.

    % Convert docs to pretty printed strings.  The int
    % argument specifies a line width in characters.
    %
:- func to_string(int, doc) = string.

    % Write docs out in pretty printed format.  The int
    % argument specifies a page width in characters.
    %
:- pred write(int, doc, io__state, io__state).
:- mode write(in, in, di, uo) is det.

:- pred write(io__output_stream, int, doc, io__state, io__state).
:- mode write(in, in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module std_util, char.

:- type doc
    --->    'NIL'
    ;       'SEQ'(doc, doc)
    ;       'NEST'(int, doc)
    ;       'LABEL'(string, doc)
    ;       'TEXT'(string)
    ;       'LINE'
    ;       'GROUP'(doc).

:- type simple_doc
    ==      list(string).

%------------------------------------------------------------------------------%

nil                     = 'NIL'.
X `<>` Y                = 'SEQ'(X, Y).
nest(I, X)              = 'NEST'(I, X).
label(L, X)             = 'LABEL'(L, X).
text(S)                 = 'TEXT'(S).
line                    = 'LINE'.
group(X)                = 'GROUP'(X).

poly(s(S))              = text(string__format("%s", [s(S)])).
poly(c(C))              = text(string__format("%c", [c(C)])).
poly(i(I))              = text(string__format("%d", [i(I)])).
poly(f(F))              = text(string__format("%f", [f(F)])).

%------------------------------------------------------------------------------%

to_string(W, X) = S :-
    pretty(pred(H::in, T::in, [H | T]::out) is det, W, X, [], Ss),
    S = string__append_list(list__reverse(Ss)).

write(W, X)             --> pretty(io__write_string, W, X).

write(Stream, W, X)     --> pretty(io__write_string(Stream), W, X).

%------------------------------------------------------------------------------%

:- pred pretty(pred(string, T, T), int, doc, T, T).
:- mode pretty(pred(in, in, out) is det, in, in, in, out) is det.
:- mode pretty(pred(in, di, uo) is det, in, in, di, uo) is det.

pretty(P, W, X)         --> layout(P, best(W, 0, X)).

%------------------------------------------------------------------------------%

:- pred layout(pred(string, T, T), simple_doc, T, T).
:- mode layout(pred(in, in, out) is det, in, in, out) is det.
:- mode layout(pred(in, di, uo) is det, in, di, uo) is det.

layout(P, Strings)      --> list__foldl(P, Strings).

%------------------------------------------------------------------------------%

:- func best(int, int, doc) = simple_doc.

best(W, K, X)           = Best
:-
    be(W, K, ["" - X], Best, []).

%------------------------------------------------------------------------------%

    % This predicate (and its children) is somewhat different to that
    % described by Wadler.  In the first place, the decision procedure
    % has been recoded (in flattening_works/3) to preserve linear
    % running times under a strict language.  The second important
    % change is that be/5 is now a predicate that accumulates its output
    % as a list of strings, doing away with the need for a more elaborate
    % simple_doc type.

:- pred be(int, int, list(pair(string, doc)), simple_doc, simple_doc).
:- mode be(in, in, in, out, in) is det.

be(_, _, [], Out, In)             :-  Out = list__reverse(In).
be(W, K, [_ - 'NIL'         | Z]) --> be(W, K, Z).
be(W, K, [I - 'SEQ'(X, Y)   | Z]) --> be(W, K, [I - X, I - Y | Z]).
be(W, K, [I - 'NEST'(J, X)  | Z]) --> be(W, K, [extend(I, J) - X | Z]).
be(W, K, [I - 'LABEL'(L, X) | Z]) --> be(W, K, [string__append(I, L) - X | Z]).
be(W, K, [_ - 'TEXT'(S)     | Z]) --> [S], be(W, (K + string__length(S)), Z).
be(W, _, [I - 'LINE'        | Z]) --> ["\n", I], be(W, string__length(I), Z).
be(W, K, [I - 'GROUP'(X)    | Z]) -->
    ( if { flattening_works(X, Z, W - K) } then
        be(W, K, [I - flatten(X) | Z])
      else
        be(W, K, [I - X | Z])
    ).

% ---------------------------------------------------------------------------- %

    % Decide whether flattening a given doc will allow it and
    % up to the next possible 'LINE' in the following docs to
    % fit on the remainder of the line.
    %
    % XXX This solution is necessary to avoid crippling performance
    % problems on large terms.  A spot of laziness would do away
    % with the need for the next three predicates.
    %
:- pred flattening_works(doc, list(pair(string, doc)), int).
:- mode flattening_works(in, in, in) is semidet.

flattening_works(DocToFlatten, FollowingDocs, RemainingWidth) :-
    fits_flattened([DocToFlatten], RemainingWidth, RemainingWidth0),
    fits_on_rest(FollowingDocs, RemainingWidth0).

% ---------------------------------------------------------------------------- %

    % Decide if a flattened list of docs will fit on the remainder
    % of the line.  Computes the space left over if so.
    %
:- pred fits_flattened(list(doc), int, int).
:- mode fits_flattened(in, in, out) is semidet.

fits_flattened([]                 ) --> [].
fits_flattened(['NIL'         | Z]) --> fits_flattened(Z).
fits_flattened(['SEQ'(X, Y)   | Z]) --> fits_flattened([X, Y | Z]).
fits_flattened(['NEST'(_, X)  | Z]) --> fits_flattened([X | Z]).
fits_flattened(['LABEL'(_, X) | Z]) --> fits_flattened([X | Z]).
fits_flattened(['LINE'        | Z]) --> fits_flattened(Z).
fits_flattened(['GROUP'(X)    | Z]) --> fits_flattened([X | Z]).
fits_flattened(['TEXT'(S)     | Z], R0, R) :-
    L = string__length(S),
    R0 > L,
    fits_flattened(Z, R0 - L, R).

% ---------------------------------------------------------------------------- %

    % Decide if a list of indent-doc pairs, up to the first 'LINE',
    % will fit on the remainder of the line.
    %
:- pred fits_on_rest(list(pair(string, doc)), int).
:- mode fits_on_rest(in, in) is semidet.

fits_on_rest([]                     , _).
fits_on_rest([_ - 'NIL'         | Z], R) :- fits_on_rest(Z, R).
fits_on_rest([I - 'SEQ'(X, Y)   | Z], R) :- fits_on_rest([I - X, I - Y | Z], R).
fits_on_rest([I - 'NEST'(_, X)  | Z], R) :- fits_on_rest([I - X | Z], R).
fits_on_rest([I - 'LABEL'(_, X) | Z], R) :- fits_on_rest([I - X | Z], R).
fits_on_rest([_ - 'LINE'        | _], _).
fits_on_rest([I - 'GROUP'(X)    | Z], R) :- fits_on_rest([I - X | Z], R).
fits_on_rest([_ - 'TEXT'(S)     | Z], R) :-
    L = string__length(S),
    R > L,
    fits_on_rest(Z, R - L).

%------------------------------------------------------------------------------%

:- func flatten(doc) = doc.

flatten('NIL')          = 'NIL'.
flatten('SEQ'(X, Y))    = 'SEQ'(flatten(X), flatten(Y)).
flatten('NEST'(_, X))   = flatten(X).
flatten('LABEL'(_, X))  = flatten(X).
flatten('TEXT'(S))      = 'TEXT'(S).
flatten('LINE')         = 'NIL'.
flatten('GROUP'(X))     = flatten(X).

%------------------------------------------------------------------------------%

:- func extend(string, int) = string.

extend(I, J) = string__append(I, string__duplicate_char(' ', J)).

%------------------------------------------------------------------------------%

X `</>` Y               = X `<>` line `<>` Y.

%------------------------------------------------------------------------------%

bracketed(L, R, D)      = text(L) `<>` D `<>` text(R).
parentheses(D)          = bracketed("(", ")", D).
brackets(D)             = bracketed("[", "]", D).
braces(D)               = bracketed("{", "}", D).

%------------------------------------------------------------------------------%

separated(_,  _,   []) = nil.

separated(PP, Sep, [X | Xs]) =
    ( if Xs = [] then
        PP(X)
      else
        PP(X) `<>` (Sep `<>` separated(PP, Sep, Xs))
    ).

%------------------------------------------------------------------------------%

comma                   = text(",").
semic                   = text(";").
colon                   = text(":").
space                   = text(" ").
comma_space             = text(", ").
semic_space             = text("; ").
colon_space             = text(": ").
comma_line              = comma `<>` line.
semic_line              = semic `<>` line.
colon_line              = colon `<>` line.
space_line              = space `<>` line.
comma_space_line        = text(", ") `<>` line.
semic_space_line        = text("; ") `<>` line.
colon_space_line        = text(": ") `<>` line.

%------------------------------------------------------------------------------%

to_doc(X) = to_doc(int__max_int, X).

%------------------------------------------------------------------------------%

    % This may throw an exception or cause a runtime abort if the term
    % in question has user-defined equality.

to_doc(Depth, X) = Doc :-
    deconstruct(X, Name, Arity, UnivArgs),
    ( if Arity = 0 then
        Doc = text(Name)
      else if Depth =< 0 then
        Doc = text(Name) `<>` text("/") `<>` poly(i(Arity))
      else
        Args = list__map(
            ( func(UnivArg) = to_doc(Depth - 1, univ_value(UnivArg)) ),
            UnivArgs
        ),
        Doc = text(Name) `<>`
            parentheses(
                group(
                    nest(2,
                        line `<>` separated(id, comma_space_line, Args)
                    )
                )
        )
    ).

% ---------------------------------------------------------------------------- %

word_wrapped(String) =
    separated(
        text,
        group(space_line),
        string__words(char__is_whitespace, String)
    ).

%------------------------------------------------------------------------------%
