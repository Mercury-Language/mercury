%------------------------------------------------------------------------------%
% pprint.m
% Copyright (C) 2000-2002 Ralph Becket <rbeck@microsoft.com>
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

    % A group doc gives the pretty printer a choice: if
    % the doc can be printed without line wrapping then
    % it does so (all line, label, nest and group
    % directives within the group are ignored); otherwise
    % the pretty printer treats the group body literally,
    % although nested group docs remain as choice points.
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

    % packed(Sep, [X1, X2, .., Xn]) = G1 `<>` G2 `<>` .. `<>` Gn where
    % Gi = group(line `<>` Xi `<>` Sep), except for Gn where
    % Gn = group(line `<>` Xn).
    %
    % For the singleton list case, packed(Sep, [X]) = group(line `<>` X).
    %
    % The resulting doc tries to pack as many items on a line as
    % possible.
    %
:- func packed(doc, list(doc)) = doc.

    % A variant of the above whereby only the first N elements of
    % the list are formatted and the rest are replaced by a single
    % ellipsis.
    %
:- func packed(int, doc, list(doc)) = doc.

    % packed_cs(Xs) = packed(comma_space, Xs).
    %
    % For example, to pretty print a Mercury list of docs
    % one might use
    %
    %   brackets(nest(2, packed_cs(Xs)))
    % 
:- func packed_cs(list(doc)) = doc.

    % A variant of the above whereby only the first N elements of
    % the list are formatted and the rest are replaced by a single
    % ellipsis.
    %
:- func packed_cs(int, list(doc)) = doc.

    % separated(PP, Sep, [X1,...,Xn]) =
    %   PP(X1) `<>` Sep `<>` ... Sep `<>` PP(Xn)
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
:- func ellipsis            = doc.      % "...".

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

:- import_module std_util, char, array, map.

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

:- type rev_simple_doc
    ==      simple_doc.

    % This type is used to format key-value pairs in maps when
    % using the generic to_doc/[1,2] functions.
    %
:- type map_pair(K, V)
    --->    map_pair(K, V).

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
    be(W, K, ["" - X], [], RevBest),
    Best = list__reverse(RevBest).

%------------------------------------------------------------------------------%

    % This predicate (and its children) is somewhat different to that
    % described by Wadler.  In the first place, the decision procedure
    % has been recoded (in flattening_works/3) to preserve linear
    % running times under a strict language.  The second important
    % change is that be/5 is now a predicate that accumulates its output
    % as a list of strings, doing away with the need for a more elaborate
    % simple_doc type.  The accumulated strings must be reversed to obtain
    % the printing order.
    %
    % W is the number of characters on a line.
    % K is the number of characters for output on the current line so far.
    % I is the current indentation string as affected by NEST and LABEL.

:- pred be(int, int, list(pair(string, doc)), rev_simple_doc, rev_simple_doc).
:- mode be(in, in, in, in, out) is det.

be(_, _, [])                      -->
    [].

be(W, K, [_ - 'NIL'         | Z]) -->
    be(W, K, Z).

be(W, K, [I - 'SEQ'(X, Y)   | Z]) -->
    be(W, K, [I - X, I - Y | Z]).

be(W, K, [I - 'NEST'(J, X)  | Z]) -->
    be(W, K, [extend(I, J) - X | Z]).

be(W, K, [I - 'LABEL'(L, X) | Z]) -->
    be(W, K, [(I ++ L) - X | Z]).

be(W, K, [_ - 'TEXT'(S)     | Z]) -->
    push_string(S),
    be(W, (K + string__length(S)), Z).

be(W, _, [I - 'LINE'        | Z]) -->
    push_string("\n"),
    push_string(I),
    be(W, string__length(I), Z).

be(W, K, [I - 'GROUP'(X)    | Z]) -->
    ( if   { fits_flattened([X], W - K) }
      then be(W, K, [I - flatten(X) | Z])
      else be(W, K, [I - X | Z])
    ).



:- pred push_string(string, rev_simple_doc, rev_simple_doc).
:- mode push_string(in, in, out) is det.

push_string(S, Ss, [S | Ss]).

%------------------------------------------------------------------------------%

    % Decide if a flattened list of docs will fit on the remainder
    % of the line.
    %
:- pred fits_flattened(list(doc), int).
:- mode fits_flattened(in, in) is semidet.

fits_flattened([]                 , _).
fits_flattened(['NIL'         | Z], R) :- fits_flattened(Z,          R).
fits_flattened(['SEQ'(X, Y)   | Z], R) :- fits_flattened([X, Y | Z], R).
fits_flattened(['NEST'(_, X)  | Z], R) :- fits_flattened([X | Z],    R).
fits_flattened(['LABEL'(_, X) | Z], R) :- fits_flattened([X | Z],    R).
fits_flattened(['LINE'        | Z], R) :- fits_flattened(Z,          R).
fits_flattened(['GROUP'(X)    | Z], R) :- fits_flattened([X | Z],    R).
fits_flattened(['TEXT'(S)     | Z], R) :-
    L = string__length(S),
    R > L,
    fits_flattened(Z, R - L).

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

extend(I, J) = I ++ string__duplicate_char(' ', J).

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
    ( if Xs = [] then PP(X)
                 else PP(X) `<>` (Sep `<>` separated(PP, Sep, Xs))
    ).

%------------------------------------------------------------------------------%

packed(_N, _Sep, []           ) =
    nil.

packed(N,  _Sep, [X]          ) =
    group(line `<>` (if 0 < N then X else ellipsis)).

packed(N,  Sep,  [X1, X2 | Xs]) =
    ( if   0 < N
      then group(line `<>` X1 `<>` Sep) `<>` packed(N - 1, Sep, [X2 | Xs])
      else group(line `<>` ellipsis)
    ).

%------------------------------------------------------------------------------%

packed(Sep, Xs) = packed(int__max_int, Sep, Xs).

%------------------------------------------------------------------------------%

packed_cs(N, Xs) = packed(N, comma_space, Xs).

%------------------------------------------------------------------------------%

packed_cs(Xs) = packed(comma_space, Xs).

%------------------------------------------------------------------------------%

    % This is like a depth-limited version of packed_cs/1 that first
    % calls to_doc/2 on each member of the argument list.
    %
:- func packed_cs_to_depth(int, list(T)) = doc.

packed_cs_to_depth(Depth, Xs) =
    packed_cs(Depth, list__map(to_doc(Depth), Xs)).

%------------------------------------------------------------------------------%

    % This is like a version of packed_cs_to_depth/1 that first
    % calls univ_value/1 for each member of the argument list.
    %
:- func packed_cs_univ_args(int, list(univ)) = doc.

packed_cs_univ_args(Depth, UnivArgs) = 
    packed_cs(
        Depth,
        list__map(func(UnivArg) = to_doc(Depth, univ_value(UnivArg)), UnivArgs)
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
ellipsis                = text("...").

%------------------------------------------------------------------------------%

to_doc(X) = to_doc(int__max_int, X).

%------------------------------------------------------------------------------%

    % This may throw an exception or cause a runtime abort if the term
    % in question has user-defined equality.
    %
to_doc(Depth, X) =
    (
      if      dynamic_cast_to_list(X, List)
      then    list_to_doc(Depth, List)

      else if dynamic_cast_to_array(X, Array)
      then    array_to_doc(Depth, Array)

      else if dynamic_cast_to_tuple(X, Tuple)
      then    tuple_to_doc(Depth, Tuple)

      else if dynamic_cast_to_map(X, Map)
      then    map_to_doc(Depth, Map)

      else if dynamic_cast_to_map_pair(X, MapPair)
      then    map_pair_to_doc(Depth, MapPair)

      else if Depth =< 0
      then    out_of_depth_term_to_doc(X)

      else    generic_term_to_doc(Depth, X)
    ).

%------------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_array(T1, array(T2)).
:-           mode dynamic_cast_to_array(in, out) is semidet.

dynamic_cast_to_array(X, A) :-

        % If X is an array then it has a type with one type argument.
        %
    [ArgTypeDesc] = type_args(type_of(X)),

        % Convert ArgTypeDesc to a type variable ArgType.
        %
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,

        % Constrain the type of A to be array(ArgType) and do the
        % cast.
        %
    dynamic_cast(X, A `with_type` array(ArgType)).

%------------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_list(T1, list(T2)).
:-           mode dynamic_cast_to_list(in, out) is semidet.

dynamic_cast_to_list(X, L) :-

        % If X is a list then it has a type with one type argument.
        %
    [ArgTypeDesc] = type_args(type_of(X)),

        % Convert ArgTypeDesc to a type variable ArgType.
        %
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,

        % Constrain the type of L to be list(ArgType) and do the
        % cast.
        %
    dynamic_cast(X, L `with_type` list(ArgType)).

%------------------------------------------------------------------------------%

:- some [T2, T3] pred dynamic_cast_to_map(T1, map(T2, T3)).
:-               mode dynamic_cast_to_map(in, out) is semidet.

dynamic_cast_to_map(X, M) :-

        % If X is a map then it has a type with two type arguments.
        %
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),

        % Convert the TypeDescs to type variables.
        %
    (_ `with_type` KeyType) `has_type` KeyTypeDesc,
    (_ `with_type` ValueType) `has_type` ValueTypeDesc,

        % Constrain the type of M to be map(KeyType, ValueType)
        % and do the cast.
        %
    dynamic_cast(X, M `with_type` map(KeyType, ValueType)).

%------------------------------------------------------------------------------%

:- some [T2, T3] pred dynamic_cast_to_map_pair(T1, map_pair(T2, T3)).
:-               mode dynamic_cast_to_map_pair(in, out) is semidet.

dynamic_cast_to_map_pair(X, MP) :-

        % If X is a map_pair then it has a type with two type arguments.
        %
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),

        % Convert the TypeDescs to type variables.
        %
    (_ `with_type` KeyType) `has_type` KeyTypeDesc,
    (_ `with_type` ValueType) `has_type` ValueTypeDesc,

        % Constrain the type of MP to be map_pair(KeyType, ValueType)
        % and do the cast.
        %
    dynamic_cast(X, MP `with_type` map_pair(KeyType, ValueType)).

%------------------------------------------------------------------------------%

:- pred dynamic_cast_to_tuple(T, T).
:- mode dynamic_cast_to_tuple(in, out) is semidet.

dynamic_cast_to_tuple(X, X) :-

        % If X is a tuple then it's functor name is {}.
        %
    functor(X, "{}", _Arity).

%------------------------------------------------------------------------------%

:- func out_of_depth_term_to_doc(T) = doc.

out_of_depth_term_to_doc(X) = Doc :-

    functor(X, Name, Arity),

    Doc = ( if Arity = 0 then text(Name)
                         else text(Name) `<>` text("/") `<>` poly(i(Arity))
    ).

%------------------------------------------------------------------------------%

:- func generic_term_to_doc(int, T) = doc.

generic_term_to_doc(Depth, X) = Doc :-

    deconstruct(X, Name, Arity, UnivArgs),

    Doc =
        ( if    Arity = 0
          then  text(Name)
          else  group(
                    text(Name) `<>` parentheses(
                        nest(2, packed_cs_univ_args(Depth - 1, UnivArgs))
                    )
                )
        ).

%------------------------------------------------------------------------------%

    % XXX Ideally we'd just walk the array.  But that's an optimization
    % for another day.
    %
:- func array_to_doc(int, array(T)) = doc.

array_to_doc(Depth, A) =
    group(
        text("array") `<>`
            parentheses(list_to_doc(Depth - 1, array__to_list(A)))
    ).

%------------------------------------------------------------------------------%

:- func list_to_doc(int, list(T)) = doc.

list_to_doc(Depth, Xs) =
    brackets(nest(1, packed_cs_to_depth(Depth - 1, Xs))).

%------------------------------------------------------------------------------%

:- func map_to_doc(int, map(T1, T2)) = doc.

map_to_doc(Depth, X) = Doc :-
    KVs = list__map(mk_map_pair, map__to_assoc_list(X)),
    Doc =
        group(
            text("map") `<>`
                parentheses(list_to_doc(Depth - 1, KVs))
        ).



:- func mk_map_pair(pair(K, V)) = map_pair(K, V).

mk_map_pair(K - V) = map_pair(K, V).



:- func map_pair_to_doc(int, map_pair(T1, T2)) = doc.

map_pair_to_doc(Depth, map_pair(Key, Value)) =
    to_doc(Depth - 1, Key) `<>` text(" -> ") `<>`
        group(nest(2, line `<>` to_doc(Depth - 1, Value))).

%------------------------------------------------------------------------------%

    % This should only really be used if the item in question really
    % is a tuple.
    %
:- func tuple_to_doc(int, T) = doc.

tuple_to_doc(Depth, Tuple) = Doc :-
    deconstruct(Tuple, _Name, _Arity, UnivArgs),
    Doc =
        group(
            braces(nest(1, packed_cs_univ_args(Depth - 1, UnivArgs)))
        ).

%------------------------------------------------------------------------------%

word_wrapped(String) =
    packed(
        space,
        list__map(
            func(Word) = text(Word),
            string__words(char__is_whitespace, String)
        )
    ).

%------------------------------------------------------------------------------%
