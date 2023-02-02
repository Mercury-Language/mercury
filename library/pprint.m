%---------------------------------------------------------------------------%
% vim:ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2010-2011 The University of Melbourne
% Copyright (C) 2014-2018, 2020, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: pprint.m
% Main author: rafe
% Stability: medium
%
% NOTE: this module has now been superceded by pretty_printer.m, which
%
% - is more economical,
% - produces better output,
% - has better control over the amount of output produced, and
% - supports user-specifiable formatting for arbitrary types.
%
% ABOUT
% -----
%
% This started off as pretty much a direct transliteration of Philip Wadler's
% Haskell pretty printer described in "A Prettier Printer", available at
% http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html
%
% Several changes have been made to the algorithm to preserve linear running
% time under a strict language, and to ensure scalability to extremely large
% terms without thrashing the VM system.
%
% Wadler's approach has three main advantages:
%
% 1. the layout algebra is small and quite intuitive (more so than Hughes');
% 2. the pretty printer is optimal in the sense that it will never generate
%    output that over-runs the specified width unless that is unavoidable; and
% 3. the pretty printer is bounded in that it never needs to look more than
%    k characters ahead to make a formatting decision.
%
% I have made the following changes:
%
% (a) rather than having group/1 as a non-primitive function (for allowing
% line-breaks to be converted into spaces at the pretty printer's discretion)
% over docs, I have extended the doc type to include a `GROUP' constructor,
% and made the appropriate algorithmic changes. Because `UNION' only arises
% as a consequence of processing a 'GROUP' it turns out to be simpler
% to do away with `UNION' altogether and convert clauses that process
% `UNION' terms to processing `GROUP's.
%
% (b) Flattened `line' breaks become empty strings rather than spaces.
%
% (c) The third change is the introduction of the `LABEL' constructor,
% which acts much like `NEST', except that indentation is defined
% using a string rather than a number of spaces. This is useful for,
% e.g., multi-line compiler errors and warnings that should be prefixed
% with the offending source file and line number.
%
% (d) The formatting decision procedure has been altered to preserve
% linear runtime behaviour in a strict language.
%
% (e) Naively marking up a term as a doc has the drawback that the
% resulting doc is significantly larger than the original term.
% Worse, any sharing structure in the original term leads to
% duplicated sub-docs, which can cause an exponential blow-up in the
% size of the doc w.r.t. the source term. To get around this problem
% I have introduced the 'DOC' constructor which causes on-demand
% conversion of arguments.
%
% [This is not true laziness in the sense that the 'DOC', once
% evaluated, will be overwritten with its value. This approach would
% lead to garbage retention and not solve the page thrashing behaviour
% otherwise experienced when converting extremely large terms.
% Instead, each 'DOC' is reevaluated each time it is examined.
% This trades off computation time for space.]
%
% I have added several obvious general purpose formatting functions.
%
%
% USAGE
% -----
%
% There are two stages in pretty printing an object of some type T:
% 1. convert the object to a pprint.doc using the constructor functions
%    described below or by simply calling pprint.to_doc/[1,2];
% 2. call pprint.write/[4,5] or pprint.to_string/2 passing the display width
%    and the doc.
%
%
% EXAMPLES
% --------
%
% The doc/1 type class has types string, char, int, float and doc as instances.
% Hence these types can all be converted to docs by applying doc/1.
% This happens automatically to the arguments of ++/2. Users may find it
% convenient to add other types as instances of the doc/1 type class.
%
% Below are some docs followed by the ways they might be displayed by the
% pretty printer given various line widths.
%
% 1. "Hello " ++ line ++ "world"
%
%   Hello
%   world
%
% 2. group("Hello " ++ line ++ "world")
%
%   Hello world
%
%   Hello
%   world
%
% 3. group("Hello " ++ nest(3, line ++ "world"))
%
%   Hello world
%
%   Hello
%      world
%
% 4. group("Goodbye " ++ nest(3, line ++ "cruel " ++ line ++ "world")
%
%   Goodbye cruel world
%
%   Goodbye
%      cruel
%      world
%
% 5. group("Goodbye " ++ nest(3, line ++ group("cruel " ++ line ++ "world")))
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
% 6. label("Look! ", line ++
%        group("Goodbye " ++
%              nest(3, line ++ group("cruel " ++ line ++ "world"))))
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module pprint.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module stream.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

    % Clients must translate data structures into docs for
    % the pretty printer to display.
    %
:- type doc.

    % This typeclass can be used to simplify the construction of docs.
    %
:- typeclass doc(T) where [
    % Convert a T to a doc, placing a limit on how much of the term
    % will be fully converted as follows:
    %
    % doc(_, f         ) = f
    % doc(N, f(A, B, C)) = f/3 if N =< 0
    % doc(N, f(A, B, C)) = some representation of the term whereby
    %   A is converted as doc(N - 1, A),
    %   B is converted as doc(N - 2, B), and
    %   C is converted as doc(N - 3, C)
    %   - if there are more than N arguments, the N+1th and subsequent
    %     arguments should be replaced with a single ellipsis.
    %
    func doc(int, T) = doc
].

:- instance doc(doc).
:- instance doc(string).
:- instance doc(int).
:- instance doc(int8).
:- instance doc(int16).
:- instance doc(int32).
:- instance doc(int64).
:- instance doc(uint).
:- instance doc(uint8).
:- instance doc(uint16).
:- instance doc(uint32).
:- instance doc(uint64).
:- instance doc(float).
:- instance doc(char).

%---------------------------------------------------------------------------%
%
% Creating a doc.
%

    % The empty document corresponding to the null string.
    %
:- func nil = doc.

    % The document consisting of a single string.
    %
    % NOTE: since string is now an instance of the doc/1
    % type class, it is simpler to just apply the doc/1
    % method.
    %
:- func text(string) = doc.

    % This function can be used to convert strings, chars, ints, uints and
    % floats to their text doc equivalents.
    %
    % NOTE: since these types are now instances of the doc/1 type class,
    % it is simpler to just apply the doc/1 method to these types.
    %
:- func poly(poly_type) = doc.

    % The newline document. In a group doc (see below) the pretty printer
    % may choose to instead `flatten' all line docs into nil docs in order
    % to fit a doc on a single line.
    %
:- func line = doc.

    % Handy punctuation docs, and versions with following spaces and/or
    % line breaks.
    %
:- func comma            = doc.
:- func semic            = doc.      % Semicolon.
:- func colon            = doc.
:- func space            = doc.
:- func comma_space      = doc.
:- func semic_space      = doc.
:- func colon_space      = doc.
:- func comma_line       = doc.
:- func semic_line       = doc.
:- func colon_line       = doc.
:- func space_line       = doc.
:- func comma_space_line = doc.
:- func semic_space_line = doc.
:- func colon_space_line = doc.
:- func ellipsis         = doc.      % "...".

    % Convert an instance of the doc/1 typeclass to a doc.
    %
:- func doc(T) = doc <= (doc(T)).

    % Convert an arbitrary term to a doc. This requires std_util.functor/3
    % to work on all components of the object being converted. The second
    % version places a maximum depth on terms which are otherwise truncated
    % in the manner described in the documentation for the doc/2 method
    % of the doc/1 type class.
    %
    % This may throw an exception or cause a runtime abort if the term
    % in question has user-defined equality.
    %
:- func to_doc(T)      = doc.
:- func to_doc(int, T) = doc.

%---------------------------------------------------------------------------%
%
% Manipulating docs.
%

    % The composition of two docs with no intervening space.
    %
    % NOTE: with the addition of the doc/1 type class, it is
    % simpler to construct compound docs using ++/2.
    %
:- func doc `<>` doc = doc.

    % An alternative to the <>/2 concatenation operator that works
    % on members of the doc/1 typeclass.
    %
:- func T1 ++ T2 = doc <= (doc(T1), doc(T2)).

    % Any `line' docs in the body that are not flattened out by the
    % pretty printer are followed by the given number of spaces
    % (nested `nest's add up).
    %
:- func nest(int, T) = doc <= (doc(T)).

    % Identical to a nest doc except that indentation is extended with
    % a string label rather than some number of spaces.
    %
:- func label(string, T) = doc <= (doc(T)).

    % A group doc gives the pretty printer a choice: if the doc can be printed
    % without line wrapping then it does so (all line, label, nest and group
    % directives within the group are ignored); otherwise the pretty printer
    % treats the group body literally, although nested group docs remain as
    % choice points.
    %
:- func group(T) = doc <= (doc(T)).

    % Shorthand for doc ++ line ++ doc.
    %
:- func doc `</>` doc = doc.

    % Various bracketing functions.
    %
    %   bracketed(L, R, Doc) = L ++ Doc ++ R
    %       parentheses(Doc) = bracketed("(", ")", Doc)
    %          brackets(Doc) = bracketed("[", "]", Doc)
    %            braces(Doc) = bracketed("{", "}", Doc)
    %
:- func bracketed(T1, T2, T3) = doc <= (doc(T1), doc(T2), doc(T3)).
:- func parentheses(T)        = doc <= (doc(T)).
:- func brackets(T)           = doc <= (doc(T)).
:- func braces(T)             = doc <= (doc(T)).

    % packed(Sep, [X1, X2, .., Xn]) = G1 `<>` G2 `<>` .. `<>` Gn where
    % Gi = group(line `<>` Xi `<>` Sep), except for Gn where
    % Gn = group(line `<>` Xn).
    %
    % For the singleton list case, packed(Sep, [X]) = group(line `<>` X).
    %
    % The resulting doc tries to pack as many items on a line as possible.
    %
:- func packed(T1, list(T2)) = doc <= (doc(T1), doc(T2)).

    % A variant of the above whereby only the first N elements of the list
    % are formatted and the rest are replaced by a single ellipsis.
    %
:- func packed(int, T1, list(T2)) = doc <= (doc(T1), doc(T2)).

    % packed_cs(Xs) = packed(comma_space, Xs).
    %
    % For example, to pretty print a Mercury list of docs one might use
    %
    %   brackets(nest(2, packed_cs(Xs)))
    %
:- func packed_cs(list(T)) = doc <= (doc(T)).

    % A variant of the above whereby only the first N elements of the list
    % are formatted and the rest are replaced by a single ellipsis.
    %
:- func packed_cs(int, list(T)) = doc <= (doc(T)).

    % This is like a depth-limited version of packed_cs/1 that first calls
    % to_doc/2 on each member of the argument list.
    %
:- func packed_cs_to_depth(int, list(T)) = doc.

    % This is like a version of packed_cs_to_depth/1 that first calls
    % univ_value/1 for each member of the argument list.
    %
:- func packed_cs_univ_args(int, list(univ)) = doc.

    % separated(PP, Sep, [X1,...,Xn]) =
    %   PP(X1) `<>` Sep `<>` ... Sep `<>` PP(Xn)
    %
:- func separated(func(T1) = doc, T2, list(T1)) = doc <= (doc(T2)).

    % Performs word wrapping at the end of line, taking whitespace sequences
    % as delimiters separating words.
    %
    % See `char.is_whitespace' for the definition of whitespace characters
    % used by this predicate.
    %
:- func word_wrapped(string) = doc.

%---------------------------------------------------------------------------%
%
% Prettyprint a doc.
%

    % Convert docs to pretty printed strings. The int argument specifies
    % a line width in characters.
    %
:- func to_string(int, doc) = string.

    % Write the given doc out in its pretty printed format to the
    % current output stream.
    %
    % The int argument specifies the preferred maximum length of each line.
    % (This preferred maximum can be exceeded, e.g. if a single text string
    % to be printed is longer than this width.)
    %
:- pred write(int::in, T::in, io::di, io::uo) is det <= doc(T).

    % Write the specified doc to the specified string writer stream
    % in its pretty printed format.
    %
    % The int argument specifies the preferred maximum length of each line.
    % (This preferred maximum can be exceeded, e.g. if a single text string
    % to be printed is longer than this width.)
    %
:- pred write(Stream::in, int::in, T::in, State::di, State::uo) is det
    <= ( doc(T), stream.writer(Stream, string, State) ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module deconstruct.
:- import_module enum.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module map.
:- import_module ops.
:- import_module pair.
:- import_module robdd.
:- import_module sparse_bitset.
:- import_module term.
:- import_module type_desc.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module version_array.

%---------------------------------------------------------------------------%

:- type doc
    --->    pp_nil
    ;       pp_text(string)
    ;       pp_line
    ;       pp_seq(doc, doc)
    ;       pp_group(doc)
    ;       pp_nest(int, doc)
    ;       pp_label(string, doc)
    ;       pp_doc(int, univ).
            % pp_doc(MaxDepth, Univ)
            % - Univ is the object to be converted to a doc via to_doc/3,
            %   represented as a univ.
            % - MaxDepth is the depth limit before using ellipsis.

    % This type is used to format key-value pairs in maps when
    % using the generic to_doc/[1,2] functions.
    %
:- type map_pair(K, V)
    --->    map_pair(K, V).

    % Used for depth-limit arguments.
    %
:- type depth == int.

%---------------------------------------------------------------------------%

:- instance doc(doc)    where [doc(_, Doc) =    Doc].
:- instance doc(string) where [doc(_, String) = text(String)].
:- instance doc(int)    where [doc(_, I) =      poly(i(I))].
:- instance doc(int8)   where [doc(_, I8) =     text(int8_to_string(I8))].
:- instance doc(int16)  where [doc(_, I16) =    text(int16_to_string(I16))].
:- instance doc(int32)  where [doc(_, I32) =    text(int32_to_string(I32))].
:- instance doc(int64)  where [doc(_, I64) =    text(int64_to_string(I64))].
:- instance doc(uint)   where [doc(_, UI) =     text(uint_to_string(UI))].
:- instance doc(uint8)  where [doc(_, UI8) =    text(uint8_to_string(UI8))].
:- instance doc(uint16) where [doc(_, UI16) =   text(uint16_to_string(UI16))].
:- instance doc(uint32) where [doc(_, UI32) =   text(uint32_to_string(UI32))].
:- instance doc(uint64) where [doc(_, UI64) =   text(uint64_to_string(UI64))].
:- instance doc(float)  where [doc(_, Float) =  poly(f(Float))].
:- instance doc(char)   where [doc(_, Char) =   poly(c(Char))].

%---------------------------------------------------------------------------%

nil            = pp_nil.
text(S)        = pp_text(S).

poly(s(S))     = text(string.format("%s", [s(S)])).
poly(c(C))     = text(string.format("%c", [c(C)])).
poly(i(I))     = text(string.format("%d", [i(I)])).
poly(i8(I8))   = text(string.format("%d", [i(int8.cast_to_int(I8))])).
poly(i16(I16)) = text(string.format("%d", [i(int16.cast_to_int(I16))])).
poly(i32(I32)) = text(string.format("%d", [i(int32.cast_to_int(I32))])).
poly(i64(I64)) = text(string.format("%d", [i(int64.cast_to_int(I64))])).
poly(u(U))     = text(string.format("%u", [u(U)])).
poly(u8(U8))   = text(string.format("%u", [u(uint8.cast_to_uint(U8))])).
poly(u16(U16)) = text(string.format("%u", [u(uint16.cast_to_uint(U16))])).
poly(u32(U32)) = text(string.format("%u", [u(uint32.cast_to_uint(U32))])).
poly(u64(U64)) = text(string.format("%u", [u(uint64.cast_to_uint(U64))])).
poly(f(F))     = text(string.format("%f", [f(F)])).

line           = pp_line.

%---------------------------------------------------------------------------%

comma            = text(",").
semic            = text(";").
colon            = text(":").
space            = text(" ").
comma_space      = text(", ").
semic_space      = text("; ").
colon_space      = text(": ").
comma_line       = "," ++ line.
semic_line       = ";" ++ line.
colon_line       = ":" ++ line.
space_line       = " " ++ line.
comma_space_line = ", " ++ line.
semic_space_line = "; " ++ line.
colon_space_line = ": " ++ line.
ellipsis         = text("...").

%---------------------------------------------------------------------------%

doc(X) = doc(int.max_int, X).

%---------------------------------------------------------------------------%

to_doc(X) = to_doc(int.max_int, X).

to_doc(Depth, X) = to_doc_prio(Depth, mercury_op_table_comma_priority, X).

%---------------------------------------------------------------------------%

X `<>` Y       = pp_seq(X, Y).
Doc1 ++ Doc2   = doc(Doc1) `<>` doc(Doc2).

nest(I, X)     = pp_nest(I, doc(X)).
label(L, X)    = pp_label(L, doc(X)).
group(X)       = pp_group(doc(X)).

%---------------------------------------------------------------------------%

X `</>` Y = X ++ line ++ Y.

%---------------------------------------------------------------------------%

bracketed(L, R, D) = L ++ D ++ R.
parentheses(D)     = bracketed("(", ")", D).
brackets(D)        = bracketed("[", "]", D).
braces(D)          = bracketed("{", "}", D).

%---------------------------------------------------------------------------%

packed(Sep, Xs) = packed(int.max_int, Sep, Xs).

packed(_N, _Sep, []) =
    nil.
packed(N, _Sep, [X]) =
    group(line ++ (if 0 < N then doc(X) else ellipsis)).
packed(N, Sep, [X1, X2 | Xs]) =
    ( if 0 < N then
        group(line ++ X1 ++ Sep) ++ packed(N - 1, Sep, [X2 | Xs])
    else
        group(line ++ ellipsis)
    ).

%---------------------------------------------------------------------------%

packed_cs(Xs) = packed(", ", Xs).

packed_cs(N, Xs) = packed(N, ", ", Xs).

%---------------------------------------------------------------------------%

packed_cs_to_depth(Depth, Xs) =
    packed_cs(Depth, list.map(to_doc(Depth), Xs)).

%---------------------------------------------------------------------------%

packed_cs_univ_args(Depth, UnivArgs) =
    packed_cs(Depth, list.map(func(UA) = pp_doc(Depth, UA), UnivArgs)).

%---------------------------------------------------------------------------%

separated(_, _,  []) = nil.
separated(PP, Sep, [X | Xs]) = Doc :-
    % XXX Using a helper predicate that takes X and Xs as separate arguments
    % (and thus always works on a nonempty list) would do the same job
    % with only one nil-vs-cons test per iteration.
    (
        Xs = [],
        Doc = PP(X)
    ;
        Xs = [_ | _],
        Doc = PP(X) ++ (Sep ++ separated(PP, Sep, Xs))
    ).

%---------------------------------------------------------------------------%

word_wrapped(String) =
    packed(space, list.map(func(Word) = text(Word),
        string.words_separator(char.is_whitespace, String))).

%---------------------------------------------------------------------------%

    % This may throw an exception or cause a runtime abort if the term
    % in question has user-defined equality.
    %
:- func to_doc_prio(int, priority, T) = doc.

to_doc_prio(Depth, Priority, X) =
    ( if dynamic_cast_to_var(X, Var) then
        var_to_doc(Depth, Var)
    else if dynamic_cast_to_sparse_bitset_of_int(X, SparseBitsetInt) then
        sparse_bitset_to_doc(Depth, SparseBitsetInt)
    else if dynamic_cast_to_sparse_bitset_of_var(X, SparseBitsetVar) then
        sparse_bitset_to_doc(Depth, SparseBitsetVar)
    else if dynamic_cast_to_list(X, List) then
        list_to_doc(Depth, List)
    else if dynamic_cast_to_array(X, Array) then
        array_to_doc(Depth, Array)
    else if dynamic_cast_to_version_array(X, VersionArray) then
        version_array_to_doc(Depth, VersionArray)
    else if dynamic_cast_to_tuple(X, Tuple) then
        tuple_to_doc(Depth, Tuple)
    else if dynamic_cast_to_map(X, Map) then
        map_to_doc(Depth, Map)
    else if dynamic_cast_to_map_pair(X, MapPair) then
        map_pair_to_doc(Depth, MapPair)
    else if dynamic_cast_to_robdd(X, Robdd) then
        robdd_to_doc(Depth, Robdd)
    else
        generic_term_to_doc(Depth, Priority, X)
    ).

%---------------------------------------------------------------------------%

:- func generic_term_to_doc(depth, priority, T) = doc.

generic_term_to_doc(Depth, Priority, X) = Doc :-
    ( if Depth =< 0 then
        functor(X, canonicalize, Name, Arity),
        Doc = ( if Arity = 0 then text(Name) else Name ++ "/" ++ Arity )
    else
        deconstruct(X, canonicalize, Name, _Arity, UnivArgs),
        Table = init_mercury_op_table,
        ( if
            UnivArgs = [UnivArg],
            lookup_prefix_op(Table, Name, OpPri, GtOrGe)
        then
            Doc = maybe_parens(Priority, OpPri,
                Name ++
                space ++
                univ_to_doc(Depth - 1, OpPri `adjusted_by` GtOrGe, UnivArg)
            )
        else if
            UnivArgs = [UnivArg],
            lookup_postfix_op(Table, Name, OpPri, GtOrGe)
        then
            Doc = maybe_parens(Priority, OpPri,
                univ_to_doc(Depth - 1, OpPri `adjusted_by` GtOrGe, UnivArg) ++
                space ++
                Name
            )
        else if
            UnivArgs = [UnivArgL, UnivArgR],
            lookup_infix_op(Table, Name, OpPri, GtOrGeL, GtOrGeR)
        then
            Doc = maybe_parens(Priority, OpPri,
                univ_to_doc(Depth - 1, OpPri `adjusted_by` GtOrGeL,
                    UnivArgL) ++
                space ++
                Name ++
                space ++
                group(line ++
                    nest(2,
                        univ_to_doc(Depth - 2, OpPri `adjusted_by` GtOrGeR,
                            UnivArgR)
                    )
                )
            )
        else if
            UnivArgs = [UnivArgR1, UnivArgR2],
            lookup_binary_prefix_op(Table, Name, OpPri, GtOrGeR1, GtOrGeR2)
        then
            Doc = maybe_parens(Priority, OpPri,
                Name ++
                space ++
                univ_to_doc(Depth - 2, OpPri `adjusted_by` GtOrGeR1,
                    UnivArgR1) ++
                space ++
                group(line ++
                    nest(2,
                        univ_to_doc(Depth - 2, OpPri `adjusted_by` GtOrGeR2,
                            UnivArgR2)
                    )
                )
            )
        else if
            UnivArgs = []
        then
            Doc = text(Name)
        else
            Doc = group(
                Name ++ parentheses(
                    nest(2, packed_cs_univ_args(Depth - 1, UnivArgs))
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % We need to put parentheses around a subterm if its top-level
    % functor has a higher priority than its parent functor.
    %
:- func maybe_parens(priority, priority, doc) = doc.

maybe_parens(ParentPriority, OpPriority, Doc) =
    ( if priority_gt(ParentPriority, OpPriority) then
        parentheses(Doc)
    else
        Doc
    ).

%---------------------------------------------------------------------------%

:- func priority `adjusted_by` arg_prio_gt_or_ge = priority.

adjusted_by(OpPriority, GtOrGe) = MinArgPriority :-
    MinArgPriority = min_priority_for_arg(OpPriority, GtOrGe).

%---------------------------------------------------------------------------%

    % Convert a univ encapsulated value into a doc.
    %
:- func univ_to_doc(int, priority, univ) = doc.

univ_to_doc(Depth, Priority, Univ) =
    to_doc_prio(Depth, Priority, univ_value(Univ)).

%---------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_var(T1::in, var(T2)::out) is semidet.

dynamic_cast_to_var(X, V) :-
    % If X is a var then it has a type with one type argument.
    [ArgTypeDesc] = type_args(type_of(X)),

    % Convert ArgTypeDesc to a type variable ArgType.
    (_ : ArgType) `has_type` ArgTypeDesc,

    % Constrain the type of V to be var(ArgType) and do the cast.
    dynamic_cast(X, V : var(ArgType)).

%---------------------------------------------------------------------------%

:- pred dynamic_cast_to_sparse_bitset_of_int(T1::in, sparse_bitset(int)::out)
    is semidet.

dynamic_cast_to_sparse_bitset_of_int(X, A) :-
    dynamic_cast(X, A : sparse_bitset(int)).

:- some [T2] pred dynamic_cast_to_sparse_bitset_of_var(T1::in,
    sparse_bitset(var(T2))::out) is semidet.

dynamic_cast_to_sparse_bitset_of_var(X, A) :-
    dynamic_cast(X, A : sparse_bitset(var)).

%---------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_version_array(T1::in,
    version_array(T2)::out) is semidet.

dynamic_cast_to_version_array(X, VA) :-
    % If X is a version array then it has a type with one type argument.
    [ArgTypeDesc] = type_args(type_of(X)),

    % Convert ArgTypeDesc to a type variable ArgType.
    (_ : ArgType) `has_type` ArgTypeDesc,

    % Constrain the type of VA to be array(ArgType) and do the cast.
    dynamic_cast(X, VA : version_array(ArgType)).

%---------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_list(T1::in, list(T2)::out) is semidet.

dynamic_cast_to_list(X, L) :-
    % If X is a list then it has a type with one type argument.
    [ArgTypeDesc] = type_args(type_of(X)),

    % Convert ArgTypeDesc to a type variable ArgType.
    (_ : ArgType) `has_type` ArgTypeDesc,

    % Constrain the type of L to be list(ArgType) and do the cast.
    dynamic_cast(X, L : list(ArgType)).

%---------------------------------------------------------------------------%

:- some [T2, T3] pred dynamic_cast_to_map(T1::in, map(T2, T3)::out) is semidet.

dynamic_cast_to_map(X, M) :-
    % If X is a map then it has a type with two type arguments.
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),

    % Convert the TypeDescs to type variables.
    (_ : KeyType) `has_type` KeyTypeDesc,
    (_ : ValueType) `has_type` ValueTypeDesc,

    % Constrain the type of M to be map(KeyType, ValueType) and do the cast.
    dynamic_cast(X, M : map(KeyType, ValueType)).

%---------------------------------------------------------------------------%

:- some [T2, T3] pred dynamic_cast_to_map_pair(T1::in, map_pair(T2, T3)::out)
    is semidet.

dynamic_cast_to_map_pair(X, MP) :-
    % If X is a map_pair then it has a type with two type arguments.
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),

    % Convert the TypeDescs to type variables.
    (_ : KeyType) `has_type` KeyTypeDesc,
    (_ : ValueType) `has_type` ValueTypeDesc,

    % Constrain the type of MP to be map_pair(KeyType, ValueType)
    % and do the cast.
    dynamic_cast(X, MP : map_pair(KeyType, ValueType)).

%---------------------------------------------------------------------------%

:- pred dynamic_cast_to_tuple(T::in, T::out) is semidet.

dynamic_cast_to_tuple(X, X) :-
    % If X is a tuple then it's functor name is {}.
    functor(X, canonicalize, "{}", _Arity).

%---------------------------------------------------------------------------%

:- some [T2] pred dynamic_cast_to_robdd(T1, robdd(T2)).
:-           mode dynamic_cast_to_robdd(in, out) is semidet.

dynamic_cast_to_robdd(X, R) :-
    % If X is a robdd then it has a type with one type argument.
    [ArgTypeDesc] = type_args(type_of(X)),

    % Convert ArgTypeDesc to a type variable ArgType.
    (_ : ArgType) `has_type` ArgTypeDesc,

    % Constrain the type of R to be robdd(ArgType) and do the cast.
    dynamic_cast(X, R : robdd(ArgType)).

%---------------------------------------------------------------------------%

:- func var_to_doc(int, var(T)) = doc.

var_to_doc(Depth, V) =
    to_doc(Depth, to_int(V)).

%---------------------------------------------------------------------------%

    % XXX Ideally we would just walk the sparse bitset.
    % But that is an optimization for another day.
    %
:- func sparse_bitset_to_doc(int, sparse_bitset(T)) = doc <= uenum(T).

sparse_bitset_to_doc(Depth, A) =
    group("sparse_bitset" ++
        parentheses(list_to_doc(Depth - 1, sparse_bitset.to_sorted_list(A)))).

%---------------------------------------------------------------------------%

:- func list_to_doc(int, list(T)) = doc.

list_to_doc(Depth, Xs) =
    brackets(nest(1, packed_cs_to_depth(Depth - 1, Xs))).

%---------------------------------------------------------------------------%

    % XXX Ideally we would just walk the array. But that is an optimization
    % for another day.
    %
:- func array_to_doc(int, array(T)) = doc.

array_to_doc(Depth, A) =
    group("array" ++ parentheses(list_to_doc(Depth - 1, array.to_list(A)))).

%---------------------------------------------------------------------------%

    % XXX Ideally we would just walk the version array. But that is
    % an optimization for another day.
    %
:- func version_array_to_doc(int, version_array(T)) = doc.

version_array_to_doc(Depth, A) =
    group("version_array"
        ++ parentheses(list_to_doc(Depth - 1, version_array.to_list(A)))).

%---------------------------------------------------------------------------%

    % This should only really be used if the item in question really
    % is a tuple.
    %
:- func tuple_to_doc(int, T) = doc.

tuple_to_doc(Depth, Tuple) = Doc :-
    deconstruct(Tuple, canonicalize, _Name, _Arity, UnivArgs),
    Doc = group(braces(nest(1, packed_cs_univ_args(Depth - 1, UnivArgs)))).

%---------------------------------------------------------------------------%

:- func map_to_doc(int, map(T1, T2)) = doc.

map_to_doc(Depth, X) = Doc :-
    KVs = list.map(mk_map_pair, map.to_assoc_list(X)),
    Doc = group("map" ++ parentheses(list_to_doc(Depth - 1, KVs))).

:- func mk_map_pair(pair(K, V)) = map_pair(K, V).

mk_map_pair(K - V) = map_pair(K, V).

%---------------------------------------------------------------------------%

:- func map_pair_to_doc(int, map_pair(T1, T2)) = doc.

map_pair_to_doc(Depth, map_pair(Key, Value)) =
    to_doc(Depth - 1, Key) ++ text(" -> ") ++
        group(nest(2, line ++ to_doc(Depth - 1, Value))).

%---------------------------------------------------------------------------%

:- func robdd_to_doc(int, robdd(T)) = doc.

robdd_to_doc(Depth, R) =
    "robdd_dnf" ++ parentheses(list_to_doc(Depth - 1, dnf(R))).

%---------------------------------------------------------------------------%

to_string(Width, Doc) = Str :-
    layout_best(pred(H::in, T::in, [H | T]::out) is det, Width, Doc, [], Strs),
    Str = string.append_list(list.reverse(Strs)).

write(Width, Doc, !IO) :-
    layout_best(io.write_string, Width, doc(Doc), !IO).

write(Stream, Width, Doc, !State) :-
    layout_best(put(Stream), Width, doc(Doc), !State).

%---------------------------------------------------------------------------%

    % This is a contraction of Wadler's pretty, layout and be functions,
    % adapted to work with a strict evaluation order.
    %
:- pred layout_best(pred(string, T, T), int, doc, T, T).
:- mode layout_best(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode layout_best(pred(in, in, out) is det, in, in, in, out) is det.

layout_best(AccPred, Width, Doc, !LayoutStream) :-
    layout_best_acc(AccPred, Width, "", Doc, 0, _UsedWidth, !LayoutStream).

    % layout_best_acc(AccPred, Width, AfterNlStr, Doc,
    %   !UsedWidth, !LayoutStream):
    %
    % AccPred is the predicate for accumulating output strings.
    %
    % Width is the number of characters on a line;
    %
    % AfterNlStr is the indentation string to we should put after newlines.
    %
    % Doc is the doc to lay out.
    %
    % !UsedWidth is the number of characters laid out on the current line
    % so far before and after laying out Doc.
    %
    % !LayoutStream is the layout stream before and after laying out Doc.
    %
    % This predicate is somewhat different to the function `be' described
    % by Wadler. In the first place, the decision procedure has been
    % recoded (in fits_flat/2) to preserve linear running times under
    % a strict language. The second important change is that layout_best_acc/8
    % handles output strings as they are identified (e.g. writing them
    % out or accumulating them in a list), doing away with the need for
    % a more elaborate simple_doc type.
    %
:- pred layout_best_acc(pred(string, T, T), int, string, doc, int, int, T, T).
:- mode layout_best_acc(pred(in, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode layout_best_acc(pred(in, in, out) is det,
    in, in, in, in, out, in, out) is det.

layout_best_acc(AccPred, Width, AfterNlStr, Doc, !UsedWidth, !LayoutStream) :-
    (
        Doc = pp_nil
    ;
        Doc = pp_seq(DocA, DocB),
        layout_best_acc(AccPred, Width, AfterNlStr, DocA,
            !UsedWidth, !LayoutStream),
        layout_best_acc(AccPred, Width, AfterNlStr, DocB,
            !UsedWidth, !LayoutStream)
    ;
        Doc = pp_nest(ExtraIndent, DocA),
        NewAfterNlStr = AfterNlStr ++ string.duplicate_char(' ', ExtraIndent),
        layout_best_acc(AccPred, Width, NewAfterNlStr, DocA,
            !UsedWidth, !LayoutStream)
    ;
        Doc = pp_label(LabelStr, DocA),
        NewAfterNlStr = AfterNlStr ++ LabelStr,
        layout_best_acc(AccPred, Width, NewAfterNlStr, DocA,
            !UsedWidth, !LayoutStream)
    ;
        Doc = pp_line,
        !:UsedWidth = string.count_code_points(AfterNlStr),
        AccPred("\n", !LayoutStream),
        AccPred(AfterNlStr, !LayoutStream)
    ;
        Doc = pp_group(DocA),
        ( if fits_flat(DocA, Width - !.UsedWidth) then
            layout_flat(AccPred, DocA, !UsedWidth, !LayoutStream)
        else
            layout_best_acc(AccPred, Width, AfterNlStr, DocA,
                !UsedWidth, !LayoutStream)
        )
    ;
        Doc = pp_doc(MaxDepth, Univ),
        DocA = to_doc(MaxDepth, univ_value(Univ)),
        layout_best_acc(AccPred, Width, AfterNlStr, DocA,
            !UsedWidth, !LayoutStream)
    ;
        Doc = pp_text(Text),
        !:UsedWidth = !.UsedWidth + string.count_code_points(Text),
        AccPred(Text, !LayoutStream)
    ).

%---------------------------------------------------------------------------%

    % Decide if a flattened doc will fit on the remainder of the line.
    %
:- pred fits_flat(doc::in, int::in) is semidet.

fits_flat(Doc, MaxWidth) :-
    fits_flat_width_left(Doc, MaxWidth, _WidthLeft).

    % Returns the width left on the line after formatting the given doc
    % on a line with the given max width. Stops and fails as soon as
    % the width left goes zero or negative.
    % XXX The "goes zero" part is arguably a bug.
    %
:- pred fits_flat_width_left(doc::in, int::in, int::out) is semidet.

fits_flat_width_left(Doc, WidthAvail, WidthLeft) :-
    (
        ( Doc = pp_nil
        ; Doc = pp_line
        ),
        WidthLeft = WidthAvail
    ;
        Doc = pp_text(Str),
        StrLen = string.count_code_points(Str),
        WidthLeft = WidthAvail - StrLen,
        WidthLeft > 0               % XXX This *could* be WidthLeft >= 0.
    ;
        Doc = pp_seq(DocA, DocB),
        fits_flat_width_left(DocA, WidthAvail, WidthLeftAfterA),
        fits_flat_width_left(DocB, WidthLeftAfterA, WidthLeft)
    ;
        ( Doc = pp_group(DocA)
        ; Doc = pp_nest(_, DocA)
        ; Doc = pp_label(_, DocA)   % XXX Why are we ignoring the labels?
        ),
        fits_flat_width_left(DocA, WidthAvail, WidthLeft)
    ;
        Doc = pp_doc(MaxDepth, Univ),
        DocA = to_doc(MaxDepth, univ_value(Univ)),
        fits_flat_width_left(DocA, WidthAvail, WidthLeft)
    ).

%---------------------------------------------------------------------------%

    % Lay out a doc in its flattened form.
    %
:- pred layout_flat(pred(string, T, T), doc, int, int, T, T).
:- mode layout_flat(pred(in, di, uo) is det,  in, in, out, di, uo) is det.
:- mode layout_flat(pred(in, in, out) is det, in, in, out, in, out) is det.

layout_flat(AccPred, Doc, !UsedWidth, !LayoutStream) :-
    (
        ( Doc = pp_nil
        ; Doc = pp_line
        )
    ;
        Doc = pp_text(Text),
        !:UsedWidth = !.UsedWidth + string.count_code_points(Text),
        AccPred(Text, !LayoutStream)
    ;
        Doc = pp_seq(DocA, DocB),
        layout_flat(AccPred, DocA, !UsedWidth, !LayoutStream),
        layout_flat(AccPred, DocB, !UsedWidth, !LayoutStream)
    ;
        ( Doc = pp_group(DocA)
        ; Doc = pp_nest(_, DocA)
        ; Doc = pp_label(_, DocA)
        ),
        layout_flat(AccPred, DocA, !UsedWidth, !LayoutStream)
    ;
        Doc = pp_doc(MaxDepth, Univ),
        DocA = to_doc(MaxDepth, univ_value(Univ)),
        layout_flat(AccPred, DocA, !UsedWidth, !LayoutStream)
    ).

%---------------------------------------------------------------------------%
:- end_module pprint.
%---------------------------------------------------------------------------%
