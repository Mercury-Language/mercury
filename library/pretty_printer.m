%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2011 The University of Melbourne
% Copyright (C) 2014-2016, 2018, 2020, 2022-2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: pretty_printer.m
% Main author: rafe
% Stability: medium
%
% This module defines a doc type for formatting and a pretty printer for
% displaying docs.
%
% The doc type includes data constructors for outputting strings, newlines,
% forming groups, indented blocks, and arbitrary values.
%
% The key feature of the algorithm is this: newlines in a group are ignored if
% the group can fit on the remainder of the current line. (The algorithm is
% similar to those of Oppen and Wadler, although it uses neither coroutines or
% laziness.)
%
% When a newline is printed, indentation is also output according to the
% current indentation level.
%
% The pretty printer includes special support for formatting Mercury style
% terms in a way that respects Mercury's rules for operator precedence and
% bracketing.
%
% The pretty printer takes a parameter specifying a collection of user-defined
% formatting functions for handling certain types rather than using the
% default built-in mechanism. This allows one to, say, format maps as
% sequences of (key -> value) pairs rather than exposing the underlying
% 234-tree structure.
%
% The amount of output produced is controlled via limit parameters.
% Three kinds of limits are supported: the output line width, the maximum
% number of lines to be output, and a limit on the depth for formatting
% arbitrary terms. Output is replaced with ellipsis ("...") when a limit
% has been exceeded.
%
%---------------------------------------------------------------------------%

:- module pretty_printer.
:- interface.

:- import_module array.
:- import_module char.
:- import_module deconstruct.
:- import_module io.
:- import_module list.
:- import_module one_or_more.
:- import_module stream.
:- import_module tree234.
:- import_module type_desc.
:- import_module univ.
:- import_module version_array.

%---------------------------------------------------------------------------%

:- type doc
    --->    str(string)
            % Output a literal string. Strings containing newlines, hard tabs,
            % etc. will lead to strange output.

    ;       nl
            % Output a newline, followed by indentation, iff the enclosing
            % group does not fit on the current line and starting a new line
            % adds more space.

    ;       hard_nl
            % Always outputs a newline, followed by indentation.

    ;       docs(list(doc))
            % An embedded sequence of docs.

    ;       format_univ(univ)
            % Use a specialised formatter if available, otherwise use the
            % generic formatter.

    ;       format_list(list(univ), doc)
            % Pretty print a list of items using the given doc as a separator
            % between items.

    ;       format_term(string, list(univ))
            % Pretty print a term with zero or more arguments. If the term
            % corresponds to a Mercury operator it will be printed with
            % appropriate fixity and, if necessary, in parentheses. The term
            % name will be quoted and escaped if necessary.

    ;       format_susp((func) = doc)
            % The argument is a suspended computation used to lazily produce a
            % doc. If the formatting limit has been reached then just "..." is
            % output, otherwise the suspension is evaluated and the resulting
            % doc is used. This is useful for formatting large structures
            % without using more resources than required. Expanding a
            % suspended computation reduces the formatting limit by one.

    ;       pp_internal(pp_internal).
            % pp_internal docs are used in the implementation and cannot be
            % exploited by user code.

:- type docs == list(doc).

    % This type is private to the implementation and cannot be exploited
    % by user code.
    %
:- type pp_internal.

%---------------------------------------------------------------------------%
%
% Functions for constructing docs.
%

    % indent(IndentString, Docs):
    %
    % Append IndentString to the current indentation while printing Docs.
    % Indentation is printed after each newline that is output.
    %
:- func indent(string, list(doc)) = doc.

    % indent(Docs) = indent("  ", Docs).
    %
    % A convenient abbreviation.
    %
:- func indent(list(doc)) = doc.

    % group(Docs):
    %
    % If Docs can be output on the remainder of the current line by ignoring
    % any nls in Docs, then do so. Otherwise nls in Docs are printed
    % (followed by any indentation). The formatting test is applied recursively
    % for any subgroups in Docs.
    %
:- func group(list(doc)) = doc.

    % format(X) = format_univ(univ(X)):
    %
    % A convenient abbreviation.
    %
:- func format(T) = doc.

    % format_arg(Doc) has the effect of formatting any term in Doc as though
    % it were an argument in a Mercury term, by enclosing it in parentheses
    % if necessary.
    %
:- func format_arg(doc) = doc.

%---------------------------------------------------------------------------%
%
% Functions for converting docs to strings and writing them out to streams.
%

    % write_doc_formatted(X, !IO):
    % write_doc_formatted(FileStream, X, !IO):
    %
    % Convert X to a doc using the format function, and then
    % call write_doc on the result.
    %
:- pred write_doc_formatted(T::in, io::di, io::uo) is det.
:- pred write_doc_formatted(io.output_stream::in, T::in,
    io::di, io::uo) is det.

    % write_doc(Doc, !IO):
    % write_doc(FileStream, Doc, !IO):
    %
    % Format Doc to io.stdout_stream or FileStream respectively using put_doc,
    % with include_details_cc, the default formatter_map, and the default
    % pp_params.
    %
:- pred write_doc(doc::in, io::di, io::uo) is det.
:- pred write_doc(io.output_stream::in, doc::in, io::di, io::uo) is det.

    % put_doc(Stream, Canonicalize, FMap, Params, Doc, !State):
    %
    % Format Doc to Stream. Format format_univ(_) docs using specialised
    % formatters Formatters, and using Params as the pretty printer parameters.
    % The Canonicalize argument controls how put_doc deconstructs values
    % of noncanonical types (see the documentation of the noncanon_handling
    % type for details).
    %
:- pred put_doc(Stream, noncanon_handling, formatter_map, pp_params, doc,
    State, State) <= stream.writer(Stream, string, State).
:- mode put_doc(in, in(canonicalize), in, in, in, di, uo) is det.
:- mode put_doc(in, in(include_details_cc), in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(put_doc/7),
    (Stream = io.output_stream, State = io.state)).

%---------------------------------------------------------------------------%
%
% Mechanisms for controlling *how* docs are converted to strings.
%

    % The type of generic formatting functions.
    % The first argument is the univ of the value to be formatted.
    % The second argument is the list of argument type_descs for
    % the type of the first argument.
    %
:- type formatter == (func(univ, list(type_desc)) = doc).

    % A formatter_map maps types to pps. Types are identified by module name,
    % type name, and type arity.
    %
:- type formatter_map.

    % Construct a new formatter_map.
    %
:- func new_formatter_map = formatter_map.

    % set_formatter(ModuleName, TypeName, TypeArity, Formatter, !FMap):
    %
    % Update !FMap to use Formatter to format values whose type is
    % ModuleName.TypeName/TypeArity.
    %
:- pred set_formatter(string::in, string::in, int::in, formatter::in,
    formatter_map::in, formatter_map::out) is det.

    % Values of this type identify a type that has an entry in a formatter_map.
    %
:- type formatter_map_entry
    --->    formatter_map_entry(string, string, int).
            % ModuleName.TypeName/TypeArity.

:- func get_formatter_map_entry_types(formatter_map) =
    list(formatter_map_entry).

%---------------------%

    % The func_symbol_limit type controls *how many* of the function symbols
    % stored in the term inside a format_univ, format_list, or format_term doc
    % the write_doc family of functions should include in the resulting string.
    %
    % A limit of linear(N) formats the first N functors before truncating
    % output to "...".
    %
    % A limit of triangular(N) formats a term t(X1, ..., Xn) by applying
    % the following limits:
    %
    % - triangular(N - 1) when formatting X1,
    % - triangular(N - 2) when formatting X2,
    % - ..., and
    % - triangular(N - n) when formatting Xn.
    %
    % The cost of formatting the term t(X1, ..., Xn) as a whole is just one,
    % so a sequence of terms T1, T2, ... is formatted with limits
    % triangular(N), triangular(N - 1), ... respectively. When the limit
    % is exhausted, terms are output as just "...".
    %
:- type func_symbol_limit
    --->    linear(int)
    ;       triangular(int).

    % The pp_params type contains the parameters of the prettyprinting process:
    %
    % - the width of each line,
    % - the maximum number of lines to print, and
    % - the controls for how many function symbols to print.
    %
:- type pp_params
    --->    pp_params(
                pp_line_width   :: int,
                pp_max_lines    :: int,
                pp_limit        :: func_symbol_limit
            ).

%---------------------%

    % A user-configurable default set of type-specific formatters and
    % formatting parameters is always attached to the I/O state.
    % The write_doc predicate (in both its arities) uses these settings.
    %
    % The get_default_formatter_map predicate reads the default formatter_map
    % from the current I/O state, while set_default_formatter_map writes
    % the specified formatter_map to the I/O state to become the new default.
    %
    % The initial value of the default formatter_map provides the means
    % to prettyprint the most commonly used types in the Mercury standard
    % library, such as arrays, chars, floats, ints, maps, strings, etc.
    %
    % The default formatter_map may also be updated by users' modules
    % (e.g. in initialisation goals).
    %
    % These defaults are thread local, and therefore changes made by one thread
    % to the default formatter_map will not be visible in another thread.
    %
:- pred get_default_formatter_map(formatter_map::out, io::di, io::uo) is det.
:- pred set_default_formatter_map(formatter_map::in, io::di, io::uo) is det.

    % set_default_formatter(ModuleName, TypeName, TypeArity, Formatter, !IO):
    %
    % Update the default formatter in the I/O state to use Formatter
    % to print values of the type ModuleName.TypeName/TypeArity.
    %
:- pred set_default_formatter(string::in, string::in, int::in, formatter::in,
    io::di, io::uo) is det.

    % Alongside the default formatter_map, the I/O state also always stores
    % a default set of pretty-printing parameters (pp_params) for use by
    % the write_doc predicate (in both its arities).
    %
    % The get_default_params predicate reads the default parameters
    % from the current I/O state, while set_default_params writes the specified
    % parameters to the I/O state to become the new default.
    %
    % The initial default parameters are pp_params(78, 100, triangular(100)).
    %
    % These defaults are thread local, and therefore changes made by one thread
    % to the default pp_params will not be visible in another thread.
    %
:- pred get_default_params(pp_params::out, io::di, io::uo) is det.
:- pred set_default_params(pp_params::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Convert a char to a doc.
    %
:- func char_to_doc(char) = doc.

    % Convert a string to a doc.
    %
:- func string_to_doc(string) = doc.

    % Convert a float to a doc.
    %
:- func float_to_doc(float) = doc.

    % Convert an int to a doc.
    %
:- func int_to_doc(int) = doc.
:- func int8_to_doc(int8) = doc.
:- func int16_to_doc(int16) = doc.
:- func int32_to_doc(int32) = doc.
:- func int64_to_doc(int64) = doc.

    % Convert a uint to a doc.
    %
:- func uint_to_doc(uint) = doc.
:- func uint8_to_doc(uint8) = doc.
:- func uint16_to_doc(uint16) = doc.
:- func uint32_to_doc(uint32) = doc.
:- func uint64_to_doc(uint64) = doc.

    % Convert an array to a doc.
    %
:- func array_to_doc(array(T)) = doc.

    % Convert a list to a doc.
    %
:- func list_to_doc(list(T)) = doc.

    % Convert a nonempty list to a doc.
    %
:- func one_or_more_to_doc(one_or_more(T)) = doc.

    % Convert a 2-3-4 tree to a doc.
    %
:- func tree234_to_doc(tree234(K, V)) = doc.

    % Convert a version array to a doc.
    %
:- func version_array_to_doc(version_array(T)) = doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module ops.
:- import_module require.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

    % Consider {add,remove}_indent as (), and {inc,dec}_std_indent as [].
    % The indent-related operations must occur as balanced pairs, meaning
    % that matching sequences such as (([])), ([][]) and [([])] are allowed,
    % but non-matching sequences such as ([)] are not.
    %
    % Since the definition of the pp_internal type is private to this module,
    % code outside this module cannot violate this requirement; only code
    % inside this module can. If it does, that is a bug.
:- type pp_internal
    --->    open_group
            % Mark the start of a group.

    ;       close_group
            % Mark the end of a group.

    ;       add_indent(string)
            % Extend the current indent stack with the given string.
    ;       remove_indent
            % Restore indentation to before the last add_indent/1.

            % Calling the two operations above {inc,dec}_user_indent
            % would be more consistent with the two operations below,
            % but the hard_coded/test_pretty_printer test case references
            % these names, even though they are supposed to be private.

    ;       inc_std_indent
            % Add a standard indentation level.
    ;       dec_std_indent
            % Remove a standard indentation level.

    ;       set_op_priority(ops.priority)
            % Set the current priority for printing operator terms with the
            % correct parenthesisation.

    ;       set_limit(func_symbol_limit).
            % Set the truncation limit.

    % Maps module names (first map), type names (second map) and type arities
    % (third map) to the formatter to be used when printing values of the type
    % ModuleName.TypeName/TypeArity.
    %
:- type formatter_map == map(string, map(string, map(int, formatter))).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

indent(Indent, Docs) =
    docs([
        pp_internal(add_indent(Indent)),
        docs(Docs),
        pp_internal(remove_indent)
    ]).

indent(Docs) =
    docs([
        pp_internal(inc_std_indent),
        docs(Docs),
        pp_internal(dec_std_indent)
    ]).

group(Docs) =
    docs([pp_internal(open_group), docs(Docs), pp_internal(close_group)]).

%---------------------------------------------------------------------------%

format(X) = format_univ(univ(X)).

format_arg(Doc) =
    docs([
        pp_internal(set_arg_priority),
        Doc
    ]).

:- func set_arg_priority = pp_internal.
:- pragma inline(func(set_arg_priority/0)).

set_arg_priority = set_op_priority(ops.mercury_op_table_arg_priority).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_export("C", write_doc_formatted(in, di, uo),
    "ML_write_doc_formatted").
:- pragma foreign_export("C", write_doc_formatted(in, in, di, uo),
    "ML_write_doc_formatted_to_stream").

write_doc_formatted(X, !IO) :-
    Doc = format(X),
    write_doc(io.stdout_stream, Doc, !IO).

write_doc_formatted(Stream, X, !IO) :-
    Doc = format(X),
    write_doc(Stream, Doc, !IO).

write_doc(Doc, !IO) :-
    write_doc(io.stdout_stream, Doc, !IO).

write_doc(Stream, Doc, !IO) :-
    get_default_formatter_map(Formatters, !IO),
    get_default_params(Params, !IO),
    promise_equivalent_solutions [!:IO] (
        put_doc(Stream, include_details_cc, Formatters, Params, Doc, !IO)
    ).

%---------------------------------------------------------------------------%

put_doc(Stream, Canonicalize, FMap, Params, Doc, !IO) :-
    Pri = ops.mercury_op_table_loosest_op_priority,
    Params = pp_params(LineWidth, MaxLines, Limit),
    RemainingWidth = LineWidth,
    Indents = indent_empty,
    do_put_docs(Stream, Canonicalize, FMap, LineWidth, [Doc],
        RemainingWidth, _, Indents, _, MaxLines, _, Limit, _, Pri, _, !IO).

%---------------------------------------------------------------------------%

    % do_put_docs(FMap, LineWidth, Docs, !RemainingWidth, !Indents,
    %   !RemainingLines, !Limit, !Pri, !IO):
    %
    % Format Docs to fit on LineWidth chars per line,
    % - tracking !RemainingWidth chars left on the current line,
    % - indenting by !Indents after newlines,
    % - truncating output after !RemainingLines,
    % - expanding terms to at most !Limit depth before truncating,
    % - tracking current operator priority !Pri.
    % Assumes that Docs is the output of expand.
    %
:- pred do_put_docs(Stream, noncanon_handling, formatter_map, int,
    list(doc), int, int, indent_stack, indent_stack, int, int,
    func_symbol_limit, func_symbol_limit,
    ops.priority, ops.priority, State, State)
    <= stream.writer(Stream, string, State).
:- mode do_put_docs(in, in(canonicalize), in, in, in,
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode do_put_docs(in, in(include_details_cc), in, in, in,
    in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- pragma type_spec(pred(do_put_docs/17),
    (Stream = io.output_stream, State = io.state)).

do_put_docs(_Stream, _Canonicalize, _FMap, _LineWidth, [],
        !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO).
do_put_docs(Stream, Canonicalize, FMap, LineWidth, [HeadDoc0 | TailDocs0],
        !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO) :-
    ( if !.RemainingLines =< 0 then
        stream.put(Stream, "...", !IO)
    else
        (
            % Output strings directly.
            HeadDoc0 = str(String),
            stream.put(Stream, String, !IO),
            StrWidth = string.count_codepoints(String),
            !:RemainingWidth = !.RemainingWidth - StrWidth,
            Docs = TailDocs0
        ;
            HeadDoc0 = nl,
            IndentWidth = count_indent_codepoints(!.Indents),
            ( if !.RemainingWidth < LineWidth - IndentWidth then
                format_nl(Stream, LineWidth, !.Indents, !:RemainingWidth,
                    !RemainingLines, !IO)
            else
                true
            ),
            Docs = TailDocs0
        ;
            HeadDoc0 = hard_nl,
            format_nl(Stream, LineWidth, !.Indents, !:RemainingWidth,
                !RemainingLines, !IO),
            Docs = TailDocs0
        ;
            HeadDoc0 = docs(HeadDocs0),
            Docs = list.(HeadDocs0 ++ TailDocs0)
        ;
            (
                HeadDoc0 = format_univ(Univ),
                expand_format_univ(Canonicalize, FMap, Univ, TailDocs0, Docs,
                    !Limit, !.Pri)
            ;
                HeadDoc0 = format_list(Univs, Sep),
                expand_format_list(Univs, Sep, TailDocs0, Docs, !Limit)
            ;
                HeadDoc0 = format_term(Name, Univs),
                expand_format_term(Name, Univs, TailDocs0, Docs, !Limit, !.Pri)
            ;
                HeadDoc0 = format_susp(Susp),
                expand_format_susp(Susp, TailDocs0, Docs, !Limit)
            )
        ;
            HeadDoc0 = pp_internal(Internal),
            (
                % Open groups: if the current group (and what follows,
                % up to the next nl) fits on the remainder of the current line,
                % then print it that way; otherwise we have to recognise
                % the nls in the group.
                Internal = open_group,
                OpenGroups1 = 1,
                CurrentRemainingWidth = !.RemainingWidth,
                expand_docs_to_line_end(Canonicalize, FMap, TailDocs0, Docs1,
                    OpenGroups1, !Limit, !Pri,
                    CurrentRemainingWidth, RemainingWidthAfterGroup),
                ( if RemainingWidthAfterGroup >= 0 then
                    output_current_group(Stream, LineWidth, !.Indents,
                        Docs1, Docs, OpenGroups1,
                        !RemainingWidth, !RemainingLines, !IO)
                else
                    Docs = Docs1
                )
            ;
                (
                    Internal = close_group
                ;
                    Internal = add_indent(IndentStr),
                    increment_user_indent(IndentStr, !Indents)
                ;
                    Internal = remove_indent,
                    decrement_user_indent(!Indents)
                ;
                    Internal = inc_std_indent,
                    increment_std_indent(!Indents)
                ;
                    Internal = dec_std_indent,
                    decrement_std_indent(!Indents)
                ;
                    Internal = set_limit(Limit),
                    !:Limit = Limit
                ;
                    Internal = set_op_priority(Pri),
                    !:Pri = Pri
                ),
                Docs = TailDocs0
            )
        ),
        do_put_docs(Stream, Canonicalize, FMap, LineWidth, Docs,
            !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO)
    ).

%---------------------%

:- type open_groups == int.

:- pred output_current_group(Stream::in, int::in, indent_stack::in,
    list(doc)::in, list(doc)::out, open_groups::in, int::in, int::out,
    int::in, int::out, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).
:- pragma type_spec(pred(output_current_group/12),
    (Stream = io.output_stream, State = io.state)).

output_current_group(_Stream, _LineWidth, _Indents, [], [],
        _OpenGroups, !RemainingWidth, !RemainingLines, !IO).
output_current_group(Stream, LineWidth, Indents, [HeadDoc0 | TailDocs0], Docs,
        !.OpenGroups, !RemainingWidth, !RemainingLines, !IO) :-
    (
        HeadDoc0 = str(String),
        stream.put(Stream, String, !IO),
        StrWidth = string.count_codepoints(String),
        !:RemainingWidth = !.RemainingWidth - StrWidth,
        output_current_group(Stream, LineWidth, Indents, TailDocs0, Docs,
            !.OpenGroups, !RemainingWidth, !RemainingLines, !IO)
    ;
        HeadDoc0 = hard_nl,
        format_nl(Stream, LineWidth, Indents, !:RemainingWidth,
            !RemainingLines, !IO),
        ( if !.RemainingLines =< 0 then
            Docs = TailDocs0
        else
            output_current_group(Stream, LineWidth, Indents, TailDocs0, Docs,
                !.OpenGroups, !RemainingWidth, !RemainingLines, !IO)
        )
    ;
        HeadDoc0 = pp_internal(Internal),
        (
            Internal = open_group,
            !:OpenGroups = !.OpenGroups + 1,
            output_current_group(Stream, LineWidth, Indents, TailDocs0, Docs,
                !.OpenGroups, !RemainingWidth, !RemainingLines, !IO)
        ;
            Internal = close_group,
            ( if !.OpenGroups = 1 then
                Docs = TailDocs0
            else
                !:OpenGroups = !.OpenGroups - 1,
                output_current_group(Stream, LineWidth, Indents,
                    TailDocs0, Docs, !.OpenGroups,
                    !RemainingWidth, !RemainingLines, !IO)
            )
        ;
            ( Internal = add_indent(_)
            ; Internal = remove_indent
            ; Internal = inc_std_indent
            ; Internal = dec_std_indent
            ; Internal = set_op_priority(_)
            ; Internal = set_limit(_)
            ),
            output_current_group(Stream, LineWidth, Indents, TailDocs0, Docs,
                !.OpenGroups, !RemainingWidth, !RemainingLines, !IO)
        )
    ;
        ( HeadDoc0 = nl
        ; HeadDoc0 = docs(_)
        ; HeadDoc0 = format_univ(_)
        ; HeadDoc0 = format_list(_, _)
        ; HeadDoc0 = format_term(_, _)
        ; HeadDoc0 = format_susp(_)
        ),
        output_current_group(Stream, LineWidth, Indents, TailDocs0, Docs,
            !.OpenGroups, !RemainingWidth, !RemainingLines, !IO)
    ).

%---------------------%

    % expand_docs_to_line_end(Canonicalize, Docs0, Docs,
    %   !.OpenGroups, !Limit, !Pri, !RemainingWidth)
    %
    % expands out any doc(_), pp_univ(_), format_list(_, _), and pp_term(_)
    % constructors in Docs0 into Docs, until either
    %
    % - Docs0 has been completely expanded, or
    % - a nl is encountered, or
    % - the remaining space on the current line has been accounted for.
    %
    % !.OpenGroups is used to track nested groups.
    % !Limit tracks the limits after accounting for expansion.
    % !Pri tracks the operator priority after accounting for expansion.
    % !RemainingWidth tracks the remaining line width after accounting for
    % expansion.
    %
:- pred expand_docs_to_line_end(noncanon_handling, formatter_map,
    list(doc), list(doc), open_groups, func_symbol_limit, func_symbol_limit,
    ops.priority, ops.priority, int, int).
:- mode expand_docs_to_line_end(in(canonicalize), in,
    in, out, in, in, out, in, out, in, out) is det.
:- mode expand_docs_to_line_end(in(include_details_cc), in,
    in, out, in, in, out, in, out, in, out) is cc_multi.

expand_docs_to_line_end(_, _, [], [], _OpenGroups,
        !Limit, !Pri, !RemainingWidth).
expand_docs_to_line_end(Canonicalize, FMap,
        Docs0 @ [HeadDoc0 | TailDocs0], Docs,
        !.OpenGroups, !Limit, !Pri, !RemainingWidth) :-
    ( if
        (
            !.OpenGroups =< 0, ( HeadDoc0 = nl ; HeadDoc0 = hard_nl )
            % We have found the first nl after the close of the current
            % open group.
        ;
            !.RemainingWidth < 0
            % We have run out of space on this line: the current open
            % group will not fit.
        )
    then
        Docs = Docs0
    else
        (
            HeadDoc0 = str(String),
            StrWidth = string.count_codepoints(String),
            !:RemainingWidth = !.RemainingWidth - StrWidth,
            expand_docs_to_line_end(Canonicalize, FMap, TailDocs0, TailDocs,
                !.OpenGroups, !Limit, !Pri, !RemainingWidth),
            Docs = [HeadDoc0 | TailDocs]
        ;
            ( HeadDoc0 = nl
            ; HeadDoc0 = hard_nl
            ),
            ( if !.OpenGroups =< 0 then
                Docs = Docs0
            else
                expand_docs_to_line_end(Canonicalize, FMap,
                    TailDocs0, TailDocs,
                    !.OpenGroups, !Limit, !Pri, !RemainingWidth),
                Docs = [HeadDoc0 | TailDocs]
            )
        ;
            HeadDoc0 = docs(HeadDocs0),
            Docs1 = list.(HeadDocs0 ++ TailDocs0),
            expand_docs_to_line_end(Canonicalize, FMap, Docs1, Docs,
                !.OpenGroups, !Limit, !Pri, !RemainingWidth)
        ;
            (
                HeadDoc0 = format_univ(HeadUniv),
                expand_format_univ(Canonicalize, FMap, HeadUniv,
                    TailDocs0, Docs1, !Limit, !.Pri)
            ;
                HeadDoc0 = format_list(HeadUnivs, Sep),
                expand_format_list(HeadUnivs, Sep, TailDocs0, Docs1, !Limit)
            ;
                HeadDoc0 = format_term(Name, HeadUnivs),
                expand_format_term(Name, HeadUnivs, TailDocs0, Docs1, !Limit,
                    !.Pri)
            ;
                HeadDoc0 = format_susp(HeadSusp),
                expand_format_susp(HeadSusp, TailDocs0, Docs1, !Limit)
            ),
            expand_docs_to_line_end(Canonicalize, FMap, Docs1, Docs,
                !.OpenGroups, !Limit, !Pri, !RemainingWidth)
        ;
            HeadDoc0 = pp_internal(Internal),
            (
                (
                    Internal = add_indent(_)
                ;
                    Internal = remove_indent
                ;
                    Internal = inc_std_indent
                ;
                    Internal = dec_std_indent
                ;
                    Internal = open_group,
                    % XXX This is probably a bug, because if !.OpenGroups = 0,
                    % then it will *stay* at 0 even *after* this opening
                    % of a group.
                    !:OpenGroups = !.OpenGroups +
                        ( if !.OpenGroups > 0 then 1 else 0 )
                ;
                    Internal = close_group,
                    !:OpenGroups = !.OpenGroups -
                        ( if !.OpenGroups > 0 then 1 else 0 )
                ),
                expand_docs_to_line_end(Canonicalize, FMap,
                    TailDocs0, TailDocs,
                    !.OpenGroups, !Limit, !Pri, !RemainingWidth),
                Docs = [HeadDoc0 | TailDocs]
            ;
                (
                    Internal = set_limit(Limit),
                    !:Limit = Limit
                ;
                    Internal = set_op_priority(Pri),
                    !:Pri = Pri
                ),
                expand_docs_to_line_end(Canonicalize, FMap, TailDocs0, Docs,
                    !.OpenGroups, !Limit, !Pri, !RemainingWidth)
            )
        )
    ).

%---------------------%

    % Output a newline followed by indentation.
    %
:- pred format_nl(Stream::in, int::in, indent_stack::in, int::out,
    int::in, int::out, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).
:- pragma type_spec(pred(format_nl/8),
    (Stream = io.output_stream, State = io.state)).

format_nl(Stream, LineWidth, Indents, RemainingWidth, !RemainingLines, !IO) :-
    stream.put(Stream, "\n", !IO),
    RemainingWidth = LineWidth - count_indent_codepoints(Indents),
    output_indent_stack(Stream, Indents, !IO),
    !:RemainingLines = !.RemainingLines - 1.

:- pred output_indent_stack(Stream::in, indent_stack::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).
:- pragma type_spec(pred(output_indent_stack/4),
    (Stream = io.output_stream, State = io.state)).

output_indent_stack(Stream, IndentStack, !IO) :-
    (
        IndentStack = indent_empty
    ;
        IndentStack = indent_user(PrevStack, IndentStr, _NumCPs),
        output_indent_stack(Stream, PrevStack, !IO),
        stream.put(Stream, IndentStr, !IO)
    ;
        IndentStack = indent_std(PrevStack, IndentLevels, _NumCPs),
        output_indent_stack(Stream, PrevStack, !IO),
        output_std_indent_levels(Stream, IndentLevels, !IO)
    ).

:- pred output_std_indent_levels(Stream::in, int::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).
:- pragma type_spec(pred(output_std_indent_levels/4),
    (Stream = io.output_stream, State = io.state)).

output_std_indent_levels(Stream, NumLevels, !IO) :-
    % We try to amortize the overhead of stream.put over as large a part
    % of the overall indentation as we can.
    ( if NumLevels >= 30 then
        std_indent_30(IndentStr),
        stream.put(Stream, IndentStr, !IO),
        output_std_indent_levels(Stream, NumLevels - 30, !IO)
    else if NumLevels > 0 then
        ( if std_indent(NumLevels, IndentStr) then
            stream.put(Stream, IndentStr, !IO)
        else
            unexpected($pred, "std_indent failed")
        )
    else
        true
    ).

:- pred std_indent(int::in, string::out) is semidet.
:- pred std_indent_30(string::out) is det.

std_indent(1,  "  ").
std_indent(2,  "    ").
std_indent(3,  "      ").
std_indent(4,  "        ").
std_indent(5,  "          ").
std_indent(6,  "            ").
std_indent(7,  "              ").
std_indent(8,  "                ").
std_indent(9,  "                  ").
std_indent(10, "                    ").
std_indent(11, "                      ").
std_indent(12, "                        ").
std_indent(13, "                          ").
std_indent(14, "                            ").
std_indent(15, "                              ").
std_indent(16, "                                ").
std_indent(17, "                                  ").
std_indent(18, "                                    ").
std_indent(19, "                                      ").
std_indent(20, "                                        ").
std_indent(21, "                                          ").
std_indent(22, "                                            ").
std_indent(23, "                                              ").
std_indent(24, "                                                ").
std_indent(25, "                                                  ").
std_indent(26, "                                                    ").
std_indent(27, "                                                      ").
std_indent(28, "                                                        ").
std_indent(29, "                                                          ").
std_indent_30( "                                                            ").

%---------------------%

    % Expand a univ into docs using the first pretty-printer in the given list
    % that succeeds, otherwise use the generic pretty- printer. If the
    % pretty-printer limit has been exhausted, then generate only "...".
    %
:- pred expand_format_univ(noncanon_handling, formatter_map, univ,
    list(doc), list(doc), func_symbol_limit, func_symbol_limit, ops.priority).
:- mode expand_format_univ(in(canonicalize), in, in,
    in, out, in, out, in) is det.
:- mode expand_format_univ(in(include_details_cc), in, in,
    in, out, in, out, in) is cc_multi.

expand_format_univ(Canonicalize, FMap, Univ, TailDocs, Docs,
        !Limit, CurrentPri) :-
    ( if func_limit_reached(!.Limit) then
        Docs = [ellipsis | TailDocs]
    else
        Value = univ_value(Univ),
        ( if
            type_ctor_and_args(type_of(Value), TypeCtorDesc, ArgTypeDescs),
            ModuleName = type_ctor_module_name(TypeCtorDesc),
            map.search(FMap, ModuleName, FMapTypeArity),
            TypeName = type_ctor_name(TypeCtorDesc),
            map.search(FMapTypeArity, TypeName, FMapArity),
            list.length(ArgTypeDescs, Arity),
            map.search(FMapArity, Arity, Formatter)
        then
            decrement_func_limit(!Limit),
            Doc0 = Formatter(Univ, ArgTypeDescs),
            set_func_limit_in_doc(!.Limit, Doc0, TailDocs, Docs)
        else
            deconstruct(Value, Canonicalize, Name, _Arity, Args),
            expand_format_term(Name, Args, TailDocs, Docs, !Limit, CurrentPri)
        )
    ).

%---------------------%

    % Expand a list of univs into docs using the given separator.
    %
:- pred expand_format_list(list(univ)::in, doc::in, list(doc)::in,
    list(doc)::out, func_symbol_limit::in, func_symbol_limit::out) is det.

expand_format_list([], _Sep, TailDocs, Docs, !Limit) :-
    Docs = TailDocs.
expand_format_list([HeadUniv | TailUnivs], Sep, TailDocs, Docs, !Limit) :-
    ( if func_limit_reached(!.Limit) then
        Docs = [ellipsis | TailDocs]
    else
        (
            TailUnivs = [],
            Docs = [
                pp_internal(set_arg_priority),
                pp_internal(open_group),
                nl,
                format_univ(HeadUniv),
                pp_internal(close_group)
                | TailDocs
            ]
        ;
            TailUnivs = [_ | _],
            Docs = [
                pp_internal(set_arg_priority),
                pp_internal(open_group),
                nl,
                format_univ(HeadUniv),
                Sep,
                pp_internal(close_group),
                format_list(TailUnivs, Sep)
                | TailDocs
            ]
        )
    ).

    % Expand a name and list of univs into docs corresponding to Mercury
    % term syntax.
    %
:- pred expand_format_term(string::in, list(univ)::in,
    list(doc)::in, list(doc)::out,
    func_symbol_limit::in, func_symbol_limit::out, ops.priority::in) is det.

expand_format_term(Name, Args, TailDocs, Docs, !Limit, CurrentPri) :-
    ( if Args = [] then
        HeadDoc0 = str(term_io.quoted_atom(Name)),
        decrement_func_limit(!Limit),
        set_func_limit_in_doc(!.Limit, HeadDoc0, TailDocs, Docs)
    else if func_limit_reached(!.Limit) then
        HeadDoc0 = ellipsis,
        % There *should* be no point im decrementing !Limit even further.
        decrement_func_limit(!Limit),
        set_func_limit_in_doc(!.Limit, HeadDoc0, TailDocs, Docs)
    else if expand_format_op(Name, Args, CurrentPri, OpDocs) then
        decrement_func_limit(!Limit),
        set_func_limit_in_docs(!.Limit, OpDocs, TailDocs, Docs)
    else
        ( if Name = "{}" then
            HeadDocs0 = [
                str("{"),
                pp_internal(inc_std_indent),
                format_list(Args, str(", ")),
                pp_internal(dec_std_indent),
                str("}")
            ]
        else
            HeadDocs0 = [
                pp_internal(open_group),
                nl,
                str(term_io.quoted_atom(Name)),
                str("("),
                pp_internal(inc_std_indent),
                format_list(Args, str(", ")),
                pp_internal(dec_std_indent),
                str(")"),
                pp_internal(close_group)
            ]
        ),
        decrement_func_limit(!Limit),
        set_func_limit_in_docs(!.Limit, HeadDocs0, TailDocs, Docs)
    ).

    % Expand a function symbol name and list of univs representing its
    % arguments into docs corresponding to Mercury operator syntax.
    %
:- pred expand_format_op(string::in, list(univ)::in, ops.priority::in,
    list(doc)::out) is semidet.

expand_format_op(Op, Args, EnclosingPriority, Docs) :-
    % XXX With one exception, all the set_op_priority pp_internals are
    % created here. They are intended set the priority of one argument,
    % which in this case is equivalent to setting the priority from
    % the occurrence of the set_op_priority until either the next
    % set_op_priority, or until the next close_group. The fact that
    % the predicates above do NOT reset the priority when they get to
    % a close_group seems to me to be a bug -zs.
    %
    % The one exception is the format_arg function, which appears
    % to have the same bug, but in an even worse form, since it does not
    % wrap an open_group/close_group pair around the argument.
    (
        Args = [ArgA],
        ops.mercury_op_table_search_op_infos(Op, OpInfos),
        ( if OpInfos ^ oi_prefix = pre(Pri, GtOrGeA) then
            OpPriority = Pri,
            PriorityArgA = min_priority_for_arg(OpPriority, GtOrGeA),
            Docs0 = [
                pp_internal(open_group),
                str(Op),
                pp_internal(set_op_priority(PriorityArgA)),
                format_univ(ArgA),
                pp_internal(close_group)
            ]
        else if OpInfos ^ oi_postfix = post(Pri, GtOrGeA) then
            OpPriority = Pri,
            PriorityArgA = min_priority_for_arg(OpPriority, GtOrGeA),
            Docs0 = [
                pp_internal(open_group),
                pp_internal(set_op_priority(PriorityArgA)),
                format_univ(ArgA),
                str(Op),
                pp_internal(close_group)
            ]
        else
            fail
        )
    ;
        Args = [ArgA, ArgB],
        ops.mercury_op_table_search_op_infos(Op, OpInfos),
        ( if
            OpInfos ^ oi_infix = in(Pri, GtOrGeA, GtOrGeB)
        then
            OpPriority = Pri,
            PriorityArgA = min_priority_for_arg(OpPriority, GtOrGeA),
            PriorityArgB = min_priority_for_arg(OpPriority, GtOrGeB),
            Docs0 = [
                pp_internal(open_group),
                pp_internal(set_op_priority(PriorityArgA)),
                format_univ(ArgA),
                ( if Op = "." then
                    str(Op)
                else
                    docs([str(" "), str(Op), str(" ")])
                ),
                pp_internal(inc_std_indent),
                nl,
                pp_internal(set_op_priority(PriorityArgB)),
                format_univ(ArgB),
                pp_internal(dec_std_indent),
                pp_internal(close_group)
            ]
        else if
            OpInfos ^ oi_binary_prefix = bin_pre(Pri, GtOrGeA, GtOrGeB)
        then
            OpPriority = Pri,
            PriorityArgA = min_priority_for_arg(OpPriority, GtOrGeA),
            PriorityArgB = min_priority_for_arg(OpPriority, GtOrGeB),
            Docs0 = [
                pp_internal(open_group),
                str(Op),
                str(" "),
                pp_internal(set_op_priority(PriorityArgA)),
                format_univ(ArgA),
                str(" "),
                pp_internal(inc_std_indent),
                pp_internal(set_op_priority(PriorityArgB)),
                format_univ(ArgB),
                pp_internal(dec_std_indent),
                pp_internal(close_group)
            ]
        else
            fail
        )
    ),
    % Add parentheses around a doc if required by operator priority.
    ( if priority_lt(OpPriority, EnclosingPriority) then
        Docs = [str("(") | Docs0] ++ [str(")")]
    else
        Docs = Docs0
    ).

%---------------------%

:- pred expand_format_susp(((func) = doc)::in, list(doc)::in, list(doc)::out,
    func_symbol_limit::in, func_symbol_limit::out) is det.

expand_format_susp(Susp, TailDocs, Docs, !Limit) :-
    ( if func_limit_reached(!.Limit) then
        Docs = [ellipsis | TailDocs]
    else
        decrement_func_limit(!Limit),
        HeadDoc0 = apply(Susp),
        set_func_limit_in_doc(!.Limit, HeadDoc0, TailDocs, Docs)
    ).

%---------------------%

:- func ellipsis = doc.

ellipsis = str("...").

%---------------------%

    % Update the limits properly after processing a term.
    %
:- pred set_func_limit_in_doc(func_symbol_limit::in,
    doc::in, list(doc)::in, list(doc)::out) is det.

set_func_limit_in_doc(linear(_), HeadDoc, TailDocs, [HeadDoc | TailDocs]).
set_func_limit_in_doc(Limit @ triangular(_), HeadDoc0, TailDocs, Docs) :-
    Docs = [HeadDoc0, pp_internal(set_limit(Limit)) | TailDocs].

    % Update the limits properly after processing a term.
    %
:- pred set_func_limit_in_docs(func_symbol_limit::in,
    list(doc)::in, list(doc)::in, list(doc)::out) is det.

set_func_limit_in_docs(linear(_), HeadDocs, TailDocs, HeadDocs ++ TailDocs).
set_func_limit_in_docs(Limit @ triangular(_), HeadDocs0, TailDocs, Docs) :-
    Docs = HeadDocs0 ++ [pp_internal(set_limit(Limit)) | TailDocs].

    % Succeeds if the pretty-printer state limits have been used up.
    %
:- pred func_limit_reached(func_symbol_limit::in) is semidet.

func_limit_reached(linear(N)) :-
    N =< 0.
func_limit_reached(triangular(N)) :-
    N =< 0.

    % Reduce the pretty-printer limit by one.
    %
:- pred decrement_func_limit(func_symbol_limit::in, func_symbol_limit::out)
    is det.

decrement_func_limit(linear(N), linear(N - 1)).
decrement_func_limit(triangular(N), triangular(N - 1)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type indent_stack
    --->    indent_empty
            % No indent at all. Number of code points is zero.
    ;       indent_user(
                % The indentation after which this indentation is added.
                user_prevstack          :: indent_stack,

                % Indentation added by a non-standard indent() function.
                user_indent_string      :: string,

                % The total number of code points in user_prevstack and
                % user_indent_string. Must be the sum of
                % count_indent_codepoints(user_prevstack) and
                % string.count_codepoints(user_indent_string).
                user_total_code_points  :: int
            )
    ;       indent_std(
                % The indentation after which this indentation is added.
                std_prevstack           :: indent_stack,

                % The number of extra standard indent levels added
                % after std_prevstack. Each indent level consists of two
                % spaces.
                std_extra_indent_levels :: int,

                % The total number of code points in user_prevstack and
                % user_extra_indent. Must be the sum of
                % count_indent_codepoints(std_prevstack) and
                % 2 * std_extra_indent_levels.
                std_total_code_points   :: int
            ).

:- func count_indent_codepoints(indent_stack) = int.

count_indent_codepoints(indent_empty) = 0.
count_indent_codepoints(indent_user(_PrevStack, _Str, NumCPs)) = NumCPs.
count_indent_codepoints(indent_std(_PrevStack, _NL, NumCPs)) = NumCPs.

:- pred increment_user_indent(string::in, indent_stack::in, indent_stack::out)
    is det.

increment_user_indent(IndentStr, IndentStack0, IndentStack) :-
    NumCPs0 = count_indent_codepoints(IndentStack0),
    NumCPs = NumCPs0 + string.count_codepoints(IndentStr),
    IndentStack = indent_user(IndentStack0, IndentStr, NumCPs).

:- pred decrement_user_indent(indent_stack::in, indent_stack::out) is det.

decrement_user_indent(IndentStack0, IndentStack) :-
    (
        IndentStack0 = indent_user(IndentStack, _, _)
    ;
        ( IndentStack0 = indent_empty
        ; IndentStack0 = indent_std(_, _, _)
        ),
        unexpected($pred, "last indent is not user indent")
    ).

:- pred increment_std_indent(indent_stack::in, indent_stack::out) is det.

increment_std_indent(IndentStack0, IndentStack) :-
    (
        IndentStack0 = indent_user(_, _, NumCPs0),
        NumCPs = NumCPs0 + 2,
        IndentStack = indent_std(IndentStack0, 1, NumCPs)
    ;
        IndentStack0 = indent_empty,
        NumCPs = 2,
        IndentStack = indent_std(IndentStack0, 1, NumCPs)
    ;
        IndentStack0 = indent_std(PrevStack, NumLevels0, NumCPs0),
        NumLevels = NumLevels0 + 1,
        NumCPs = NumCPs0 + 2,
        IndentStack = indent_std(PrevStack, NumLevels, NumCPs)
    ).

:- pred decrement_std_indent(indent_stack::in, indent_stack::out) is det.

decrement_std_indent(IndentStack0, IndentStack) :-
    (
        IndentStack0 = indent_std(PrevStack, NumLevels0, NumCPs0),
        NumLevels = NumLevels0 - 1,
        ( if NumLevels > 0 then
            NumCPs = NumCPs0 - 2,
            IndentStack = indent_std(PrevStack, NumLevels, NumCPs)
        else
            IndentStack = PrevStack
        )
    ;
        ( IndentStack0 = indent_empty
        ; IndentStack0 = indent_user(_, _, _)
        ),
        unexpected($pred, "last indent is not std indent")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

new_formatter_map = map.init.

set_formatter(ModuleName, TypeName, Arity, Formatter, !FMap) :-
    ( if map.search(!.FMap, ModuleName, FMapTypeArity0) then
        ( if map.search(FMapTypeArity0, TypeName, FMapArity0) then
            map.det_update(Arity, Formatter, FMapArity0, FMapArity),
            map.det_update(TypeName, FMapArity, FMapTypeArity0, FMapTypeArity)
        else
            FMapArity = map.singleton(Arity, Formatter),
            map.det_insert(TypeName, FMapArity, FMapTypeArity0, FMapTypeArity)
        ),
        map.det_update(ModuleName, FMapTypeArity, !FMap)
    else
        FMapArity = map.singleton(Arity, Formatter),
        FMapTypeArity = map.singleton(TypeName, FMapArity),
        map.det_insert(ModuleName, FMapTypeArity, !FMap)
    ).

%---------------------%

get_formatter_map_entry_types(FMap) = Entries :-
    % To allocate as few cons cells as possible, build Entries from the back.
    map.foldr(get_fmap_entries_module, FMap, [], Entries).

:- pred get_fmap_entries_module(string::in,
    map(string, map(int, formatter))::in,
    list(formatter_map_entry)::in, list(formatter_map_entry)::out) is det.

get_fmap_entries_module(ModuleName, TypeNameArityMap, !Entries) :-
    map.foldr(get_fmap_entries_type(ModuleName), TypeNameArityMap, !Entries).

:- pred get_fmap_entries_type(string::in, string::in, map(int, formatter)::in,
    list(formatter_map_entry)::in, list(formatter_map_entry)::out) is det.

get_fmap_entries_type(ModuleName, TypeName, ArityMap, !Entries) :-
    map.foldr(get_fmap_entries_arity(ModuleName, TypeName),
        ArityMap, !Entries).

:- pred get_fmap_entries_arity(string::in, string::in, int::in, formatter::in,
    list(formatter_map_entry)::in, list(formatter_map_entry)::out) is det.

get_fmap_entries_arity(ModuleName, TypeName, Arity, _Formatter, !Entries) :-
    Entry = formatter_map_entry(ModuleName, TypeName, Arity),
    !:Entries = [Entry | !.Entries].

%---------------------%

get_default_formatter_map(FMap, !IO) :-
    pretty_printer_is_initialised(Okay, !IO),
    (
        Okay = no,
        FMap = initial_formatter_map,
        set_default_formatter_map(FMap, !IO)
    ;
        Okay = yes,
        unsafe_get_default_formatter_map(FMap, !IO)
    ).

% set_default_formatter_map is implemented using only foreign_procs,
% with no Mercury code.

set_default_formatter(ModuleName, TypeName, TypeArity, Formatter, !IO) :-
    get_default_formatter_map(FMap0, !IO),
    set_formatter(ModuleName, TypeName, TypeArity, Formatter, FMap0, FMap),
    set_default_formatter_map(FMap, !IO).

%---------------------%

    % Because there is no guaranteed order of module initialisation, we need
    % to ensure that we do the right thing if other modules try to update the
    % default formatter_map before this module has been initialised.
    %
    % All of this machinery is needed to avoid a race condition between
    % initialise directives and initialisation of mutables.
    %
:- pragma foreign_decl("C",
"
    extern MR_Bool ML_pretty_printer_is_initialised;
    extern MR_Word ML_pretty_printer_default_formatter_map;
").
:- pragma foreign_code("C",
"
    MR_Bool ML_pretty_printer_is_initialised = MR_NO;
    MR_Word ML_pretty_printer_default_formatter_map = 0;
").

:- pragma foreign_code("C#",
"
    static mr_bool.Bool_0 isInitialised = mr_bool.NO;
    static tree234.Tree234_2 defaultFormatterMap = null;
").

:- pragma foreign_code("Java",
"
    static bool.Bool_0 isInitialised = bool.NO;
    static tree234.Tree234_2<String,
            tree234.Tree234_2<String,
             tree234.Tree234_2<Integer, /* closure */ java.lang.Object[]>>>
                defaultFormatterMap = null;
").

%---------------------%

:- pred pretty_printer_is_initialised(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    pretty_printer_is_initialised(Okay::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Okay = ML_pretty_printer_is_initialised;
").

:- pragma foreign_proc("C#",
    pretty_printer_is_initialised(Okay::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    Okay = pretty_printer.isInitialised;
").

:- pragma foreign_proc("Java",
    pretty_printer_is_initialised(Okay::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    Okay = pretty_printer.isInitialised;
").

%---------------------%

    % This predicate must not be called unless pretty_printer_is_initialised ==
    % MR_TRUE, which occurs when set_default_formatter_map has been called at
    % least once.
    %
:- pred unsafe_get_default_formatter_map(formatter_map::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    unsafe_get_default_formatter_map(FMap::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    FMap = ML_pretty_printer_default_formatter_map;
").

:- pragma foreign_proc("C#",
    unsafe_get_default_formatter_map(FMap::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    FMap = pretty_printer.defaultFormatterMap;
").

:- pragma foreign_proc("Java",
    unsafe_get_default_formatter_map(FMap::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    FMap = pretty_printer.defaultFormatterMap;
").

%---------------------%

:- pragma foreign_proc("C",
    set_default_formatter_map(FMap::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    ML_pretty_printer_default_formatter_map = FMap;
    ML_pretty_printer_is_initialised = MR_TRUE;
").

:- pragma foreign_proc("C#",
    set_default_formatter_map(FMap::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, may_not_duplicate],
"
    pretty_printer.isInitialised = mr_bool.YES;
    pretty_printer.defaultFormatterMap = FMap;
").

:- pragma foreign_proc("Java",
    set_default_formatter_map(FMap::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, may_not_duplicate],
"
    pretty_printer.isInitialised = bool.YES;
    pretty_printer.defaultFormatterMap = FMap;
").

%---------------------------------------------------------------------------%

    % Construct the initial default formatter map. This function
    % should be extended as more specialised formatters are added
    % to the standard library modules.
    %
:- func initial_formatter_map = formatter_map.

initial_formatter_map = !:Formatters :-
    !:Formatters = new_formatter_map,
    set_formatter("builtin", "character", 0, fmt_char,    !Formatters),
    set_formatter("builtin", "float",     0, fmt_float,   !Formatters),
    set_formatter("builtin", "int",       0, fmt_int,     !Formatters),
    set_formatter("builtin", "int8",      0, fmt_int8,    !Formatters),
    set_formatter("builtin", "int16",     0, fmt_int16,   !Formatters),
    set_formatter("builtin", "int32",     0, fmt_int32,   !Formatters),
    set_formatter("builtin", "int64",     0, fmt_int64,   !Formatters),
    set_formatter("builtin", "uint",      0, fmt_uint,    !Formatters),
    set_formatter("builtin", "uint8",     0, fmt_uint8,   !Formatters),
    set_formatter("builtin", "uint16",    0, fmt_uint16,  !Formatters),
    set_formatter("builtin", "int32",     0, fmt_uint32,  !Formatters),
    set_formatter("builtin", "uint64",    0, fmt_uint64,  !Formatters),
    set_formatter("builtin", "string",    0, fmt_string,  !Formatters),
    set_formatter("array",   "array",     1, fmt_array,   !Formatters),
    set_formatter("list",    "list",      1, fmt_list,    !Formatters),
    set_formatter("one_or_more", "one_or_more",
                                          1, fmt_one_or_more, !Formatters),
    set_formatter("tree234", "tree234",   2, fmt_tree234, !Formatters),
    set_formatter("version_array", "version_array",
                                          1, fmt_version_array, !Formatters).

%---------------------%

:- func fmt_char(univ, list(type_desc)) = doc.

fmt_char(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.char_to_doc(X)
    else
        str("?char?")
    ).

:- func fmt_float(univ, list(type_desc)) = doc.

fmt_float(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.float_to_doc(X)
    else
        str("?float?")
    ).

:- func fmt_int(univ, list(type_desc)) = doc.

fmt_int(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.int_to_doc(X)
    else
        str("?int?")
    ).

:- func fmt_int8(univ, list(type_desc)) = doc.

fmt_int8(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.int8_to_doc(X)
    else
        str("?int8?")
    ).

:- func fmt_int16(univ, list(type_desc)) = doc.

fmt_int16(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.int16_to_doc(X)
    else
        str("?int16?")
    ).

:- func fmt_int32(univ, list(type_desc)) = doc.

fmt_int32(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.int32_to_doc(X)
    else
        str("?int32?")
    ).

:- func fmt_int64(univ, list(type_desc)) = doc.

fmt_int64(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.int64_to_doc(X)
    else
        str("?int64?")
    ).

:- func fmt_uint(univ, list(type_desc)) = doc.

fmt_uint(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.uint_to_doc(X)
    else
        str("?uint?")
    ).

:- func fmt_uint8(univ, list(type_desc)) = doc.

fmt_uint8(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.uint8_to_doc(X)
    else
        str("?uint8?")
    ).

:- func fmt_uint16(univ, list(type_desc)) = doc.

fmt_uint16(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.uint16_to_doc(X)
    else
        str("?uint16?")
    ).

:- func fmt_uint32(univ, list(type_desc)) = doc.

fmt_uint32(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.uint32_to_doc(X)
    else
        str("?uint32?")
    ).

:- func fmt_uint64(univ, list(type_desc)) = doc.

fmt_uint64(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.uint64_to_doc(X)
    else
        str("?uint64?")
    ).

:- func fmt_string(univ, list(type_desc)) = doc.

fmt_string(Univ, _ArgDescs) =
    ( if Univ = univ(X) then
        pretty_printer.string_to_doc(X)
    else
        str("?string?")
    ).

:- func fmt_array(univ, list(type_desc)) = doc.

fmt_array(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : array(T))
    then
        pretty_printer.array_to_doc(X)
    else
        str("?array?")
    ).

:- func fmt_list(univ, list(type_desc)) = doc.

fmt_list(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : list(T))
    then
        pretty_printer.list_to_doc(X)
    else
        str("?list?")
    ).

:- func fmt_one_or_more(univ, list(type_desc)) = doc.

fmt_one_or_more(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : one_or_more(T))
    then
        pretty_printer.one_or_more_to_doc(X)
    else
        str("?one_or_more?")
    ).

:- func fmt_tree234(univ, list(type_desc)) = doc.

fmt_tree234(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDescA, ArgDescB],
        has_type(_ArgA : K, ArgDescA),
        has_type(_ArgB : V, ArgDescB),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : tree234(K, V))
    then
        pretty_printer.tree234_to_doc(X)
    else
        str("?tree234?")
    ).

:- func fmt_version_array(univ, list(type_desc)) = doc.

fmt_version_array(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : version_array(T))
    then
        pretty_printer.version_array_to_doc(X)
    else
        str("?version_array?")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This is where we keep the display parameters (line width etc.).
    % The formatter map is handled separately, because it *has* to be
    % initialised immediately, i.e. before any other module's initialisation
    % directive can update the default formatter map.
    %
:- mutable(io_pp_params, pp_params, pp_params(78, 100, triangular(100)),
    ground, [attach_to_io_state, untrailed]).

get_default_params(Params, !IO) :-
    get_io_pp_params(Params, !IO).

set_default_params(Params, !IO) :-
    set_io_pp_params(Params, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

char_to_doc(C) = str(term_io.quoted_char(C)).

string_to_doc(S) = str(term_io.quoted_string(S)).

float_to_doc(F) = str(string.float_to_string(F)).

int_to_doc(I) = str(string.int_to_string(I)).
int8_to_doc(I) = str(string.int8_to_string(I)).
int16_to_doc(I) = str(string.int16_to_string(I)).
int32_to_doc(I) = str(string.int32_to_string(I)).
int64_to_doc(I) = str(string.int64_to_string(I)).

uint_to_doc(U) = str(string.uint_to_string(U)).
uint8_to_doc(U) = str(string.uint8_to_string(U)).
uint16_to_doc(U) = str(string.uint16_to_string(U)).
uint32_to_doc(U) = str(string.uint32_to_string(U)).
uint64_to_doc(U) = str(string.uint64_to_string(U)).

%---------------------------------------------------------------------------%
% XXX The to_doc functions of the compound library types used to put
% different amounts of indentation at the outermost level.
%
% - array_to_doc added one standard indent (two spaces)
% - list_to_doc added one space as indent
% - one_or_more_to_doc added one space as indent
% - tree234_to_doc added one standard indent (two spaces)
% - version_array_to_doc added one standard indent (two spaces)
%
% We now leave any indentation around the doc returned by all these X_to_doc
% functions to their caller.
%---------------------------------------------------------------------------%

array_to_doc(A) =
    docs([str("array(["), array_to_doc_loop(A, 0), str("])")]).

:- func array_to_doc_loop(array(T), int) = doc.

array_to_doc_loop(A, I) = Doc :-
    ( if I > array.max(A) then
        Doc = str("")
    else
        array.unsafe_lookup(A, I, Elem),
        Doc = docs([
            format_arg(format(Elem)),
            ( if I = array.max(A) then
                str("")
            else
                group([str(", "), nl])
            ),
            format_susp((func) = array_to_doc_loop(A, I + 1))
        ])
    ).

%---------------------------------------------------------------------------%

list_to_doc(Xs) = docs([str("["), list_to_doc_loop(Xs), str("]")]).

:- func list_to_doc_loop(list(T)) = doc.

list_to_doc_loop([]) = str("").
list_to_doc_loop([X | Xs]) = Doc :-
    (
        Xs = [],
        Doc = format_arg(format(X))
    ;
        Xs = [_ | _],
        Doc = docs([
            format_arg(format(X)),
            group([str(", "), nl]),
            format_susp((func) = list_to_doc_loop(Xs))
        ])
    ).

%---------------------------------------------------------------------------%

one_or_more_to_doc(one_or_more(H, T)) =
    docs([
        str("one_or_more("),
        format_arg(format(H)),
        group([str(", "), nl]),
        str("["),
        format_susp((func) = list_to_doc_loop(T)),
        str("])")
    ]).

%---------------------------------------------------------------------------%

    % With this type definition, including the use of the -> operator
    % as the function symbol, the default pretty_printer formatting
    % for key_value_pair will generate the output we want.
    %
:- type key_value_pair(K, V)
    --->    (K -> V).

tree234_to_doc(T) =
    docs([
        str("map(["),
        tree234_elements_to_doc(tree234_to_lazy_list(T, tll_nil)),
        str("])")
    ]).

:- func tree234_elements_to_doc(tree234_lazy_list(K, V)) = doc.

tree234_elements_to_doc(tll_nil) = str("").
tree234_elements_to_doc(tll_lazy_cons(K, V, Susp)) = Doc :-
    LL = apply(Susp),
    (
        LL = tll_nil,
        Doc = group([nl, format_arg(format((K -> V)))])
    ;
        LL = tll_lazy_cons(_, _, _),
        Doc = docs([
            group([nl, format_arg(format((K -> V))), str(", ")]),
            format_susp((func) = tree234_elements_to_doc(LL))
        ])
    ).

%---------------------------------------------------------------------------%

version_array_to_doc(A) =
    docs([str("version_array(["), version_array_to_doc_loop(A, 0), str("])")]).

:- func version_array_to_doc_loop(version_array(T), int) = doc.

version_array_to_doc_loop(VA, I) = Doc :-
    ( if I > version_array.max(VA) then
        Doc = str("")
    else
        version_array.lookup(VA, I, Elem),
        Doc = docs([
            format_arg(format(Elem)),
            ( if I = version_array.max(VA) then
                str("")
            else
                group([str(", "), nl])
            ),
            format_susp((func) = version_array_to_doc_loop(VA, I + 1))
        ])
    ).

%---------------------------------------------------------------------------%
:- end_module pretty_printer.
%---------------------------------------------------------------------------%
