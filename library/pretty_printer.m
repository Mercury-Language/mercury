%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2011 The University of Melbourne
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
% the group can fit on the remainder of the current line.  [The algorithm is
% similar to those of Oppen and Wadler, although it uses neither coroutines or
% laziness.]
%
% When a newline is printed, indentation is also output according to the
% current indentation level.
%
% The pretty printer includes special support for formatting Mercury style
% terms in a way that respects Mercury's operator precedence and
% bracketing rules.
%
% The pretty printer takes a parameter specifying a collection of user-defined
% formatting functions for handling certain types rather than using the
% default built-in mechanism.  This allows one to, say, format maps as
% sequences of (key -> value) pairs rather than exposing the underlying
% 234-tree structure.
%
% The amount of output produced is controlled via limit parameters.  Three
% kinds of limits are supported: the output line width, the maximum number of
% lines to be output, and a limit on the depth for formatting arbitrary terms.
% Output is replaced with ellipsis ("...") when limits are exceeded.
%
%---------------------------------------------------------------------------%

:- module pretty_printer.
:- interface.

:- import_module deconstruct.
:- import_module list.
:- import_module io.
:- import_module stream.
:- import_module type_desc.
:- import_module univ.

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

    ;       docs(docs)
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
            % pp_internal docs are used in the implementation and cannot
            % be exploited by user code.

:- type docs == list(doc).

    % This type is private to the implementation. It cannot be exploited by
    % user code.
    %
:- type pp_internal.

    % indent(IndentString, Docs):
    %
    % Append IndentString to the current indentation while printing Docs.
    % Indentation is printed after each newline that is output.
    %
:- func indent(string, docs) = doc.

    % indent(Docs) = indent("  ", Docs).
    %   A convenient abbreviation.
    %
:- func indent(docs) = doc.

    % group(Docs):
    %
    % If Docs can be output on the remainder of the current line by ignoring
    % any nls in Docs, then do so. Otherwise nls in Docs are printed
    % (followed by any indentation). The formatting test is applied recursively
    % for any subgroups in Docs.
    %
:- func group(docs) = doc.

    % format(X) = format_univ(univ(X)):
    % A convenient abbreviation.
    %
:- func format(T) = doc.

    % format_arg(Doc) has the effect of formatting any term in Doc as though
    % it were an argument in a Mercury term by enclosing it in parentheses if
    % necessary.
    %
:- func format_arg(doc) = doc.

    % The pretty-printer limit type, used to control conversion by
    % format_univ, format_list, and format_term.
    %
    % A limit of linear(N) formats the first N functors before truncating
    % output to "...".
    %
    % A limit of triangular(N) formats a term t(X1, ..., Xn) by applying a
    % limit of triangular(N - 1) when formatting X1, triangular(N - 2) when
    % formatting X2, ..., and triangular(N - n) when formatting Xn.
    % The cost of formatting the term t(X1, ..., Xn) as a whole is just one,
    % so a sequence of terms T1, T2, ... is formatted with limits
    % triangular(N), triangular(N - 1), ... respectively.  When the
    % limit is exhausted, terms are output as just "...".
    %
:- type formatting_limit
    --->    linear(int)                 % Print this many functors.
    ;       triangular(int).            % Print first arg with limit N-1,
                                        % second arg with limit N-2, ...

    % The type of generic formatting functions.
    % The first argument is the univ of the value to be formatted.
    % The second argument is the list of argument type_descs for
    % the type of the first argument.
    %
:- type formatter == ( func(univ, list(type_desc)) = doc ).

    % A formatter_map maps types to pps.  Types are identified by module name,
    % type name, and type arity.
    %
:- type formatter_map.

    % Construct a new formatter_map.
    %
:- func new_formatter_map = formatter_map.

    % set_formatter(ModuleName, TypeName, TypeArity, Formatter, FMap):
    %
    % Update FMap to use Formatter to format the type
    % ModuleName.TypeName/TypeArity.
    %
:- func set_formatter(string, string, int, formatter, formatter_map) =
    formatter_map.



    % format(Stream, FMap, LineWidth, MaxLines, Limit, Doc, !State):
    %
    % Format Doc to fit on lines of LineWidth chars, truncating after
    % MaxLines lines, fomatting format_univ(_) docs using specialised
    % formatters Formatters starting with pretty-printer limits Limit.
    %
:- pred write_doc_to_stream(Stream, noncanon_handling, formatter_map, int, int,
    formatting_limit, doc, State, State)
    <= stream.writer(Stream, string, State).
:- mode write_doc_to_stream(in, in(canonicalize), in, in, in, in, in,
    di, uo) is det.
:- mode write_doc_to_stream(in, in(include_details_cc), in, in, in, in, in,
    di, uo) is cc_multi.

    % Convenience predicates.  A user-configurable set of type-specific
    % formatters and formatting parameters are attached to the I/O state.
    % The I/O state-specific format predicate below uses this settings.
    %
:- type pp_params
    --->    pp_params(
                pp_line_width   :: int,             % Line width in characters.
                pp_max_lines    :: int,             % Max lines to output.
                pp_limit        :: formatting_limit % Term formatting limit.
            ).

    % An initial default formatter_map is provided for the most commonly
    % used types in the Mercury standard library (array, char, float,
    % int, map, string, etc.)
    %
    % The default formatter_map may also be updated by users' modules
    % (e.g., in initialisation goals).
    %
    % These defaults are thread local (i.e., changes made by one thread to
    % the default formatter_map will not be visible in another thread).
    %
:- pred get_default_formatter_map(formatter_map::out, io::di, io::uo) is det.
:- pred set_default_formatter_map(formatter_map::in, io::di, io::uo) is det.
:- pred set_default_formatter(string::in, string::in, int::in, formatter::in,
    io::di, io::uo) is det.

    % The initial default pp_params are pp_params(78, 100, triangular(100)).
    % These defaults are thread local (i.e., changes made by one thread to
    % the default pp_params will not be visible in another thread).
    %
:- pred get_default_params(pp_params::out, io::di, io::uo) is det.
:- pred set_default_params(pp_params::in, io::di, io::uo) is det.

    % write_doc(Doc, !IO):
    % write_doc(FileStream, Doc, !IO):
    %
    % Format Doc to io.stdout_stream or FileStream respectively, using
    % write_doc_to stream, with include_details_cc, the default formatter_map,
    % and the default pp_params.
    %
:- pred write_doc(doc::in, io::di, io::uo) is det.
:- pred write_doc(io.output_stream::in, doc::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.                 % For array_to_doc.
:- import_module bool.
:- import_module char.                  % For char_to_doc.
:- import_module deconstruct.
:- import_module float.                 % For float_to_doc.
:- import_module int.
:- import_module map.
:- import_module ops.
:- import_module string.
:- import_module term_io.
:- import_module tree234.               % For tree234_to_doc.
:- import_module version_array.         % For version_array_to_doc.

%---------------------------------------------------------------------------%

:- type formatter_map == map(string, map(string, map(int, formatter))).

:- type indents == list(string).

:- type pp_internal
    --->    open_group
            % Mark the start of a group.

    ;       close_group
            % Mark the end of a group.

    ;       indent(string)
            % Extend the current indentation.

    ;       outdent
            % Restore indentation to before the last indent/1.

    ;       set_op_priority(ops.priority)
            % Set the current priority for printing operator terms with the
            % correct parenthesisation.

    ;       set_limit(formatting_limit).
            % Set the truncation limit.

%---------------------------------------------------------------------------%

new_formatter_map = map.init.

%---------------------------------------------------------------------------%

set_formatter(ModuleName, TypeName, Arity, Formatter, FMap0) = FMap :-
    ( if FMap0 ^ elem(ModuleName) = FMap0_Type_Arity then
        ( if FMap0_Type_Arity ^ elem(TypeName) = FMap0_Arity then
            FMap_Arity = FMap0_Arity ^ elem(Arity) := Formatter
          else
            FMap_Arity = map.init ^ elem(Arity) := Formatter
        ),
        FMap_Type_Arity = FMap0_Type_Arity ^ elem(TypeName) := FMap_Arity,
        FMap = FMap0 ^ elem(ModuleName) := FMap_Type_Arity
      else
        FMap_Arity = map.init ^ elem(Arity) := Formatter,
        FMap_Type_Arity = map.init ^ elem(TypeName) := FMap_Arity,
        FMap = FMap0 ^ elem(ModuleName) := FMap_Type_Arity
    ).

%---------------------------------------------------------------------------%

:- pred get_formatter(formatter_map::in, string::in, string::in, int::in,
    formatter::out) is semidet.

get_formatter(FMap, ModuleName, TypeName, Arity, Formatter) :-
    Formatter = FMap ^ elem(ModuleName) ^ elem(TypeName) ^ elem(Arity).

%---------------------------------------------------------------------------%

indent(Indent, Docs) =
    docs([pp_internal(indent(Indent)), docs(Docs), pp_internal(outdent)]).

%---------------------------------------------------------------------------%

indent(Docs) =
    indent("  ", Docs).

%---------------------------------------------------------------------------%

group(Docs) =
    docs([pp_internal(open_group), docs(Docs), pp_internal(close_group)]).

%---------------------------------------------------------------------------%

format(X) = format_univ(univ(X)).

%---------------------------------------------------------------------------%

format_arg(Doc) =
    docs([
        pp_internal(
            set_op_priority(ops.arg_priority(ops.init_mercury_op_table))),
        Doc
    ]).

%---------------------------------------------------------------------------%

write_doc_to_stream(Stream, Canonicalize, FMap, LineWidth, MaxLines, Limit,
        Doc, !IO) :-
    Pri = ops.max_priority(ops.init_mercury_op_table),
    RemainingWidth = LineWidth,
    Indents = [],
    write_doc_to_stream(Stream, Canonicalize, FMap, LineWidth, [Doc],
        RemainingWidth, _, Indents, _, MaxLines, _, Limit, _, Pri, _, !IO).

%---------------------------------------------------------------------------%

    % format(FMap, LineWidth, Docs, !RemainingWidth, !Indents,
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
:- pred write_doc_to_stream(Stream, noncanon_handling, formatter_map, int,
    docs, int, int, indents, indents, int, int,
    formatting_limit, formatting_limit,
    ops.priority, ops.priority, State, State)
    <= stream.writer(Stream, string, State).
:- mode write_doc_to_stream(in, in(canonicalize), in, in, in,
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode write_doc_to_stream(in, in(include_details_cc), in, in, in,
    in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

write_doc_to_stream(_Stream, _Canonicalize, _FMap, _LineWidth, [],
        !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO).
write_doc_to_stream(Stream, Canonicalize, FMap, LineWidth, [Doc | Docs0],
        !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO) :-
    ( if !.RemainingLines =< 0 then
        stream.put(Stream, "...", !IO)
      else
        (
            % Output strings directly.
            %
            Doc = str(String),
            stream.put(Stream, String, !IO),
            !:RemainingWidth = !.RemainingWidth -
                string.count_codepoints(String),
            Docs = Docs0
        ;
            Doc = nl,
            ( if
                F = ( func(S, W) = string.count_codepoints(S) + W ),
                IndentWidth = list.foldl(F, !.Indents, 0),
                !.RemainingWidth < LineWidth - IndentWidth
              then
                format_nl(Stream, LineWidth, !.Indents, !:RemainingWidth,
                    !RemainingLines, !IO)
              else
                true
            ),
            Docs = Docs0
        ;
            Doc = hard_nl,
            format_nl(Stream, LineWidth, !.Indents, !:RemainingWidth,
                !RemainingLines, !IO),
            Docs = Docs0
        ;
            Doc = docs(Docs1),
            Docs = list.(Docs1 ++ Docs0)
        ;
            Doc = format_univ(Univ),
            expand_pp(Canonicalize, FMap, Univ, Doc1, !Limit, !.Pri),
            Docs = [Doc1 | Docs0]
        ;
            Doc = format_list(Univs, Sep),
            expand_format_list(Univs, Sep, Doc1, !Limit),
            Docs = [Doc1 | Docs0]
        ;
            Doc = format_term(Name, Univs),
            expand_format_term(Name, Univs, Doc1, !Limit, !.Pri),
            Docs = [Doc1 | Docs0]
        ;
            Doc = format_susp(Susp),
            expand_format_susp(Susp, Doc1, !Limit),
            Docs = [Doc1 | Docs0]
        ;
            % Indents.
            %
            Doc = pp_internal(indent(Indent)),
            !:Indents = [Indent | !.Indents],
            Docs = Docs0
        ;
            % Outdents.
            %
            Doc = pp_internal(outdent),
            !:Indents = list.det_tail(!.Indents),
            Docs = Docs0
        ;
            % Open groups: if the current group (and what follows up to the
            % next nl) fits on the remainder of the current line then print
            % it that way; otherwise we have to recognise the nls in the
            % group.
            %
            Doc = pp_internal(open_group),
            OpenGroups = 1,
            CurrentRemainingWidth = !.RemainingWidth,
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups,
                !Limit, !Pri, CurrentRemainingWidth, RemainingWidthAfterGroup),
            ( if RemainingWidthAfterGroup >= 0 then
                output_current_group(Stream, LineWidth, !.Indents, OpenGroups,
                    Docs1, Docs, !RemainingWidth, !RemainingLines, !IO)
              else
                Docs = Docs1
            )
        ;
            % Close groups.
            %
            Doc = pp_internal(close_group),
            Docs = Docs0
        ;
            Doc = pp_internal(set_limit(Lim)),
            !:Limit = Lim,
            Docs = Docs0
        ;
            Doc = pp_internal(set_op_priority(NewPri)),
            !:Pri = NewPri,
            Docs = Docs0
        ),
        write_doc_to_stream(Stream, Canonicalize, FMap, LineWidth, Docs,
            !RemainingWidth, !Indents, !RemainingLines, !Limit, !Pri, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_current_group(Stream::in, int::in, indents::in, int::in,
    docs::in, docs::out, int::in, int::out, int::in, int::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

output_current_group(_Stream, _LineWidth, _Indents, _OpenGroups,
        [], [], !RemainingWidth, !RemainingLines, !IO).
output_current_group(Stream, LineWidth, Indents, OpenGroups,
        [Doc | Docs0], Docs, !RemainingWidth, !RemainingLines, !IO) :-
    ( if Doc = str(String) then
        stream.put(Stream, String, !IO),
        !:RemainingWidth = !.RemainingWidth - string.count_codepoints(String),
        output_current_group(Stream, LineWidth, Indents, OpenGroups,
            Docs0, Docs, !RemainingWidth, !RemainingLines, !IO)
      else if Doc = hard_nl then
        format_nl(Stream, LineWidth, Indents, !:RemainingWidth,
            !RemainingLines, !IO),
        ( if !.RemainingLines =< 0 then
            Docs = Docs0
          else
            output_current_group(Stream, LineWidth, Indents, OpenGroups,
                Docs0, Docs, !RemainingWidth, !RemainingLines, !IO)
        )
      else if Doc = pp_internal(open_group) then
        output_current_group(Stream, LineWidth, Indents, OpenGroups + 1,
            Docs0, Docs, !RemainingWidth, !RemainingLines, !IO)
      else if Doc = pp_internal(close_group) then
        ( if OpenGroups = 1 then
            Docs = Docs0
          else
            output_current_group(Stream, LineWidth, Indents, OpenGroups - 1,
                Docs0, Docs, !RemainingWidth, !RemainingLines, !IO)
        )
      else
        output_current_group(Stream, LineWidth, Indents, OpenGroups,
            Docs0, Docs, !RemainingWidth, !RemainingLines, !IO)
    ).

%---------------------------------------------------------------------------%

    % expand_docs(Canonicalize, Docs0, Docs, G, !L, !P, !R) expands out any
    % doc(_), pp_univ(_), format_list(_, _), and pp_term(_) constructors in
    % Docs0 into Docs, until either Docs0 has been completely expanded, or a nl
    % is encountered, or the remaining space on the current line has been
    % accounted for.
    % G is used to track nested groups.
    % !L tracks the limits after accounting for expansion.
    % !L tracks the operator priority after accounting for expansion.
    % !R tracks the remaining line width after accounting for expansion.
    %
:- pred expand_docs(noncanon_handling, formatter_map, docs, docs, int,
    formatting_limit, formatting_limit, ops.priority, ops.priority,
    int, int) is cc_multi.
:- mode expand_docs(in(canonicalize), in, in, out, in, in, out,
    in, out, in, out) is det.
:- mode expand_docs(in(include_details_cc), in, in, out, in, in, out,
    in, out, in, out) is cc_multi.

expand_docs(_Canonicalize, _FMap, [], [], _OpenGroups,
        !Limit, !Pri, !RemainingWidth).
expand_docs(Canonicalize, FMap, [Doc | Docs0], Docs, OpenGroups,
        !Limit, !Pri, !RemainingWidth) :-
    ( if
        (
            OpenGroups =< 0, ( Doc = nl ; Doc = hard_nl )
            % We have found the first nl after the close of the current
            % open group.
        ;
            !.RemainingWidth < 0
            % We have run out of space on this line: the current open
            % group will not fit.
        )
      then
        Docs = [Doc | Docs0]
      else
        (
            Doc = str(String),
            !:RemainingWidth = !.RemainingWidth -
                string.count_codepoints(String),
            Docs = [Doc | Docs1],
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            ( Doc = nl ; Doc = hard_nl ),
            ( if OpenGroups =< 0 then
                Docs = [Doc | Docs0]
              else
                Docs = [Doc | Docs1],
                expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups,
                    !Limit, !Pri, !RemainingWidth)
            )
        ;
            Doc = docs(Docs1),
            expand_docs(Canonicalize, FMap, list.(Docs1 ++ Docs0), Docs,
                OpenGroups, !Limit, !Pri, !RemainingWidth)
        ;
            Doc = format_univ(Univ),
            expand_pp(Canonicalize, FMap, Univ, Doc1, !Limit, !.Pri),
            expand_docs(Canonicalize, FMap, [Doc1 | Docs0], Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = format_list(Univs, Sep),
            expand_format_list(Univs, Sep, Doc1, !Limit),
            expand_docs(Canonicalize, FMap, [Doc1 | Docs0], Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = format_term(Name, Univs),
            expand_format_term(Name, Univs, Doc1, !Limit, !.Pri),
            expand_docs(Canonicalize, FMap, [Doc1 | Docs0], Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = format_susp(Susp),
            expand_format_susp(Susp, Doc1, !Limit),
            expand_docs(Canonicalize, FMap, [Doc1 | Docs0], Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(indent(_)),
            Docs = [Doc | Docs1],
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(outdent),
            Docs = [Doc | Docs1],
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(open_group),
            Docs = [Doc | Docs1],
            OpenGroups1 = OpenGroups + ( if OpenGroups > 0 then 1 else 0 ),
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups1,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(close_group),
            Docs = [Doc | Docs1],
            OpenGroups1 = OpenGroups - ( if OpenGroups > 0 then 1 else 0 ),
            expand_docs(Canonicalize, FMap, Docs0, Docs1, OpenGroups1,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(set_limit(Lim)),
            !:Limit = Lim,
            expand_docs(Canonicalize, FMap, Docs0, Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        ;
            Doc = pp_internal(set_op_priority(NewPri)),
            !:Pri = NewPri,
            expand_docs(Canonicalize, FMap, Docs0, Docs, OpenGroups,
                !Limit, !Pri, !RemainingWidth)
        )
    ).

%---------------------------------------------------------------------------%

    % Output a newline followed by indentation.
    %
:- pred format_nl(Stream::in, int::in, indents::in, int::out,
    int::in, int::out, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

format_nl(Stream, LineWidth, Indents, RemainingWidth, !RemainingLines, !IO) :-
    stream.put(Stream, "\n", !IO),
    output_indentation(Stream, Indents, LineWidth, RemainingWidth, !IO),
    !:RemainingLines = !.RemainingLines - 1.


:- pred output_indentation(Stream::in, indents::in, int::in, int::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

output_indentation(_Stream, [], !RemainingWidth, !IO).
output_indentation(Stream, [Indent | Indents], !RemainingWidth, !IO) :-
    output_indentation(Stream, Indents, !RemainingWidth, !IO),
    stream.put(Stream, Indent, !IO),
    !:RemainingWidth = !.RemainingWidth - string.count_codepoints(Indent).

%---------------------------------------------------------------------------%

    % Expand a univ into docs using the first pretty-printer in the given list
    % that succeeds, otherwise use the generic pretty- printer. If the
    % pretty-printer limit has been exhausted, then only "..." is produced.
    %
:- pred expand_pp(noncanon_handling, formatter_map, univ, doc,
    formatting_limit, formatting_limit, ops.priority).
:- mode expand_pp(in(canonicalize), in, in, out, in, out, in)
    is det.
:- mode expand_pp(in(include_details_cc), in, in, out, in, out, in)
    is cc_multi.

expand_pp(Canonicalize, FMap, Univ, Doc, !Limit, CurrentPri) :-
    ( if
        limit_overrun(!.Limit)
      then
        Doc = ellipsis
      else if
        Value = univ_value(Univ),
        type_ctor_and_args(type_of(Value), TypeCtorDesc, ArgTypeDescs),
        ModuleName = type_ctor_module_name(TypeCtorDesc),
        TypeName = type_ctor_name(TypeCtorDesc),
        Arity = list.length(ArgTypeDescs),
        get_formatter(FMap, ModuleName, TypeName, Arity, Formatter)
      then
        decrement_limit(!Limit),
        Doc = set_formatting_limit_correctly(!.Limit,
            Formatter(Univ, ArgTypeDescs))
      else
        deconstruct(univ_value(Univ), Canonicalize, Name, _Arity, Args),
        expand_format_term(Name, Args, Doc, !Limit, CurrentPri)
    ).

%---------------------------------------------------------------------------%

    % Expand a list of univs into docs using the given separator.
    %
:- pred expand_format_list(list(univ)::in, doc::in, doc::out,
    formatting_limit::in, formatting_limit::out) is det.

expand_format_list([], _Sep, docs([]), !Limit).

expand_format_list([Univ | Univs], Sep, Doc, !Limit) :-
    ( if limit_overrun(!.Limit) then
        Doc = ellipsis
      else
        (
            Univs = [],
            Doc = format_arg(group([nl, format_univ(Univ)]))
        ;
            Univs = [_ | _],
            Doc = docs([
                format_arg(group([nl, format_univ(Univ), Sep])),
                format_list(Univs, Sep)
            ])
        )
    ).

%---------------------------------------------------------------------------%

    % Expand a name and list of univs into docs corresponding to Mercury
    % term syntax.
    %
:- pred expand_format_term(string::in, list(univ)::in, doc::out,
        formatting_limit::in, formatting_limit::out, ops.priority::in)
        is det.

expand_format_term(Name, Args, Doc, !Limit, CurrentPri) :-
    ( if Args = [] then
        Doc0 = str(term_io.quoted_atom(Name))
      else if limit_overrun(!.Limit) then
        Doc0 = ellipsis
      else if expand_format_op(Name, Args, CurrentPri, OpDoc) then
        Doc0 = OpDoc
      else if Name = "{}" then
        Doc0 = docs([
            str("{"), indent([format_list(Args, str(", "))]), str("}")
        ])
      else
        Doc0 = group([
            nl,
            str(term_io.quoted_atom(Name)),
            str("("), indent([format_list(Args, str(", "))]), str(")")
        ])
    ),
    decrement_limit(!Limit),
    Doc = set_formatting_limit_correctly(!.Limit, Doc0).

%---------------------------------------------------------------------------%

:- pred expand_format_susp(((func) = doc)::in, doc::out,
    formatting_limit::in, formatting_limit::out) is det.

expand_format_susp(Susp, Doc, !Limit) :-
    ( if limit_overrun(!.Limit) then
        Doc = ellipsis
      else
        decrement_limit(!Limit),
        Doc = set_formatting_limit_correctly(!.Limit, apply(Susp))
    ).

%---------------------------------------------------------------------------%

    % Expand a name and list of univs into docs corresponding to Mercury
    % operator syntax.
    %
:- pred expand_format_op(string::in, list(univ)::in, ops.priority::in,
    doc::out) is semidet.

expand_format_op(Op, [Arg], CurrentPri, Docs) :-
    ( if ops.lookup_prefix_op(ops.init_mercury_op_table, Op, OpPri, Assoc) then
        Doc =
            group([
                str(Op),
                pp_internal(set_op_priority(adjust_priority(OpPri, Assoc))),
                format_univ(Arg)
            ]),
        Docs = add_parens_if_needed(OpPri, CurrentPri, Doc)
      else
        ops.lookup_postfix_op(ops.init_mercury_op_table, Op, OpPri, Assoc),
        Doc =
            group([
                pp_internal(set_op_priority(adjust_priority(OpPri, Assoc))),
                format_univ(Arg),
                str(Op)
            ]),
        Docs = add_parens_if_needed(OpPri, CurrentPri, Doc)
    ).

expand_format_op(Op, [ArgA, ArgB], CurrentPri, Docs) :-
    ( if
        ops.lookup_infix_op(ops.init_mercury_op_table, Op, OpPri, AssocA,
            AssocB)
      then
        Doc =
            group([
                pp_internal(set_op_priority(adjust_priority(OpPri, AssocA))),
                format_univ(ArgA),
                ( if Op = "." then
                    str(Op)
                  else
                    docs([str(" "), str(Op), str(" ")])
                ),
                indent([
                    nl,
                    pp_internal(set_op_priority(adjust_priority(OpPri,
                        AssocB))),
                    format_univ(ArgB)
                ])
            ]),
        Docs = add_parens_if_needed(OpPri, CurrentPri, Doc)
      else
        ops.lookup_binary_prefix_op(ops.init_mercury_op_table, Op, OpPri,
            AssocA, AssocB),
        Doc =
            group([
                str(Op), str(" "),
                pp_internal(set_op_priority(adjust_priority(OpPri, AssocA))),
                format_univ(ArgA),
                str(" "),
                indent([
                    pp_internal(set_op_priority(adjust_priority(OpPri,
                        AssocB))),
                    format_univ(ArgB)
                ])
            ]),
        Docs = add_parens_if_needed(OpPri, CurrentPri, Doc)
    ).

%---------------------------------------------------------------------------%

    % Update the limits properly after processing a pp_term.
    %
:- func set_formatting_limit_correctly(formatting_limit, doc) = doc.

set_formatting_limit_correctly(linear(_), Doc) =
    Doc.
set_formatting_limit_correctly(Limit @ triangular(_), Doc0) =
    docs([Doc0, pp_internal(set_limit(Limit))]).

%---------------------------------------------------------------------------%

    % Add parentheses around a doc if required by operator priority.
    %
:- func add_parens_if_needed(ops.priority, ops.priority, doc) = doc.

add_parens_if_needed(OpPriority, EnclosingPriority, Doc) =
    ( if OpPriority > EnclosingPriority then
        docs([str("("), Doc, str(")")])
      else
        Doc
    ).

%---------------------------------------------------------------------------%

:- func adjust_priority(ops.priority, ops.assoc) = ops.priority.

adjust_priority(Priority, Assoc) = AdjustedPriority :-
    ops.adjust_priority_for_assoc(Priority, Assoc, AdjustedPriority).

%---------------------------------------------------------------------------%

    % Succeeds if the pretty-printer state limits have been used up.
    %
:- pred limit_overrun(formatting_limit::in) is semidet.

limit_overrun(linear(N)) :-
    N =< 0.
limit_overrun(triangular(N)) :-
    N =< 0.

%---------------------------------------------------------------------------%

    % Reduce the pretty-printer limit by one.
    %
:- pred decrement_limit(formatting_limit::in, formatting_limit::out) is det.

decrement_limit(linear(N), linear(N - 1)).
decrement_limit(triangular(N), triangular(N - 1)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Convenience predicates.
%

    % This is where we keep the display parameters (line width etc.).
    % The formatter map is handled separately because it *has* to
    % be initialised immediately (i.e., before any other module's
    % initialisation directive can update the default formatter map).
    %
:- mutable(io_pp_params, pp_params, pp_params(78, 100, triangular(100)),
    ground, [attach_to_io_state, untrailed]).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pred pretty_printer_is_initialised(bool::out, io::di, io::uo)
        is det.

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

:- pragma foreign_proc("Erlang",
        pretty_printer_is_initialised(Okay::out, _IO0::di, _IO::uo),
        [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    'ML_erlang_global_server' !
        {get_mutable, pretty_printer_is_initialised, self()},
    receive
        {get_mutable_ack, undefined} -> Okay = {no};
        {get_mutable_ack, Okay} -> void
    end
").

%---------------------------------------------------------------------------%

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

:- pragma foreign_proc("Erlang",
        unsafe_get_default_formatter_map(FMap::out, _IO0::di, _IO::uo),
        [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    'ML_erlang_global_server' !
        {get_mutable, pretty_printer_default_formatter_map, self()},
    receive
        {get_mutable_ack, FMap} -> FMap
    end
").

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

:- pragma foreign_proc("Erlang",
        set_default_formatter_map(FMap::in, _IO0::di, _IO::uo),
        [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    'ML_erlang_global_server' !
        {set_mutable, pretty_printer_is_initialised, {yes}},
    'ML_erlang_global_server' !
        {set_mutable, pretty_printer_default_formatter_map, FMap}
").

%---------------------------------------------------------------------------%

set_default_formatter(ModuleName, TypeName, Arity, Formatter, !IO) :-
    get_default_formatter_map(FMap0, !IO),
    FMap = set_formatter(ModuleName, TypeName, Arity, Formatter, FMap0),
    set_default_formatter_map(FMap, !IO).

%---------------------------------------------------------------------------%

get_default_params(Params, !IO) :-
    get_io_pp_params(Params, !IO).

set_default_params(Params, !IO) :-
    set_io_pp_params(Params, !IO).

%---------------------------------------------------------------------------%

write_doc(Doc, !IO) :-
    write_doc(io.stdout_stream, Doc, !IO).

write_doc(Stream, Doc, !IO) :-
    get_default_formatter_map(Formatters, !IO),
    get_default_params(pp_params(LineWidth, MaxLines, Limit), !IO),
    promise_equivalent_solutions [!:IO] (
        write_doc_to_stream(Stream, include_details_cc, Formatters,
            LineWidth, MaxLines, Limit, Doc, !IO)
    ).

%---------------------------------------------------------------------------%

    % Construct the initial default formatter map. This function
    % should be extended as more specialised formatters are added
    % to the standard library modules.
    %
:- func initial_formatter_map = formatter_map.

initial_formatter_map = !:Formatters :-
    !:Formatters = new_formatter_map,
    set_formatter_sv("builtin", "character", 0, fmt_char,    !Formatters),
    set_formatter_sv("builtin", "float",     0, fmt_float,   !Formatters),
    set_formatter_sv("builtin", "int",       0, fmt_int,     !Formatters),
    set_formatter_sv("builtin", "string",    0, fmt_string,  !Formatters),
    set_formatter_sv("array",   "array",     1, fmt_array,   !Formatters),
    set_formatter_sv("list",    "list",      1, fmt_list,    !Formatters),
    set_formatter_sv("tree234", "tree234",   2, fmt_tree234, !Formatters),
    set_formatter_sv("version_array", "version_array", 1, fmt_version_array,
        !Formatters).

%---------------------------------------------------------------------------%

:- pred set_formatter_sv(string::in, string::in, int::in, formatter::in,
    formatter_map::in, formatter_map::out) is det.

set_formatter_sv(ModuleName, TypeName, Arity, Formatter, FMap0, FMap) :-
    FMap = set_formatter(ModuleName, TypeName, Arity, Formatter, FMap0).

%---------------------------------------------------------------------------%

:- func fmt_char(univ, list(type_desc)) = doc.

fmt_char(Univ, _ArgDescs) =
    ( if Univ = univ(X) then char_to_doc(X) else str("?char?") ).

%---------------------------------------------------------------------------%

:- func fmt_float(univ, list(type_desc)) = doc.

fmt_float(Univ, _ArgDescs) =
    ( if Univ = univ(X) then float_to_doc(X) else str("?float?") ).

%---------------------------------------------------------------------------%

:- func fmt_int(univ, list(type_desc)) = doc.

fmt_int(Univ, _ArgDescs) =
    ( if Univ = univ(X) then int_to_doc(X) else str("?int?") ).

%---------------------------------------------------------------------------%

:- func fmt_string(univ, list(type_desc)) = doc.

fmt_string(Univ, _ArgDescs) =
    ( if Univ = univ(X) then string_to_doc(X) else str("?string?") ).

%---------------------------------------------------------------------------%

:- func fmt_array(univ, list(type_desc)) = doc.

fmt_array(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : array(T))
      then
        array_to_doc(X)
      else
        str("?array?")
    ).

%---------------------------------------------------------------------------%

:- func fmt_version_array(univ, list(type_desc)) = doc.

fmt_version_array(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : version_array(T))
      then
        version_array_to_doc(X)
      else
        str("?version_array?")
    ).

%---------------------------------------------------------------------------%

:- func fmt_list(univ, list(type_desc)) = doc.

fmt_list(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDesc],
        has_type(_Arg : T, ArgDesc),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : list(T))
      then
        list_to_doc(X)
      else
        str("?list?")
    ).

%---------------------------------------------------------------------------%

:- func fmt_tree234(univ, list(type_desc)) = doc.

fmt_tree234(Univ, ArgDescs) =
    ( if
        ArgDescs = [ArgDescA, ArgDescB],
        has_type(_ArgA : K, ArgDescA),
        has_type(_ArgB : V, ArgDescB),
        Value = univ_value(Univ),
        dynamic_cast(Value, X : tree234(K, V))
      then
        tree234_to_doc(X)
      else
        str("?tree234?")
    ).

%---------------------------------------------------------------------------%

:- func ellipsis = doc.

ellipsis = str("...").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
