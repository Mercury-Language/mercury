%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: print_help.m.
% Main author: zs.

:- module libs.print_help.
:- interface.

:- import_module io.

:- type print_what_help
    --->    print_public_help
    ;       print_public_and_private_help.

:- pred options_help_new(io.text_output_stream::in, print_what_help::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.optdb_help.
:- import_module libs.options.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

options_help_new(Stream, What, !IO) :-
     OptdbPred =
        ( pred(OptdbRecord::out) is multi :-
            optdb(Cat, Opt, OptData, Help),
            OptdbRecord = optdb_record(Cat, Opt, OptData, Help)
        ),
    solutions(OptdbPred, OptdbRecords),
    list.foldl(acc_help_message(What), OptdbRecords, cord.init, LineCord),
    write_lines(Stream, cord.list(LineCord), !IO).

:- pred write_lines(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

write_lines(_, [], !IO).
write_lines(Stream, [Line | Lines], !IO) :-
    io.write_string(Stream, Line, !IO),
    io.nl(Stream, !IO),
    write_lines(Stream, Lines, !IO).

%---------------------------------------------------------------------------%

:- type optdb_record
    --->    optdb_record(
                option_category,
                option,
                option_data,
                libs.optdb_help.help
            ).

:- type maybe_expect_arg
    --->    do_not_expect_arg
    ;       expect_arg.

:- type maybe_negate
    --->    do_not_negate
    ;       negate.

:- type maybe_add_negative
    --->    no_negative_version
    ;       add_negative_version.

:- type option_params
    --->    option_params(
                op_expect       :: maybe_expect_arg,
                op_negate       :: maybe_negate,
                op_add_negative :: maybe_add_negative
            ).

%---------------------------------------------------------------------------%

:- pred get_optdb_record_params(optdb_record::in, option_params::out) is det.

get_optdb_record_params(OptdbRecord, Params) :-
    OptdbRecord = optdb_record(_Cat, _Option, OptionData, _Help),
    (
        OptionData = bool(Bool),
        MaybeExpectArg = do_not_expect_arg,
        ( Bool = no,  MaybeNegate = do_not_negate
        ; Bool = yes, MaybeNegate = negate
        ),
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = bool_special,
        MaybeExpectArg = do_not_expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        ( OptionData = int(_)
        ; OptionData = string(_)
        ; OptionData = int_special
        ; OptionData = string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = special,
        MaybeExpectArg = do_not_expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = file_special,
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        ( OptionData = accumulating(_)
        ; OptionData = maybe_int(_)
        ; OptionData = maybe_string(_)
        ; OptionData = maybe_string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = add_negative_version
    ),
    Params = option_params(MaybeExpectArg, MaybeNegate, MaybeAddNegVersion).

%---------------------------------------------------------------------------%

:- pred acc_help_message(print_what_help::in, optdb_record::in,
    cord(string)::in, cord(string)::out) is det.

acc_help_message(What, OptdbRecord, !EffectiveLinesCord) :-
    get_optdb_record_params(OptdbRecord, Params),
    % XXX We could automatically add "(This option is not for general use.)"
    % to the start of the description of every private option, to save
    % the repetition of including it in help_private structures.
    %
    % We currently handle this message quite badly. First, we do not include it
    % in many help_private structures (which, to be fair, won't matter
    % until we implement callers that specify print_public_and_private_help.
    % Second, we *do* include it in a few help_public structures, in which
    % cases it gives non-developer readers useless information. To make
    % the message useful, the message would have to say *in what situations*
    % the option may be relevant to non-developers.
    OptdbRecord = optdb_record(_Cat, Option, OptionData, Help),
    some [!LineCord]
    (
        !:LineCord = cord.init,
        (
            Help = no_help,
            PublicOrPrivate = help_private,
            DescLines = []
        ;
            Help = xunnamed_help(DescLines),
            % XXX It is quite likely that many options that do not have entries
            % in the long_table predicate, which therefore should be in optdb
            % with unnamed_help, are there with some other help structure,
            % such as priv_help.
            PublicOrPrivate = help_private,
            string.format("%sUNNAMED OPTION %s",
                [s(option_name_indent), s(string(Option))], NameLine),
            cord.snoc(NameLine, !LineCord)
        ;
            Help = xgen_help(ShortNames, LongName, AltLongNames,
                PublicOrPrivate, DescLines),
            acc_short_option_names(Params, Option, no_arg, no_align,
                ShortNames, !LineCord),
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xhelp(LongName, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xarg_help(LongName, ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_help(LongName, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_arg_help(LongName, ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord)
        ;
            (
                Help = xalt_help(LongName, AltLongNames, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xalt_arg_help(LongName, AltLongNames, ArgName,
                    DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_alt_help(LongName, AltLongNames, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_alt_arg_help(LongName, AltLongNames, ArgName,
                    DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xshort_help(ShortName, LongName, AltLongNames,
                    DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xshort_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_short_help(ShortName, LongName, AltLongNames,
                    DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_short_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_short_option_name(Params, Option, MaybeArg, no_align,
                ShortName, !LineCord),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xalt_align_help(LongName, AltLongNames,
                    AlignedText, DescLines),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_alt_align_help(LongName, AltLongNames,
                    AlignedText, DescLines),
                PublicOrPrivate = help_private
            ),
            MaybeArg = no_arg,
            Align = aligned_text(AlignedText),
            acc_long_option_name(Params, Option, MaybeArg, Align,
                LongName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = xshort_alt_align_help(ShortName, LongName, AltLongNames,
                AlignedText, DescLines),
            PublicOrPrivate = help_public,
            acc_short_option_name(Params, Option, no_arg,
                aligned_text(AlignedText), ShortName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = xno_align_help(LongName, AlignedText, NoAlignedText,
                DescLines),
            PublicOrPrivate = help_public,
            expect(is_bool(OptionData), $pred,
                "unexpected use of xno_align_help"),
            ParamsNN = Params ^ op_negate := do_not_negate,
            FirstLine0 = long_option_name_line(ParamsNN, Option, no_arg,
                LongName),
            SecondLine0 = long_negated_option_name_line(LongName),
            % In this case, we add *different* aligned text to each line.
            add_aligned_text(AlignedText, FirstLine0, FirstLine),
            add_aligned_text(NoAlignedText, SecondLine0, SecondLine),
            cord.snoc(FirstLine, !LineCord),
            cord.snoc(SecondLine, !LineCord)
        ;
            Help = xalt_arg_align_help(LongName, ArgAligns, DescLines),
            PublicOrPrivate = help_public,
            % In this case, we add *different* aligned text to each line.
            list.foldl(acc_arg_align_text(Params, Option, LongName),
                ArgAligns, !LineCord)
        ),
        ( if
            (
                PublicOrPrivate = help_public
            ;
                PublicOrPrivate = help_private,
                What = print_public_and_private_help
            )
        then
            ( if
                cord.is_empty(!.LineCord),
                DescLines = []
            then
                true
            else
                DescPrefix = option_desc_indent,
                (
                    DescLines = [],
                    acc_prefixed_line(DescPrefix,
                        "There is no help text available.", !LineCord)
                ;
                    DescLines = [_ | _],
                    list.foldl(acc_prefixed_line(DescPrefix), DescLines,
                        !LineCord)
                ),
                BlankLineCord = cord.singleton(""),
                (
                    PublicOrPrivate = help_public,
                    PrivatePrefixCord = cord.init
                ;
                    PublicOrPrivate = help_private,
                    PrivatePrefixCord =
                        cord.singleton(option_name_indent ++ "PRIVATE OPTION")
                ),
                !:EffectiveLinesCord = !.EffectiveLinesCord ++
                    BlankLineCord ++ PrivatePrefixCord ++ !.LineCord
            )
        else
            true
        )
    ).

:- pred acc_arg_align_text(option_params::in, option::in, string::in,
    arg_align::in, cord(string)::in, cord(string)::out) is det.

acc_arg_align_text(Params, Option, LongName, ArgAlign, !LineCord) :-
    Params = option_params(MaybeExpectArg, MaybeNegate, MaybeAddNegVersion),
    expect(unify(MaybeExpectArg, expect_arg), $pred,
        "unexpected MaybeExpectArg"),
    expect(unify(MaybeNegate, do_not_negate), $pred,
        "unexpected MaybeNegate"),
    expect(unify(MaybeAddNegVersion, no_negative_version), $pred,
        "unexpected MaybeAddNegVersion"),

    ArgAlign = arg_align(ArgName, AlignedText),
    Line0 = long_option_name_line(Params, Option, arg_name(ArgName), LongName),
    add_aligned_text(AlignedText, Line0, Line),
    cord.snoc(Line, !LineCord).

:- pred is_bool(option_data::in) is semidet.

is_bool(bool(_)).

%---------------------------------------------------------------------------%

:- type maybe_arg_name
    --->    no_arg
    ;       arg_name(string).

:- type maybe_aligned_text
    --->    no_align
    ;       aligned_text(string).

% The next two predicates next are needed because folds over
% acc_{long,short}_option_name do not preserve the inst of the
% maybe_aligned_text argument.

:- pred acc_long_option_names(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(string), cord(string), cord(string)).
:- mode acc_long_option_names(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_long_option_names(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_long_option_names(_, _, _, _, [], !LineCord).
acc_long_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        [LongName | LongNames], !LineCord) :-
    acc_long_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !LineCord),
    acc_long_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        LongNames, !LineCord).

:- pred acc_short_option_names(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(char), cord(string), cord(string)).
:- mode acc_short_option_names(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_short_option_names(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_short_option_names(_, _, _, _, [], !LineCord).
acc_short_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        [ShortName | ShortNames], !LineCord) :-
    acc_short_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !LineCord),
    acc_short_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortNames, !LineCord).

%---------------------------------------------------------------------------%

:- pred acc_long_option_name(option_params, option, maybe_arg_name,
    maybe_aligned_text, string, cord(string), cord(string)).
:- mode acc_long_option_name(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_long_option_name(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_long_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !LineCord) :-
    FirstLine0 = long_option_name_line(Params, Option, MaybeArgName, LongName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate, MaybeAddNegVersion),
    (
        MaybeAddNegVersion = no_negative_version
    ;
        MaybeAddNegVersion = add_negative_version,
        SecondLine = long_negated_option_name_line(LongName),
        cord.snoc(SecondLine, !LineCord)
    ).

:- pred acc_short_option_name(option_params, option, maybe_arg_name,
    maybe_aligned_text, char, cord(string), cord(string)).
:- mode acc_short_option_name(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_short_option_name(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_short_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !LineCord) :-
    FirstLine0 = short_option_name_line(Params, Option, MaybeArgName,
        ShortName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate, MaybeAddNegVersion),
    (
        MaybeAddNegVersion = no_negative_version
    ;
        MaybeAddNegVersion = add_negative_version,
        SecondLine = short_negated_option_name_line(ShortName),
        cord.snoc(SecondLine, !LineCord)
    ).

%---------------------%

:- func long_option_name_line(option_params, option, maybe_arg_name, string)
    = string.

long_option_name_line(Params, Option, MaybeArgName, LongName0) = Line :-
    Indent = option_name_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate, _MaybeAddNegVersion),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            LongName0, LongName),
        Line = long_negated_option_name_line(LongName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, LongName0, LongName),
            string.format("%s--%s", [s(Indent), s(LongName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, LongName0, LongName),
            MaybeWrappedArgName = maybe_wrap_arg_name(Option, ArgName),
            string.format("%s--%s %s",
                [s(Indent), s(LongName), s(MaybeWrappedArgName)], Line)
        )
    ).

:- func short_option_name_line(option_params, option, maybe_arg_name, char)
    = string.

short_option_name_line(Params, Option, MaybeArgName, ShortName0) = Line :-
    Indent = option_name_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate, _MaybeAddNegVersion),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            ShortName0, ShortName),
        Line = short_negated_option_name_line(ShortName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            string.format("%s-%c", [s(Indent), c(ShortName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            MaybeWrappedArgName = maybe_wrap_arg_name(Option, ArgName),
            string.format("%s-%c %s",
                [s(Indent), c(ShortName), s(MaybeWrappedArgName)], Line)
        )
    ).

%---------------------%

:- func long_negated_option_name_line(string) = string.

long_negated_option_name_line(LongName) = Line :-
    Indent = option_name_indent,
    string.format("%s--no-%s", [s(Indent), s(LongName)], Line).

:- func short_negated_option_name_line(char) = string.

short_negated_option_name_line(ShortName) = Line :-
    Indent = option_name_indent,
    string.format("%s-%c-", [s(Indent), c(ShortName)], Line).

%---------------------%

:- func maybe_wrap_arg_name(option, string) = string.

maybe_wrap_arg_name(Option, ArgName) = MaybeWrappedArgName :-
    ( if
        ArgName = ""
    then
        unexpected($pred, string(Option) ++ " has empty arg name")
    else if
        % Do not put <>s around argument "names" that are actually not names,
        % but instead are either
        %
        % - sets of the allowed values wrapped in {}s, or
        % - default optimization levels, such as -O2.
        ( string.find_first_char(ArgName, '{', _)
        ; string.find_first_char(ArgName, '-', _)
        )
    then
        MaybeWrappedArgName = ArgName
    else
        MaybeWrappedArgName = "<" ++ ArgName ++ ">"
    ).

%---------------------------------------------------------------------------%

:- pred maybe_have_arg(maybe_expect_arg::in, option::in, maybe_arg_name::in,
    T::in, T::out) is det.

maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
        OptionName0, OptionName) :-
    (
        MaybeArgName = no_arg,
        have_no_arg(MaybeExpectArg, Option, OptionName0, OptionName)
    ;
        MaybeArgName = arg_name(_),
        have_arg(MaybeExpectArg, Option, OptionName0, OptionName)
    ).

:- pred have_no_arg(maybe_expect_arg::in, option::in, T::in, T::out) is det.

have_no_arg(MaybeExpectArg, Option, OptionName0, OptionName) :-
    (
        MaybeExpectArg = do_not_expect_arg,
        OptionName = OptionName0
    ;
        MaybeExpectArg = expect_arg,
        string.format("missing arg for %s", [s(string(Option))], Msg),
        unexpected($pred, Msg)
    ).

:- pred have_arg(maybe_expect_arg::in, option::in, T::in, T::out) is det.

have_arg(MaybeExpectArg, Option, OptionName0, OptionName) :-
    (
        MaybeExpectArg = do_not_expect_arg,
        string.format("unexpected arg for %s", [s(string(Option))], Msg),
        unexpected($pred, Msg)
    ;
        MaybeExpectArg = expect_arg,
        OptionName = OptionName0
    ).

%---------------------------------------------------------------------------%

:- pred add_aligned_text(string::in, string::in, string::out) is det.

add_aligned_text(AlignedText, Line0, Line) :-
    string.format("%-39s %s", [s(Line0), s(AlignedText)], Line).

%---------------------------------------------------------------------------%

:- func option_name_indent = string.
:- func option_desc_indent = string.

option_name_indent = "    ".
option_desc_indent = "        ".

:- pred acc_prefixed_line(string::in, string::in,
    cord(string)::in, cord(string)::out) is det.

acc_prefixed_line(Prefix, LineBody, !LineCord) :-
    Line = Prefix ++ LineBody,
    cord.snoc(Line, !LineCord).

%---------------------------------------------------------------------------%
:- end_module libs.print_help.
%---------------------------------------------------------------------------%
