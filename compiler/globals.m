%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: globals.m.
% Main author: fjh.

% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io.state.

%-----------------------------------------------------------------------------%

:- module libs.globals.
:- interface.

:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data. % for module_name

:- import_module bool.
:- import_module getopt_io.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type globals.

:- type compilation_target
    --->    c       % Generate C code (including GNU C)
    ;       il      % Generate IL assembler code
                    % IL is the Microsoft .NET Intermediate Language
    ;       java    % Generate Java
                    % (Work in progress)
    ;       asm.    % Compile directly to assembler via the GCC back-end.
                    % Do not go via C, instead generate GCC's internal
                    % `tree' data structure. (Work in progress.)

:- type foreign_language
    --->    c
%   ;       cplusplus
    ;       csharp
    ;       managed_cplusplus
    ;       java
    ;       il.

    % A string representation of the compilation target suitable
    % for use in human-readable error messages.
    %
:- func compilation_target_string(compilation_target) = string.

    % A string representation of the foreign language suitable
    % for use in human-readable error messages.
    %
:- func foreign_language_string(foreign_language) = string.

    % A string representation of the foreign language suitable
    % for use in machine-readable name mangling.
    %
:- func simple_foreign_language_string(foreign_language) = string.

    % The GC method specifies how we do garbage collection.
    % The last four alternatives are for the C and asm back-ends;
    % the first alternative is for compiling to IL or Java,
    % where the target language implementation handles garbage
    % collection automatically.
    %
:- type gc_method
    --->    automatic   % It is the responsibility of the target language
                        % that we are compiling to to handle GC.

    ;       none        % No garbage collection.
                        % But memory may be recovered on backtracking,
                        % if the --reclaim-heap-on-*failure options are set.

    ;       boehm       % The Boehm et al conservative collector.

    ;       mps         % A different conservative collector, based on
                        % Ravenbrook Limited's MPS (Memory Pool System) kit.
                        % Benchmarking indicated that this one performed worse
                        % than the Boehm collector, so we don't really
                        % support this option anymore.

    ;       accurate.   % Our own home-grown copying collector.
                        % See runtime/mercury_accurate_gc.c
                        % and compiler/ml_elim_nested.m.

    % Returns yes if the GC method is conservative, i.e. if it is `boehm'
    % or `mps'. Conservative GC methods don't support heap reclamation
    % on failure.
    %
:- func gc_is_conservative(gc_method) = bool.

:- type tags_method
    --->    none
    ;       low
    ;       high.

:- type termination_norm
    --->    simple
    ;       total
    ;       num_data_elems
    ;       size_data_elems.

    % Map from module name to file name.
    %
:- type source_file_map == map(module_name, string).

:- type maybe_thread_safe == bool.

:- pred convert_target(string::in, compilation_target::out) is semidet.
:- pred convert_foreign_language(string::in, foreign_language::out) is semidet.
:- pred convert_gc_method(string::in, gc_method::out) is semidet.
:- pred convert_tags_method(string::in, tags_method::out) is semidet.
:- pred convert_termination_norm(string::in, termination_norm::out) is semidet.
:- pred convert_maybe_thread_safe(string::in, maybe_thread_safe::out)
    is semidet.

%-----------------------------------------------------------------------------%
%
% Access predicates for the `globals' structure
%

:- pred globals_init(option_table::di, compilation_target::di, gc_method::di,
    tags_method::di, termination_norm::di, termination_norm::di,
    trace_level::di, trace_suppress_items::di,
    maybe_thread_safe::di, globals::uo) is det.

:- pred get_options(globals::in, option_table::out) is det.
:- pred get_target(globals::in, compilation_target::out) is det.
:- pred get_backend_foreign_languages(globals::in,
    list(foreign_language)::out) is det.
:- pred get_gc_method(globals::in, gc_method::out) is det.
:- pred get_tags_method(globals::in, tags_method::out) is det.
:- pred get_termination_norm(globals::in, termination_norm::out)
    is det.
:- pred get_termination2_norm(globals::in, termination_norm::out)
    is det.
:- pred get_trace_level(globals::in, trace_level::out) is det.
:- pred get_trace_suppress(globals::in, trace_suppress_items::out)
    is det.
:- pred get_source_file_map(globals::in, maybe(source_file_map)::out)
    is det.
:- pred get_maybe_thread_safe(globals::in, maybe_thread_safe::out)
    is det.
:- pred get_extra_error_info(globals::in, bool::out) is det.

:- pred set_option(option::in, option_data::in,
    globals::in, globals::out) is det.
:- pred set_options(option_table::in, globals::in, globals::out)
    is det.
:- pred set_gc_method(gc_method::in, globals::in, globals::out)
    is det.
:- pred set_tags_method(tags_method::in, globals::in, globals::out)
    is det.
:- pred set_trace_level(trace_level::in, globals::in, globals::out)
    is det.
:- pred set_trace_level_none(globals::in, globals::out) is det.
:- pred set_source_file_map(maybe(source_file_map)::in,
    globals::in, globals::out) is det.
:- pred set_extra_error_info(bool::in, globals::in, globals::out)
    is det.

:- pred lookup_option(globals::in, option::in, option_data::out)
    is det.

:- pred lookup_bool_option(globals, option, bool).
:- mode lookup_bool_option(in, in, out) is det.
:- mode lookup_bool_option(in, in, in) is semidet. % implied
:- pred lookup_int_option(globals::in, option::in, int::out) is det.
:- pred lookup_string_option(globals::in, option::in, string::out)
    is det.
:- pred lookup_maybe_string_option(globals::in, option::in,
    maybe(string)::out) is det.
:- pred lookup_accumulating_option(globals::in, option::in,
    list(string)::out) is det.

%-----------------------------------------------------------------------------%
%
% More complex options
%

    % Check if static code addresses are available in the
    % current grade of compilation.
    %
:- pred have_static_code_addresses(globals::in, bool::out) is det.

    % Check if we should include variable information in the layout
    % structures of call return sites.
    %
:- pred want_return_var_layouts(globals::in, bool::out) is det.

    % imported_is_constant(NonLocalGotos, AsmLabels, IsConst)
    % figures out whether an imported label address is a constant.
    % This depends on how we treat labels.
    %
:- pred imported_is_constant(bool::in, bool::in, bool::out) is det.

%-----------------------------------------------------------------------------%
%
% Access predicates for storing a `globals' structure in the io.state
% using io.set_globals and io.get_globals.
%

:- pred globals_io_init(option_table::di, compilation_target::in,
    gc_method::in, tags_method::in, termination_norm::in,
    termination_norm::in, trace_level::in, trace_suppress_items::in,
    maybe_thread_safe::in, io::di, io::uo) is det.

:- pred io_get_target(compilation_target::out, io::di, io::uo) is det.
:- pred io_get_backend_foreign_languages(list(foreign_language)::out,
    io::di, io::uo) is det.

:- pred io_lookup_foreign_language_option(option::in,
    foreign_language::out, io::di, io::uo) is det.

:- pred io_get_gc_method(gc_method::out, io::di, io::uo) is det.
:- pred io_get_tags_method(tags_method::out, io::di, io::uo) is det.
:- pred io_get_termination_norm(termination_norm::out,
    io::di, io::uo) is det.

:- pred io_get_termination2_norm(termination_norm::out,
    io::di, io::uo) is det.

:- pred io_get_trace_level(trace_level::out, io::di, io::uo) is det.

:- pred io_get_trace_suppress(trace_suppress_items::out,
    io::di, io::uo) is det.
:- pred io_get_maybe_thread_safe(maybe_thread_safe::out,
    io::di, io::uo) is det.

:- pred io_get_extra_error_info(bool::out, io::di, io::uo) is det.

:- pred io_get_globals(globals::out, io::di, io::uo) is det.

:- pred io_set_globals(globals::di, io::di, io::uo) is det.

:- pred io_set_option(option::in, option_data::in,
    io::di, io::uo) is det.

:- pred io_set_gc_method(gc_method::in, io::di, io::uo) is det.
:- pred io_set_tags_method(tags_method::in, io::di, io::uo) is det.
:- pred io_set_trace_level(trace_level::in, io::di, io::uo) is det.
:- pred io_set_trace_level_none(io::di, io::uo) is det.
:- pred io_set_extra_error_info(bool::in, io::di, io::uo) is det.

:- pred io_lookup_option(option::in, option_data::out,
    io::di, io::uo) is det.

:- pred io_lookup_bool_option(option::in, bool::out,
    io::di, io::uo) is det.

:- pred io_lookup_int_option(option::in, int::out,
    io::di, io::uo) is det.

:- pred io_lookup_string_option(option::in, string::out,
    io::di, io::uo) is det.

:- pred io_lookup_maybe_string_option(option::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred io_lookup_accumulating_option(option::in, list(string)::out,
    io::di, io::uo) is det.

:- pred io_printing_usage(bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

convert_target(String, Target) :-
    convert_target_2(string.to_lower(String), Target).

:- pred convert_target_2(string::in, compilation_target::out) is semidet.

convert_target_2("java", java).
convert_target_2("asm", asm).
convert_target_2("il", il).
convert_target_2("c", c).

convert_foreign_language(String, ForeignLanguage) :-
    convert_foreign_language_2(string.to_lower(String), ForeignLanguage).

:- pred convert_foreign_language_2(string::in, foreign_language::out)
    is semidet.

convert_foreign_language_2("c", c).
convert_foreign_language_2("mc++", managed_cplusplus).
convert_foreign_language_2("managedc++", managed_cplusplus).
convert_foreign_language_2("managed c++", managed_cplusplus).
convert_foreign_language_2("c#", csharp).
convert_foreign_language_2("csharp", csharp).
convert_foreign_language_2("c sharp", csharp).
convert_foreign_language_2("il", il).
convert_foreign_language_2("java", java).

convert_gc_method("none", none).
convert_gc_method("conservative", boehm).
convert_gc_method("boehm", boehm).
convert_gc_method("mps", mps).
convert_gc_method("accurate", accurate).
convert_gc_method("automatic", automatic).

convert_tags_method("none", none).
convert_tags_method("low", low).
convert_tags_method("high", high).

convert_termination_norm("simple", simple).
convert_termination_norm("total", total).
convert_termination_norm("num-data-elems", num_data_elems).
convert_termination_norm("size-data-elems", size_data_elems).

convert_maybe_thread_safe("yes", yes).
convert_maybe_thread_safe("no",  no).

compilation_target_string(c)    = "C".
compilation_target_string(il)   = "IL".
compilation_target_string(java) = "Java".
compilation_target_string(asm)  = "asm".

foreign_language_string(c) = "C".
foreign_language_string(managed_cplusplus) = "Managed C++".
foreign_language_string(csharp) = "C#".
foreign_language_string(il) = "IL".
foreign_language_string(java) = "Java".

simple_foreign_language_string(c) = "c".
simple_foreign_language_string(managed_cplusplus) = "cpp". % XXX mcpp is better
simple_foreign_language_string(csharp) = "csharp".
simple_foreign_language_string(il) = "il".
simple_foreign_language_string(java) = "java".

gc_is_conservative(boehm) = yes.
gc_is_conservative(mps) = yes.
gc_is_conservative(none) = no.
gc_is_conservative(accurate) = no.
gc_is_conservative(automatic) = no.

%-----------------------------------------------------------------------------%

:- type globals
    --->    globals(
                options                 :: option_table,
                target                  :: compilation_target,
                gc_method               :: gc_method,
                tags_method             :: tags_method,
                termination_norm        :: termination_norm,
                termination2_norm       :: termination_norm,
                trace_level             :: trace_level,
                trace_suppress_items    :: trace_suppress_items,
                source_file_map         :: maybe(source_file_map),
                have_printed_usage      :: bool,
                maybe_thread_safe       :: bool,
                extra_error_info        :: bool
                                        % Is there extra information
                                        % about errors available, that
                                        % could be printed out if `-E'
                                        % were enabled.
            ).

globals_init(Options, Target, GC_Method, TagsMethod,
        TerminationNorm, Termination2Norm, TraceLevel, TraceSuppress,
        MaybeThreadSafe,
    globals(Options, Target, GC_Method, TagsMethod,
        TerminationNorm, Termination2Norm, TraceLevel, TraceSuppress,
        no, no, MaybeThreadSafe, no)).

get_options(Globals, Globals ^ options).
get_target(Globals, Globals ^ target).
get_gc_method(Globals, Globals ^ gc_method).
get_tags_method(Globals, Globals ^ tags_method).
get_termination_norm(Globals, Globals ^ termination_norm).
get_termination2_norm(Globals, Globals ^ termination2_norm).
get_trace_level(Globals, Globals ^ trace_level).
get_trace_suppress(Globals, Globals ^ trace_suppress_items).
get_source_file_map(Globals, Globals ^ source_file_map).
get_maybe_thread_safe(Globals, Globals ^ maybe_thread_safe).
get_extra_error_info(Globals, Globals ^ extra_error_info).

get_backend_foreign_languages(Globals, ForeignLangs) :-
    lookup_accumulating_option(Globals, backend_foreign_languages,
        LangStrs),
    ForeignLangs = list.map(func(String) = ForeignLang :-
        ( convert_foreign_language(String, ForeignLang0) ->
            ForeignLang = ForeignLang0
        ;
            unexpected(this_file, "io_get_backend_foreign_languages: " ++
                "invalid foreign_language string")
        ), LangStrs).

set_options(Options, Globals, Globals ^ options := Options).

set_option(Option, OptionData, !Globals) :-
    get_options(!.Globals, OptionTable0),
    map.set(OptionTable0, Option, OptionData, OptionTable),
    set_options(OptionTable, !Globals).

set_gc_method(GC_Method, Globals, Globals ^ gc_method := GC_Method).

set_tags_method(Tags_Method, Globals,
    Globals ^ tags_method := Tags_Method).

set_trace_level(TraceLevel, Globals,
    Globals ^ trace_level := TraceLevel).
set_trace_level_none(Globals,
    Globals ^ trace_level := trace_level_none).

set_source_file_map(SourceFileMap, Globals,
    Globals ^ source_file_map := SourceFileMap).

lookup_option(Globals, Option, OptionData) :-
    get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

set_extra_error_info(ExtraErrorInfo, Globals,
    Globals ^ extra_error_info := ExtraErrorInfo).

%-----------------------------------------------------------------------------%

lookup_bool_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( OptionData = bool(Bool) ->
        Value = Bool
    ;
        unexpected(this_file, "lookup_bool_option: invalid bool option")
    ).

lookup_string_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( OptionData = string(String) ->
        Value = String
    ;
        unexpected(this_file, "lookup_string_option: invalid string option")
    ).

lookup_int_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( OptionData = int(Int) ->
        Value = Int
    ;
        unexpected(this_file, "lookup_int_option: invalid int option")
    ).

lookup_maybe_string_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( OptionData = maybe_string(MaybeString) ->
        Value = MaybeString
    ;
        unexpected(this_file,
            "lookup_string_option: invalid maybe_string option")
    ).

lookup_accumulating_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( OptionData = accumulating(Accumulating) ->
        Value = Accumulating
    ;
        unexpected(this_file, "lookup_accumulating_option: "
            ++ "invalid accumulating option")
    ).

%-----------------------------------------------------------------------------%

have_static_code_addresses(Globals, IsConst) :-
    get_options(Globals, OptionTable),
    have_static_code_addresses_2(OptionTable, IsConst).

:- pred have_static_code_addresses_2(option_table::in,
    bool::out) is det.

have_static_code_addresses_2(OptionTable, IsConst) :-
    getopt_io.lookup_bool_option(OptionTable, gcc_non_local_gotos,
        NonLocalGotos),
    getopt_io.lookup_bool_option(OptionTable, asm_labels, AsmLabels),
    imported_is_constant(NonLocalGotos, AsmLabels, IsConst).

want_return_var_layouts(Globals, WantReturnLayouts) :-
    % We need to generate layout info for call return labels
    % if we are using accurate gc or if the user wants uplevel printing.
    (
        (
            get_gc_method(Globals, GC_Method),
            GC_Method = accurate
        ;
            get_trace_level(Globals, TraceLevel),
            get_trace_suppress(Globals, TraceSuppress),
            trace_needs_return_info(TraceLevel, TraceSuppress) = yes
        )
    ->
        WantReturnLayouts = yes
    ;
        WantReturnLayouts = no
    ).

    % The logic of this function and how it is used to select the default
    % type_info method must agree with the code in runtime/typeinfo.h.

imported_is_constant(NonLocalGotos, AsmLabels, IsConst) :-
    (
        NonLocalGotos = yes,
        AsmLabels = no
    ->
        %
        % With non-local gotos but no asm labels, jumps to code addresses
        % in different c_modules must be done via global variables; the value
        % of these global variables is not constant (i.e. not computable at
        % load time), since they can't be initialized until we call
        % init_modules().
        %
        IsConst = no
    ;
        IsConst = yes
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

globals_io_init(Options, Target, GC_Method, TagsMethod, TerminationNorm,
        Termination2Norm, TraceLevel, TraceSuppress, MaybeThreadSafe, !IO) :-
    copy(Target, Target1),
    copy(GC_Method, GC_Method1),
    copy(TagsMethod, TagsMethod1),
    copy(TerminationNorm, TerminationNorm1),
    copy(Termination2Norm, Termination2Norm1),
    copy(TraceLevel, TraceLevel1),
    copy(TraceSuppress, TraceSuppress1),
    copy(MaybeThreadSafe, MaybeThreadSafe1),
    globals_init(Options, Target1, GC_Method1, TagsMethod1,
        TerminationNorm1, Termination2Norm1, TraceLevel1,
        TraceSuppress1, MaybeThreadSafe1, Globals),
    io_set_globals(Globals, !IO).

io_get_target(Target, !IO) :-
    io_get_globals(Globals, !IO),
    get_target(Globals, Target).

io_get_gc_method(GC_Method, !IO) :-
    io_get_globals(Globals, !IO),
    get_gc_method(Globals, GC_Method).

io_get_tags_method(Tags_Method, !IO) :-
    io_get_globals(Globals, !IO),
    get_tags_method(Globals, Tags_Method).

io_get_termination_norm(TerminationNorm, !IO) :-
    io_get_globals(Globals, !IO),
    get_termination_norm(Globals, TerminationNorm).

io_get_termination2_norm(Termination2Norm, !IO) :-
    io_get_globals(Globals, !IO),
    get_termination2_norm(Globals, Termination2Norm).

io_get_trace_level(TraceLevel, !IO) :-
    io_get_globals(Globals, !IO),
    get_trace_level(Globals, TraceLevel).

io_get_trace_suppress(TraceSuppress, !IO) :-
    io_get_globals(Globals, !IO),
    get_trace_suppress(Globals, TraceSuppress).

io_get_maybe_thread_safe(MaybeThreadSafe, !IO) :-
    io_get_globals(Globals, !IO),
    get_maybe_thread_safe(Globals, MaybeThreadSafe).

io_get_extra_error_info(ExtraErrorInfo, !IO) :-
    io_get_globals(Globals, !IO),
    get_extra_error_info(Globals, ExtraErrorInfo).

io_get_globals(Globals, !IO) :-
    io.get_globals(UnivGlobals, !IO),
    ( univ_to_type(UnivGlobals, Globals0) ->
        Globals = Globals0
    ;
        unexpected(this_file, "io_get_globals: univ_to_type failed")
    ).

io_set_globals(Globals, !IO) :-
    type_to_univ(Globals, UnivGlobals),
    io.set_globals(UnivGlobals, !IO).

%-----------------------------------------------------------------------------%

io_lookup_option(Option, OptionData, !IO) :-
    io_get_globals(Globals, !IO),
    get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

io_set_option(Option, OptionData, !IO) :-
    io_get_globals(Globals0, !IO),
    get_options(Globals0, OptionTable0),
    map.set(OptionTable0, Option, OptionData, OptionTable),
    set_options(OptionTable, Globals0, Globals1),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
    unsafe_promise_unique(Globals1, Globals),
    io_set_globals(Globals, !IO).

io_set_gc_method(GC_Method, !IO) :-
    io_get_globals(Globals0, !IO),
    set_gc_method(GC_Method, Globals0, Globals1),
    unsafe_promise_unique(Globals1, Globals),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
    io_set_globals(Globals, !IO).

io_set_tags_method(Tags_Method, !IO) :-
    io_get_globals(Globals0, !IO),
    set_tags_method(Tags_Method, Globals0, Globals1),
    unsafe_promise_unique(Globals1, Globals),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
    io_set_globals(Globals, !IO).

io_set_trace_level(TraceLevel, !IO) :-
    io_get_globals(Globals0, !IO),
    set_trace_level(TraceLevel, Globals0, Globals1),
    unsafe_promise_unique(Globals1, Globals),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
    io_set_globals(Globals, !IO).

io_set_extra_error_info(ExtraErrorInfo, !IO) :-
    some [!Globals] (
        io_get_globals(!:Globals, !IO),
        set_extra_error_info(ExtraErrorInfo, !Globals),
        unsafe_promise_unique(!Globals),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
        io_set_globals(!.Globals, !IO)
    ).

    % This predicate is needed because mercury_compile.m doesn't know
    % anything about type trace_level.
io_set_trace_level_none(!IO) :-
    io_set_trace_level(trace_level_none, !IO).

%-----------------------------------------------------------------------------%

io_lookup_foreign_language_option(Option, ForeignLang, !IO) :-
    io_lookup_string_option(Option, String, !IO),
    ( convert_foreign_language(String, ForeignLang0) ->
        ForeignLang = ForeignLang0
    ;
        unexpected(this_file, "io_lookup_foreign_language_option: "
            ++ "invalid foreign_language option")
    ).

io_get_backend_foreign_languages(ForeignLangs, !IO) :-
    io_get_globals(Globals, !IO),
    get_backend_foreign_languages(Globals, ForeignLangs).

io_lookup_bool_option(Option, Value, !IO) :-
    io_get_globals(Globals, !IO),
    lookup_bool_option(Globals, Option, Value).

io_lookup_int_option(Option, Value, !IO) :-
    io_get_globals(Globals, !IO),
    lookup_int_option(Globals, Option, Value).

io_lookup_string_option(Option, Value, !IO) :-
    io_get_globals(Globals, !IO),
    lookup_string_option(Globals, Option, Value).

io_lookup_maybe_string_option(Option, Value, !IO) :-
    io_get_globals(Globals, !IO),
    lookup_maybe_string_option(Globals, Option, Value).

io_lookup_accumulating_option(Option, Value, !IO) :-
    io_get_globals(Globals, !IO),
    lookup_accumulating_option(Globals, Option, Value).

%-----------------------------------------------------------------------------%

io_printing_usage(AlreadyPrinted, !IO) :-
    io_get_globals(Globals0, !IO),
    AlreadyPrinted = Globals0 ^ have_printed_usage,
    Globals1 = Globals0 ^ have_printed_usage := yes,
    unsafe_promise_unique(Globals1, Globals),
        % XXX there is a bit of a design flaw with regard to
        % uniqueness and io.set_globals
    io_set_globals(Globals, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "globals.m".

%-----------------------------------------------------------------------------%
:- end_module globals.
%-----------------------------------------------------------------------------%
