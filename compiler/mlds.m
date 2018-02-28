%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2011 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds.m.
% Main author: fjh.
%
% MLDS - The Medium-Level Data Structure.
%
% This module defines the MLDS data structure itself.
% The MLDS is an intermediate data structure used in compilation;
% we compile Mercury source -> parse tree -> HLDS -> MLDS -> target (e.g. C).
% See notes/compiler_design.html for more information about the MLDS & LLDS.
%
% The MLDS is intended to be suitable for generating target code in
% languages such as Java, Java bytecode, high-level C, C++, or C--, etc.
% This is as opposed to the LLDS, which is better suited for generating
% assembler or the very low-level C or GNU C (lower level than C--) that
% the original Mercury compiler backend generates. In the LLDS, we are
% doing all our own register allocation and stack manipulation, but with
% MLDS, those tasks are the responsibility of the target.
% The one really important simplification that we do make relative to the
% HLDS is that MLDS does not have any support for non-determinism, so the
% HLDS->MLDS compiler must compile non-deterministic code into code that
% uses continuation passing.
%
% The MLDS data structure is quite full-featured, including for example
% support for arbitrary nesting, multiple return values, and tagged pointers.
% However, many
% of the intended target languages don't support all of those features.
% Therefore the HLDS->MLDS compiler must ensure that the final MLDS code that
% it eventually generates does not use features which the target does not
% support. This will (presumably) be accomplished by having handle_options.m
% set various flags according to the selected target, and having the
% HLDS->MLDS compiler take account of those flags and either generate simpler
% MLDS code in the first place or run some extra simplification passes over
% the MLDS code before invoking the MLDS->target compiler.
%
%---------------------------------------------------------------------------%
%
% Mapping Mercury names to MLDS names
%
%
% 1. Modules
%
% Mercury module names map directly to MLDS package names, except that
% modules in the Mercury standard library map get a `mercury' prefix,
% e.g. `mercury.builtin', `mercury.io', `mercury.univ', etc.
% [Rationale: omitting the `mercury' prefix would lead to namespace
% pollution in the generated target language code.]
%
% 2. Procedure names
%
% HLDS procedure names map directly to MLDS function names.
% MLDS function names are structured terms that include sufficient
% information (arity, pred_or_func indicator, mode number, etc.)
% to avoid any ambiguities.
%
% [Rationale: the reason for keeping structured names rather than flattened
% names at the MLDS level is that we don't want to do name mangling at
% the HLDS -> MLDS stage, since we don't know what restrictions the target
% language will impose. For example, some target languages (e.g. C++, Java)
% will support overloading, while others (e.g. C) will not.]
%
% 3. Procedure signatures
%
% MLDS function signatures are determined by the HLDS procedure's
% argument types, modes, and determinism.
% Procedures arguments with dummy types (such as `io.state' or `store(_)')
% are not passed.
% Procedure arguments with top_unused modes are not passed.
% Procedures arguments with top_in modes are passed as input.
% Procedures arguments with top_out modes are normally passed by reference.
% However, several alternative approaches are also supported (see below).
%
% Procedures with determinism model_det need no special handling.
% Procedures with determinism model_semi must return a boolean.
% Procedures with determinism model_non get passed a continuation;
% if the procedure succeeds, it must call the continuation, and if it fails,
% it must return.
%
% With the `--copy-out' option, arguments with top_out modes will be returned
% by value. This requires the target language to support multiple return
% values. The MLDS->target code generator can of course handle that by mapping
% functions with multiple return values into functions that return a struct,
% array, or tuple.
% With the `--nondet-copy-out' option, arguments for nondet procedures with
% top_out modes will be passed as arguments to the continuation.
%
% [Rationale: this mapping is designed to be as simple as possible,
% and to map as naturally to the target language as possible.
% The complication with the `--copy-out' and `--nondet-copy-out'
% options is needed to cater to different target languages.
% Some target languages (e.g. C) fully support pass-by-reference,
% and then it is most consistent to use it everywhere.
% Some (e.g. IL) support pass-by-reference, but don't allow references
% to be stored in the environment structs needed for continuations,
% and so can't use pass-by-reference for nondet procedures.
% Others (e.g. Java) don't support pass-by-reference at all.]
%
% 4. Variables
%
% MLDS variable names are determined by the HLDS variable name and
% (to avoid ambiguity) variable number. The MLDS variable name is
% a structured term that keeps the original variable name separate
% from the distinguishing variable number. It is up to each individual backend
% to mangle the variable name and number to avoid ambiguity where necessary.
%
% [Rationale: the reason for keeping structured names rather than
% mangled names at the MLDS level is that in some cases the mangling is
% undesirable, as the original HLDS variable names are what is required
% (for instance, when interfacing with foreign code which includes
% references to the original HLDS variable names).]
%
% 5. Global data
%
% MLDS names for global data are structured; they hold some
% information about the kind of global data (see the mlds_data_name type).
%
% 6. Types
%
% If there is an MLDS type corresponding to a Mercury type, then
% the Mercury type name maps directly to the MLDS type name,
% suitably module-qualified of course.
% The MLDS type name includes the type arity (arity overloading is allowed).
% However, if a low-level data representation scheme is used,
% then some Mercury types may not have corresponding MLDS type names
% (that is, the corresponding MLDS type may be just `MR_Word' or its
% equivalent).
%
% 7. Data constructors.
%
% With --high-level-data, Mercury data constructors normally get mapped to
% MLDS types nested within the MLDS type for the Mercury data type to which
% the constructors belong. There are some exceptions; see ml_type_gen.m
% for full details. The MLDS type name includes the constructor arity.
% [Rationale: Mercury allows data constructors to be overloaded on
% their result type within a single module, and also allows them
% to be overloaded on arity within a single type. Nesting resolves
% the first problem, and keeping the arity in the MLDS type name
% resolves the second.]
% XXX There is a problem in the Java back-end with Mercury types for which
% the constructor name is the same as the type name.
%
% XXX Also need to think about equivalence types and no_tag types.
%
% XXX There are problems with the RTTI definitions for the Java back-end.
% Under the current system, the definitions are output as static variables
% with static initializers, ordered so that subdefinitions always appear before
% the definition which uses them. This is necessary because in Java, static
% initializers are performed at runtime in textual order, and if a definition
% relies on another static variable for its constructor but said variable has
% not been initialized, then it is treated as `null' by the JVM with no
% warning.
% The problem with this approach is that it won't work for cyclic definitions.
% eg:
%       :- type foo ---> f(bar) ; g.
%       :- type bar ---> f2(foo) ; g2
% At some point this should be changed so that initialization is performed by 2
% phases: first allocate all of the objects, then fill in the fields.
%
% 8. Insts and modes
%
% Inst and mode definitions do not get translated into MLDS.
%
% [Inst and mode definitions do however affect the signatures of the
% MLDS functions used to implement each procedure.]
%
% 9. Type classes.
%
% Currently type classes are handled early and at a fairly low level.
% It is not yet clear how easy it will be to convert this to MLDS.
% It may depend to some degree on the target language.
% But if it is possible, then when it is done the target language
% generator will be able to ignore the issue of type classes.
%
% For language interoperability, however, it might be nice if the
% translation were done at higher level.
%
% Mercury type classes should map directly to MLDS interfaces.
%
% Mercury instance definitions should map to classes which implement the
% corresponding interface. Note that if there is an instance declaration
% `:- instance foo(bar)', then the MLDS type for `bar' will *not* implement
% the MLDS interface for `foo' -- instead, there will be a new MLDS type
% for `instance foo(bar)' which implements that interface.
%
%---------------------------------------------------------------------------%
%
% Mapping MLDS names to the target language.
%
%
% Ultimately the exact choice of how MLDS names are mapped to
% the target language is up to the target language generator.
% So the remarks here are just guidelines, not strict rules.
%
% 1. Names.
%
% If the target language has standard conventions about certain categories
% of names beginning with uppercase or lowercase letters, then the target
% language generator should generate names that respect those conventions.
%
% An MLDS name may contain arbitrary characters.
% If the target language has restrictions on what names can be used
% in identifiers, then it is the responsibility of the target language
% generator to mangle MLDS names accordingly to ensure that they abide
% by those restrictions.
%
% 2. Packages.
%
% MLDS packages should be mapped directly to the corresponding notion
% in the target language, if possible.
% If the target doesn't have a notion of packages, then they should be
% mapped to names of the form "foo.bar.baz" or if dots are not allowed
% then to "foo__bar__baz".
%
% 3. Overloading.
%
% If the target does not support overloading, then any Mercury names which
% are overloaded within a single Mercury module must be qualified to avoid
% ambiguity. However, Mercury names which are not overloaded should not
% be qualified. If a name is overloaded but occurs only once in the module's
% interface then the version in the interface should not be qualified.
% The arity-zero version of type should not be arity-qualified
% unless this would cause ambiguity with an unqualified name generated by
% the previous rule. Likewise, the arity-zero version of function
% (i.e. a constant) should not be function-qualified or arity-qualified
% unless this would cause ambiguity with an unqualified name generated
% the aforementioned rule.
% The first mode of a predicate should not be mode-qualified.
%
% [Rationale: name mangling should be avoided where possible, because
% this makes it easier for the user to interoperate with other languages
% and to use tools which do not properly demangle Mercury names.]
%
% 4. Procedures.
%
% If a procedure name needs to be qualified, the qualification should be
% done by appending "_f" for functions or "_p" for predicates,
% optionally followed by the arity,
% optionally followed by an underscore and then the mode number,
% optionally followed by "_i" and then the MLDS function sequence number
% (for internal MLDS functions, used e.g. to implement backtracking).
%
% [Rationale: any qualifiers should go at the end of a name, so that
% command-line completion works even for mangled names (and hopefully
% even shows all the possible alternatives...).]
%
% To avoid ambiguity as to whether a name is qualified or not,
% any procedures whose unqualified name matches the pattern for qualified
% names, i.e. the regular expression `.*_[fp][0-9]*(_[0-9]+)?(_i[0-9]+)?',
% should always be qualified (even if not overloaded).
%
% 5. Types.
%
% If a type name needs to be qualified, the qualification should be
% done by appending an underscore followed by the arity.
%
% To avoid ambiguity as to whether a name is qualified or not,
% any types whose unqualified name matches the pattern for qualified
% names, i.e. the regular expression `.*_[0-9]+',
% should always be qualified (even if not overloaded).
%
%---------------------------------------------------------------------------%
%
% Notes on garbage collection and liveness.
%
%
% "Liveness-accurate GC" is GC in which the collector does not trace local
% variables which are definitely not live according to a straight-forward
% static analysis of definite-deadness/possible-liveness. Liveness-accurate
% GC is desirable, in general, since tracing through variables which are
% no longer live may lead to excessive memory retention for some programs.
% However these programs are relatively rare, so liveness-accurate GC
% is not always worth the extra complication.
%
% The MLDS is therefore designed to optionally support liveness-accurate
% GC, if the target language supports it. If liveness-accurate GC is
% supported and enabled, then it is the responsibility of the target
% language implementation to do whatever is needed to avoid having the GC
% trace variables which have gone out of scope.
%
% That means that to support liveness-accurate the HLDS->MLDS code
% generator just needs to cover the cases where a straight-forward liveness
% calculation on the generated MLDS does not match up with the desired
% result of a straight-forward liveness calculation on the HLDS. That is,
% the HLDS->MLDS code generator must generate code to clobber variables
% which are no longer live according to a straight-forward liveness
% calculation on the HLDS but which have not gone out of scope in
% the generated MLDS. For example, with our current HLDS->MLDS code
% generation scheme, this is the case for variables in the `else' of a
% nondet if-then-else once the `then' has been entered.
% (XXX Currently ml_code_gen.m does _not_ clobber those variables, though.)
%
% The rationale for leaving most of the responsibility for liveness-accurate
% GC up to the MLDS back-end is as follows: at very least we need the
% target language implementation's _cooperation_, since if the MLDS code
% generator inserts statements to clobber variables that are no longer live,
% then an uncooperative target language implementation could just optimize
% them away, since they are assignments to dead variables. Given this need
% for the MLDS target back-end's cooperation, it makes sense to assign as
% much of the responsibility for this task as is possible to the MLDS target
% back-end, to keep the front-end simple and to keep the responsibility
% for this task in one place.
%
% But in cases such as nondet if-then-else, where HLDS-liveness does not
% match MLDS-liveness, we can't just leave it to the MLDS target back-end,
% because that would require assuming an unreasonably smart liveness
% calculation in the MLDS target back-end, so in such cases we do need
% to handle it in the HLDS->MLDS code generator.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_global_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % The type `mlds' is the actual MLDS.
:- type mlds
    --->    mlds(
                % The original Mercury module name.
                mlds_name               :: mercury_module_name,

                % The MLDS code itself.

                % Packages/classes to import.
                mlds_toplevel_imports   :: list(mlds_import),

                mlds_global_defns       :: ml_global_data,

                % Definitions of the classes that represent types
                % with high level data, and the types of the environment
                % structures we introduce while flattening nested functions
                % (unless we are compiling to a target that doesn't need them
                % to be flattened).
                mlds_type_defns         :: list(mlds_class_defn),

                % Definitions of the structures that hold the tables
                % of tabled procedures.
                mlds_table_struct_defns :: list(mlds_global_var_defn),

                % Definitions of the translated procedures.
                mlds_proc_defns         :: list(mlds_function_defn),

                % The names of init and final preds.
                mlds_init_preds         :: list(string),
                mlds_final_preds        :: list(string),

                % Code defined in some other language, e.g. for
                % `pragma foreign_decl', etc.
                mlds_foreign_code_map   :: map(foreign_language,
                                            mlds_foreign_code),

                mlds_exported_enums     :: list(mlds_exported_enum)
            ).

:- func mlds_get_module_name(mlds) = mercury_module_name.

%---------------------------------------------------------------------------%
%
% Module names of various kinds.
%

:- type mercury_module_name == sym_name.module_name.

    % An mlds_module_name specifies the name of an mlds package or class.
    % XXX This is wrongly named as it is also used for class names.
    %
:- type mlds_module_name.

    % An mlds_package_name specifies the name of an mlds package.
    % XXX This is wrongly named as it is used for module names in the Java
    % backend.
    %
:- type mlds_package_name == mlds_module_name.

    % Given the name of a Mercury module, return the name of the corresponding
    % MLDS package in which this module is defined.
    %
:- func mercury_module_name_to_mlds(mercury_module_name) = mlds_module_name.

    % Given the name of a Mercury module, and a package name, return the
    % name of the corresponding MLDS module name in which this module is
    % defined.
    % In this case, the package is specified as the first parameter (c.f.
    % mercury_module_name_to_mlds above).
    %
:- func mercury_module_and_package_name_to_mlds(mercury_module_name,
    mercury_module_name) = mlds_module_name.

    % Given the name of a Mercury module return the fully qualified module
    % name, e.g. for the name System.Object which is defined in the source
    % package mscorlib, it will return System.Object.
    %
:- func mlds_module_name_to_sym_name(mlds_package_name) = sym_name.

    % Give the name of a Mercury module, return the name of the corresponding
    % MLDS package.
    %
:- func mlds_module_name_to_package_name(mlds_module_name) = sym_name.

    % Is the current module a member of the std library,
    % and if so which module is it?
    %
:- pred is_std_lib_module(mlds_module_name::in, mercury_module_name::out)
    is semidet.

    % For the Java back-end, we need to distinguish between module qualifiers
    % and type qualifiers, because type names get the case of their initial
    % letter inverted (i.e. lowercase => uppercase).
    %
:- type mlds_qual_kind
    --->    module_qual
    ;       type_qual.

    % Given an MLDS module name (e.g. `foo.bar'), append another class
    % qualifier (e.g. for a class `baz'), and return the result (e.g.
    % `foo.bar.baz'). The `arity' argument specifies the arity of the class.
    % The qual_kind argument specifies the qualifier kind of the module_name
    % argument.
    %
:- func mlds_append_class_qualifier(mlds_target_lang, mlds_module_name,
    mlds_qual_kind, mlds_class_name, arity) = mlds_module_name.
:- func mlds_append_class_qualifier_module_qual(mlds_module_name,
    mlds_class_name, arity) = mlds_module_name.

    % Append an arbitrary qualifier to the module name and leave the package
    % name unchanged.
    %
:- func mlds_append_name(mlds_module_name, string) = mlds_module_name.

%---------------------------------------------------------------------------%
%
% Module imports.
%

:- type mlds_imports == list(mlds_import).

    % For the C backend, we generate a `.mh' file containing the
    % prototypes for C functions generated for `:- pragma foreign_export'
    % declarations, and a `.mih' file containing the prototypes
    % for the C functions generated by the compiler which should
    % not be visible to the user.
    %
    % For languages which don't support multiple interfaces, it is
    % probably OK to put everything in the compiler-visible interface.
:- type mercury_mlds_import_type
    --->    user_visible_interface
    ;       compiler_visible_interface.

    % An mlds_import specifies  FIXME
    % Currently an import just gives the name of the package to be imported.
    % This might perhaps need to be expanded to cater to different kinds of
    % imports, e.g. imports with wild-cards ("import java.lang.*").
:- type mlds_import
    --->    mercury_import(
                mercury_mlds_import_type    :: mercury_mlds_import_type,
                import_name                 :: mlds_module_name
            ).

%---------------------------------------------------------------------------%
%
% Global variable definitions.
%

    % An mlds_global_var represents a variable or constant that is defined
    % outside all functions and classes.
    %
:- type mlds_global_var_defn
    --->    mlds_global_var_defn(
                mgvd_name               :: mlds_global_var_name,
                mgvd_context            :: prog_context,
                mgvd_decl_flags         :: mlds_global_var_decl_flags,

                mgvd_type               :: mlds_type,
                mgvd_init               :: mlds_initializer,

                % If accurate GC is enabled, we associate with each global
                % variable the compiler-generated gc trace functions that will
                % tell the accurate collector what roots (i.e. pointers to
                % heap cells allocated by the accurate gc system) exist inside
                % that global variable.
                %
                % This field may contain gc_no_stmt if the variable contains
                % no such roots, either because its contents are fully static
                % (i.e. the global variable is actually a global constant),
                % or because all its dynamic parts point to heap memory
                % allocated by some other system (e.g. plain non-gc malloc).
                mgvd_gc                 :: mlds_gc_statement
            ).

:- type mlds_global_var_decl_flags
    --->    mlds_global_var_decl_flags(
                mgvdf_access            :: global_var_access,
                mgvdf_constness         :: constness
            ).

:- func global_dummy_var = qual_global_var_name.

%---------------------%

    % Note that `one_copy' variables *must* have an initializer
    % (the GCC back-end, when it still existed, relied on this).
    % XXX Currently we only record the type for structs.
    % We should do the same for objects and arrays.
    %
:- type mlds_initializer
    --->    init_obj(mlds_rval)
    ;       init_struct(mlds_type, list(mlds_initializer))
    ;       init_array(list(mlds_initializer))
    ;       no_initializer.

:- type initializer_array_size
    --->    array_size(int)
    ;       no_size.
            % Either the size is unknown, or the data is not an array.

:- func get_initializer_array_size(mlds_initializer) = initializer_array_size.

%---------------------%

    % If accurate GC is enabled, we associate with each variable
    % (including function parameters) the code needed to initialise
    % or trace that variable when doing GC (in the GC trace function).
    % `gc_no_stmt' here indicates that no code is needed (e.g. because
    % accurate GC isn't enabled, or because the variable can never
    % contain pointers to objects on the heap).
    % 'gc_trace_code' indicates that GC tracing code is required for the
    % variable.
    % 'gc_initialiser' indicates that the variable is a compiler-generated
    % variable that needs to be initialised before tracing the other
    % variables for the predicate (used when generating closures).
    %
:- type mlds_gc_statement
    --->    gc_trace_code(mlds_stmt)
    ;       gc_initialiser(mlds_stmt)
    ;       gc_no_stmt.

%---------------------------------------------------------------------------%
%
% Class definitions.
%
% While the general form allows the definition of a class, many do not define
% any methods, and are effectively just type definitions.
%

    % Note that standard C doesn't support empty structs, so when targeting C,
    % it is the MLDS code generator's responsibility to ensure that each
    % generated MLDS class has at least one base class or non-static
    % data member.
    %
:- type mlds_class_defn
    --->    mlds_class_defn(
                mcd_class_name      :: mlds_class_name,
                mcd_class_arity     :: arity,
                mcd_context         :: prog_context,
                mcd_decl_flags      :: mlds_class_decl_flags,

                mcd_kind            :: mlds_class_kind,

                % Imports these classes (or modules, packages, ...).
                mcd_imports         :: list(mlds_import),

                % Inherits these base classes (unless it is an env class).
                mcd_inherits        :: mlds_class_inherits,

                % Implements these interfaces.
                mcd_implements      :: list(mlds_interface_id),

                % Type parameters.
                mcd_tparams         :: list(type_param),

                % Contains these members.
                % The mcd_member_methods field is used only by mlds_to_java.m;
                % it should be set to the empty list everywhere else.
                mcd_member_fields   :: list(mlds_field_var_defn),
                mcd_member_classes  :: list(mlds_class_defn),
                mcd_member_methods  :: list(mlds_function_defn),

                % Has these constructors.
                mcd_ctors           :: list(mlds_function_defn)
            ).

:- type mlds_class_decl_flags
    --->    mlds_class_decl_flags(
                mcdf_access         :: class_access,
                mcdf_overridability :: overridability,
                mcdf_constness      :: constness
            ).

:- type mlds_class_inherits
    --->    inherits_nothing
            % There is no base class.

    ;       inherits_class(mlds_class_id)
            % There is one base class. (We don't have a case for more than one
            % base class, because (a) none of the (current) MLDS target
            % languages support multiple inheritance; (b) even if some did,
            % we may not want to use that capability.

    ;       inherits_generic_env_ptr_type.
            % There is no base class, but there is a base *type*, and it is
            % mlds_generic_env_ptr_type. Used only to implement environment
            % structures, and only when put_nondet_env_on_heap is set,
            % which means only when targeting C# or Java.

:- type qual_class_name
    --->    qual_class_name(mlds_module_name, mlds_qual_kind, mlds_class_name).
:- type mlds_class_name == string.

:- type mlds_class_id
    --->    mlds_class_id(qual_class_name, arity, mlds_class_kind).
:- type mlds_interface_id
    --->    mlds_interface_id(qual_class_name, arity, mlds_class_kind).

:- type mlds_class_kind
    --->    mlds_class     % A generic class: can inherit a class and
                           % interfaces.

    ;       mlds_interface % A class with no variable data members (can only
                           % inherit other interfaces).

    ;       mlds_struct    % A value class (can only inherit other structs).

    ;       mlds_enum.     % A class with one integer member and a bunch
                           % of static consts (cannot inherit anything).

:- type mlds_field_var_defn
    --->    mlds_field_var_defn(
                mfvd_name               :: mlds_field_var_name,
                mfvd_context            :: prog_context,
                mfvd_decl_flags         :: mlds_field_var_decl_flags,

                mfvd_type               :: mlds_type,
                mfvd_init               :: mlds_initializer,

                % If accurate GC is enabled, we associate with each variable
                % the code needed to initialise or trace that variable when
                % doing GC (in the compiler-generated gc trace functions).
                mfvd_gc                 :: mlds_gc_statement
            ).

:- type mlds_field_var_decl_flags
    --->    mlds_field_var_decl_flags(
                % Field variables are implicitly always "public" within
                % the class that defines them.
                mfvdf_per_instance      :: per_instance,
                mfvdf_constness         :: constness
            ).

%---------------------------------------------------------------------------%
%
% Function definitions.
%

:- type mlds_function_defn
    --->    mlds_function_defn(
                mfd_function_name       :: mlds_function_name,
                mfd_context             :: prog_context,
                mfd_decl_flags          :: mlds_function_decl_flags,

                % Identifies the original Mercury procedure, if any.
                mfd_orig_proc           :: maybe(pred_proc_id),

                % The argument types and return types.
                mfd_param               :: mlds_func_params,

                mfd_body                :: mlds_function_body,

                % The set of environment variables referred to
                % by the function body.
                mfd_env_vars            :: set(string),

                % Information used to generate tail recursion errors.
                mfd_tail_rec            :: maybe(require_tail_recursion)
            ).

:- type mlds_function_decl_flags
    --->    mlds_function_decl_flags(
                mfdf_access         :: function_access,
                mfdf_per_instance   :: per_instance
            ).

%---------------------%

:- type qual_function_name
    --->    qual_function_name(mlds_module_name, mlds_function_name).
            % Function names can only ever be module qualified.

:- type qual_func_label
    --->    qual_func_label(mlds_module_name, mlds_func_label).
            % Function labels can only ever be module qualified.

:- type qual_proc_label
    --->    qual_proc_label(mlds_module_name, mlds_proc_label).
            % Procedure labels can only ever be module qualified.

%---------------------%

:- type mlds_function_name
    --->    mlds_function_name(
                mlds_plain_func_name
            )
    ;       mlds_function_export(
                % A pragma foreign_export name.
                string
            ).

:- type mlds_plain_func_name
    --->    mlds_plain_func_name(
                % Identifies the source code predicate or function.
                pfn_func_label      :: mlds_func_label,

                % Specifies the HLDS pred_id.
                % This should generally not be needed much, since all the
                % necessary information should be in the mlds_pred_label
                % and/or in the mlds_entity_defn. However, the target
                % generator may want to refer to the HLDS for additional
                % information.
                pfn_pred_id         :: pred_id
            ).

:- type mlds_func_label
    --->    mlds_func_label(
                mlds_proc_label,

                % The second field will be proc_func for the MLDS function
                % we generate for a HLDS procedure as a whole, but it will be
                % some other function symbol wrapper around N for the Nth
                % auxiliary MLDS function we generate for that procedure,
                % e.g. to handle backtracking. (It is actually N, not N-1,
                % because mlds_aux_func_seq_nums start at 1.)
                mlds_maybe_aux_func_id
            ).

:- type mlds_maybe_aux_func_id
    --->    proc_func
    ;       proc_aux_func(int)
    ;       gc_trace_for_proc_func
    ;       gc_trace_for_proc_aux_func(int).

:- type mlds_proc_label
    --->    mlds_proc_label(mlds_pred_label, proc_id).

    % An mlds_pred_label is a structure containing information that
    % uniquely identifies a HLDS predicate within a given module.
    %
    % Note that a predicate can have both a declaring and a defining module.
    % The defining module is the module that provides the code for the
    % predicate, the declaring module contains the `:- pred' declaration.
    % When these are different, as for specialised versions of predicates
    % from `.opt' files, the defining module's name is added as a
    % qualifier to the pred name.
    %
:- type mlds_pred_label
    --->    mlds_user_pred_label(
                pred_or_func,       % predicate/function
                maybe(mercury_module_name),
                                    % The declaring module,
                                    % if different to the defining module
                string,             % Name.
                arity,              % Arity.
                code_model,         % Code model.
                bool                % Function without return value
                                    % (i.e. non-default mode).
            )
    ;       mlds_special_pred_label(
                string,             % pred name
                maybe(mercury_module_name),
                                    % The module declaring the type,
                                    % if this is different to module defining
                                    % the special_pred.
                string,             % The type name.
                arity               % The type arity.
            ).

%---------------------%

    % It is possible for the function to be defined externally,
    % if the Mercury procedure has a `:- pragma external_{pred/func}'.
    % (If you want to generate an abstract body, consider adding another
    % alternative here).
    %
:- type mlds_function_body
    --->    body_defined_here(mlds_stmt)
    ;       body_external.

%---------------------%

:- type mlds_func_params
    --->    mlds_func_params(
                list(mlds_argument),    % names and types of arguments (inputs)
                mlds_return_types       % types of return values (outputs)
            ).

:- type mlds_argument
    --->    mlds_argument(
                mlds_local_var_name,    % Argument name.
                mlds_type,              % Argument type.
                mlds_gc_statement       % Code for GC tracing this argument.
            ).

    % An mlds_func_signature is like an mlds_func_params, except that
    % it includes only the types of the function's arguments,
    % and not their names.
    %
:- type mlds_func_signature
    --->    mlds_func_signature(
                mlds_arg_types,    % argument types
                mlds_return_types  % return types
            ).

:- type mlds_arg_types == list(mlds_type).
:- type mlds_return_types == list(mlds_type).

:- func mlds_std_tabling_proc_label(mlds_proc_label) = mlds_proc_label.

:- func mlds_get_arg_types(list(mlds_argument)) = list(mlds_type).

:- func mlds_get_func_signature(mlds_func_params) = mlds_func_signature.

%---------------------------------------------------------------------------%
%
% Local variable definitions.
%

:- type mlds_local_var_defn
    --->    mlds_local_var_defn(
                mlvd_name               :: mlds_local_var_name,
                mlvd_context            :: prog_context,
                % Local variables don't need flags. They are always local
                % and modifiable.

                mlvd_type               :: mlds_type,
                mlvd_init               :: mlds_initializer,

                % If accurate GC is enabled, we associate with each variable
                % the code needed to initialise or trace that variable when
                % doing GC (in the compiler-generated gc trace functions).
                mlvd_gc                 :: mlds_gc_statement
            ).

%---------------------------------------------------------------------------%
%
% Declaration flags.
%

:- type global_var_access
    --->    gvar_acc_module_only
    ;       gvar_acc_whole_program.

    % The accessibility of classes themselves.
:- type class_access
    --->    class_public
    ;       class_private.

:- type function_access
    --->    func_public
            % Accessible to anyone.

    ;       func_private
            % Accessible only to the module or class that defines the function.

    ;       func_local.
            % Accessible only within the block in which this (nested) function
            % is defined.

:- type constness
    --->    modifiable
    ;       const.

:- type per_instance
    --->    one_copy
            % I.e. "static" storage duration (but not necessarily static
            % linkage) or static member function. Note that `one_copy'
            % variables *must* have an initializer.

    ;       per_instance.
            % I.e. "auto" local variable in function, or non-static
            % member of class.

:- type overridability
    --->    overridable
    ;       sealed.     % i.e. the class cannot be inherited from,
                        % or the virtual method is not overridable.

%---------------------------------------------------------------------------%
%
% Types.
%

:- type mlds_type
    --->    mercury_type(
                % A Mercury data type.

                % The exact Mercury type.
                mer_type,

                % What kind of type it is: enum, float, ...
                type_ctor_category,

                % A representation of the type which can be used to determine
                % the foreign language representation of the type.
                exported_type
            )

    ;       mlds_mercury_array_type(mlds_type)
            % The Mercury array type is treated specially, some backends
            % will treat it like any other mercury_type, whereas other may
            % use a special representation for it.
            % Arrays are type constructors in some backends, and so it is
            % easier to represent it here as a special type constructor.
            % (if we used the mercury_type representation above, we would
            % only classify the topmost level of the type, whereas we
            % really want to classify the element type for arrays, so
            % we can generate int[] for array(int)).
            % Note that mlds_mercury_array_type/1 is used for representing
            % Mercury arrays, whereas mlds_array_type/1 (see below)
            % is used for representing the target language's native arrays.

    ;       mlds_cont_type(mlds_return_types)
            % The type for the continuation functions used to handle
            % nondeterminism.

    ;       mlds_commit_type
            % mlds_commit_type is used for storing information about a
            % commit. This is an abstract type; the exact definition
            % will depend on the back-end. The only operations on this ADT
            % are `try_commit' and `do_commit'. This type holds
            % information about the `try_commit' stack frame that is
            % needed to unwind the stack when a `do_commit' is executed.
            %
            % For the C back-end, if we are implementing do_commit/try_commit
            % using setjmp/longjmp, then mlds_commit_type will be jmp_buf.
            % If we are implementing them using GNU C nested functions, then
            % it will be `__label__'; in this case, the local variable
            % of this "type" is actually a label, and doing a goto to that
            % label will unwind the stack.
            %
            % If the back-end implements commits using the target language's,
            % try/catch-style exception handling, as in Java/C++/etc., then
            % the target language implementation's exception handling support
            % will keep track of the information needed to unwind the stack,
            % and so variables of this type don't need to be declared at all.
            %
            % See also the comments in ml_code_gen.m which show how commits
            % can be implemented for different target languages.

    ;       mlds_native_bool_type
    ;       mlds_native_int_type
    ;       mlds_native_uint_type
    ;       mlds_native_float_type
    ;       mlds_native_char_type
            % MLDS native builtin types.
            % These are the builtin types of the MLDS target language,
            % whatever that may be.
            % Currently we don't actually use many of these.

    ;       mlds_foreign_type(
                % This is a type of the target language.
                foreign_language_type
            )

    ;       mlds_class_type(
                % MLDS types defined using mlds_class_defn.

                mlds_class_id
            )

    ;       mlds_array_type(mlds_type)
            % MLDS array types.
            % These are single-dimensional, and can be indexed
            % using the `field' lval with an `offset' mlds_field_id;
            % indices start at zero.
            %
            % Note that mlds_array_type/1 is used for representing
            % the target language's native arrays, whereas
            % mlds_mercury_array_type/1 (see above) is used for
            % representing Mercury arrays.
            %
            % Currently MLDS array types are used for
            % (a) static constants that would otherwise be allocated
            %     with a `new_object', if we are using the low-level
            %     data representation (--no-high-level-data)
            % (b) for static constants of certain Mercury types which are
            %     always represented using the low-level data
            %     representation, regardless of --high-level-data,
            %     in particular closures and type_infos.
            % (c) for any other arrays generated internally by the
            %     MLDS code generator, e.g. the arrays used for
            %     string switches.

    ;       mlds_mostly_generic_array_type(list(mlds_type))
            % A generic array with some float elements. All other elements
            % are mlds_generic_type.
            %
            % This is the same as mlds_array_type(mlds_generic_type) except
            % that some element types are mlds_native_float_type instead of
            % mlds_generic_type. In C, it is not possible to initialize an
            % element of a generic array with a float literal, so we replace
            % the generic array type with a structure type containing a
            % combination of generic fields and float fields.

    ;       mlds_ptr_type(mlds_type)
            % Pointer types.
            % Currently these are used for handling output arguments.

    ;       mlds_func_type(mlds_func_params)
            % Function types.
            % For the C back-end, these are mapped to function pointer types,
            % since C doesn't have first-class function types.

    ;       mlds_generic_type
            % A generic type (e.g. `Word') that can hold any Mercury value.
            % This is used for implementing polymorphism.

    ;       mlds_generic_env_ptr_type
            % A generic pointer type (e.g. `void *' in C) that can be used
            % to point to the environment (set of local variables) of the
            % containing function. This is used for handling nondeterminism,
            % if the target language doesn't support nested functions, and
            % also for handling closures for higher-order code.

    ;       mlds_type_info_type

    ;       mlds_pseudo_type_info_type

    ;       mlds_rtti_type(rtti_id_maybe_element)

    ;       mlds_tabling_type(proc_tabling_struct_id)

    ;       mlds_unknown_type.
            % A type used by the ML code generator for references to variables
            % that have yet to be declared. This occurs once in ml_code_util.m
            % where references to env_ptrs are generated but the declaration
            % of these env_ptrs does not occur until the ml_elim_nested pass.

:- func mercury_type_to_mlds_type(module_info, mer_type) = mlds_type.

%---------------------------------------------------------------------------%
%
% Statements.
%

:- type while_loop_kind
    --->    may_loop_zero_times
    ;       loop_at_least_once.

:- type mlds_stmt
    % Sequence.

    --->    ml_stmt_block(
                list(mlds_local_var_defn),
                list(mlds_function_defn),
                list(mlds_stmt),
                prog_context
            )

    % Iteration.

    ;       ml_stmt_while(
                % Is this a "while (Cond) {...}" loop
                % or a "do {...} while (Cond)" loop?
                while_loop_kind,

                % The condition.
                mlds_rval,

                % The body.
                mlds_stmt,

                prog_context
            )

    % Selection (see also computed_goto).

    ;       ml_stmt_if_then_else(
                % The condition.
                mlds_rval,

                % The then part.
                mlds_stmt,

                % The else part (if any).
                maybe(mlds_stmt),

                prog_context
            )

    ;       ml_stmt_switch(
                % This representation for switches is very general:
                % it allows switching on any type, and cases can match
                % on ranges as well as on values.
                % Many target languages only allow switches on ints or enums.
                % Some (e.g. C#) also allow switches on strings.
                % Most target languages only allow matching on values;
                % only some (e.g. GNU C) allow matching on ranges.
                % The MLDS code generator should only generate switches
                % that the target will support.
                %
                % Note that unlike C, MLDS cases do NOT fall through; if you
                % want to achieve that effect, you need to use an explicit
                % goto.

                % The value to switch on.
                mlds_type,
                mlds_rval,

                % The range of possible values which the value might take
                % (if known).
                mlds_switch_range,

                % The different cases.
                list(mlds_switch_case),

                % What to do if none of the cases match.
                mlds_switch_default,

                prog_context
            )

    % Transfer of control.

    ;       ml_stmt_label(mlds_label, prog_context)
            % Defines a label that can be used as the target of calls,
            % gotos, etc.

    ;       ml_stmt_goto(mlds_goto_target, prog_context)
            % goto(Target): Branch to the specified address.

    ;       ml_stmt_computed_goto(
                % Evaluate rval, which should be an integer, and jump to the
                % (rval+1)th label in the list.
                % For example, computed_goto(2, [A, B, C, D])
                % will branch to label C.
                mlds_rval,
                list(mlds_label),
                prog_context
            )

    % Function call/return.

    ;       ml_stmt_call(
                % The signature of the callee function.
                mlds_func_signature,

                % The function to call.
                mlds_rval,

                % The ordinary function arguments.
                list(mlds_rval),

                % Th places to store the function return value(s).
                list(mlds_lval),

                % Is this call an ordinary call, a tail call,
                % or a no-return call?
                ml_call_kind,

                prog_context
            )

    ;       ml_stmt_return(
                % Some targets will not support returning more than one value.
                list(mlds_rval),
                prog_context
            )

    % Commits (a specialized form of exception handling).

    ;       ml_stmt_try_commit(mlds_lval, mlds_stmt, mlds_stmt, prog_context)
    ;       ml_stmt_do_commit(mlds_rval, prog_context)
            % try_commit(Ref, GoalToTry, CommitHandlerGoal, _Context):
            %   Execute GoalToTry. If GoalToTry exits via a `commit(Ref)'
            %   instruction, then execute CommitHandlerGoal.
            %
            % do_commit(Ref, _Context):
            %   Unwind the stack to the corresponding `try_commit'
            %   statement for Ref, and branch to the CommitHandlerGoal
            %   that was specified in that try_commit instruction.
            %
            % For both try_commit and do_commit instructions, Ref should be
            % the name of a local variable of type mlds_commit_type.
            % (This variable can be used by the back-end's implementation
            % of do_commit and try_commit to store information needed to unwind
            % the stack.) There should be exactly one try_commit instruction
            % for each Ref. do_commit(Ref) instructions should only be used
            % in goals called from the GoalToTry goal in the try_commit
            % instruction with the same Ref.
            %
            % The C back-end requires each try_commit to be put in its own
            % nested function, to avoid problems with setjmp() and local vars
            % not declared volatile. They also require each do_commit to be
            % put in its own function -- this is needed when using
            % __builtin_setjmp()/__builtin_longjmp() to ensure that the
            % call to __builtin_longjmp() is not in the same function
            % as the call to __builtin_setjmp().

    % Exception handling.
    %
    % We use C++-style exceptions.
    % For C, the back-end can simulate them using setjmp/longjmp.
    %
    % XXX This is tentative -- the current definition may be
    % a bit too specific to C++-style exceptions.
    % It might not be a good choice for different target languages.
    % XXX Full exception handling support is not yet implemented.

%   ;       ml_stmt_throw(mlds_type, mlds_rval, prog_context)
%           % Throw the specified exception.

%   ;       ml_stmt_rethrow(prog_context)
%           % Rethrow the current exception
%           % (only valid inside an exception handler).

%   ;       ml_stmt_try_catch(
%               mlds_stmt,
%               list(mlds_exception_handler),
%               prog_context
%           )
%           % Execute the specified statement, and if it throws an exception,
%           % and the exception matches any of the exception handlers,
%           % then execute the first matching exception handler.

    % Atomic statements.

    ;       ml_stmt_atomic(mlds_atomic_statement, prog_context).

:- inst ml_stmt_is_block for mlds_stmt/0
    --->    ml_stmt_block(ground, ground, ground, ground).
:- inst ml_stmt_is_while for mlds_stmt/0
    --->    ml_stmt_while(ground, ground, ground, ground).
:- inst ml_stmt_is_if_then_else for mlds_stmt/0
    --->    ml_stmt_if_then_else(ground, ground, ground, ground).
:- inst ml_stmt_is_switch for mlds_stmt/0
    --->    ml_stmt_switch(ground, ground, ground, ground, ground, ground).
:- inst ml_stmt_is_label for mlds_stmt/0
    --->    ml_stmt_label(ground, ground).
:- inst ml_stmt_is_goto for mlds_stmt/0
    --->    ml_stmt_goto(ground, ground).
:- inst ml_stmt_is_computed_goto for mlds_stmt/0
    --->    ml_stmt_computed_goto(ground, ground, ground).
:- inst ml_stmt_is_call for mlds_stmt/0
    --->    ml_stmt_call(ground, ground, ground, ground, ground, ground).
:- inst ml_stmt_is_return for mlds_stmt/0
    --->    ml_stmt_return(ground, ground).
:- inst ml_stmt_is_try_commit for mlds_stmt/0
    --->    ml_stmt_try_commit(ground, ground, ground, ground).
:- inst ml_stmt_is_do_commit for mlds_stmt/0
    --->    ml_stmt_do_commit(ground, ground).
:- inst ml_stmt_is_atomic for mlds_stmt/0
    --->    ml_stmt_atomic(ground, ground).

%---------------------------------------------------------------------------%
%
% Extra info for switches.
%

    % The range of possible values which the switch variable might take
    % (if known).
:- type mlds_switch_range
    --->    mlds_switch_range_unknown
    ;       mlds_switch_range(range_min::int, range_max::int).
            % From range_min to range_max, both inclusive.

    % Each switch case consists of the conditions to match against,
    % and the statement to execute if the match succeeds.
    % Unlike C, cases do NOT fall through; if you want to achieve that
    % effect, you need to use an explicit goto.
:- type mlds_switch_case
    --->    mlds_switch_case(
                % Each switch case contains one or more conditions.
                % If _any_ of the conditions match, this case will be selected.
                mlds_case_match_cond,
                list(mlds_case_match_cond),

                mlds_stmt
            ).

    % A case_match_cond specifies when a switch case will be selected
:- type mlds_case_match_cond
    --->    match_value(mlds_rval)
            % match_value(Val) matches if the switch value is equal to
            % the specified Val.

    ;       match_range(mlds_rval, mlds_rval).
            % match_range(Min, Max) matches if the switch value is between
            % Min and Max, both inclusive. Note that this should only be used
            % if the target supports it; currently the C back-end supports
            % this only if you are using the GNU C compiler.

    % The switch_default specifies what to do if none of the switch
    % conditions match.
:- type mlds_switch_default
    --->    default_is_unreachable
            % The switch is exhaustive, so the default case should
            % never be reached.

    ;       default_do_nothing
            % The default action is to just fall through to the statement
            % after the switch.

    ;       default_case(mlds_stmt).
            % The default is to execute the specified statement.

%---------------------------------------------------------------------------%
%
% Extra info for labels.
%

:- type mlds_label == string.

:- type mlds_goto_target
    --->    goto_label(mlds_label)
            % Branch to the specified label.

    ;       goto_break_switch
            % Branch to just after the end of the immediately enclosing switch,
            % just like a C/C++/Java `break' statement.
            % Not supported by all target languages.

    ;       goto_break_loop
            % Branch to just after the end of the immediately enclosing loop
            % just like a C/C++/Java `break' statement.
            % Not supported by all target languages.

    ;       goto_continue_loop.
            % Branch to the end of the loop body for the immediately enclosing
            % loop, just like a C/C++/Java/C# `continue' statement.
            % Not supported by all target languages.

%---------------------------------------------------------------------------%
%
% Extra info for calls.
%

    % The `ml_call_kind' type indicates whether a call is a tail call
    % and whether the call is known to never return.
    %
    % Marking a call as a tail_call is intended as a hint to the target
    % language compiler that this call can be implemented by removing
    % the caller's stack frame and jumping to the destination.
    % However, the target code generator need not obey this hint; it may
    % generate the same kind of code it would generate for a non-tail call.
    %
:- type ml_call_kind
    --->    no_return_call  % A call that never returns
                            % (this is a special case of a tail call).
    ;       tail_call       % A tail call.
    ;       ordinary_call.  % Just an ordinary call.

%---------------------------------------------------------------------------%
%
% Extra info for exception handling.
%

    % XXX This is tentative -- the current definition may be
    % a bit too specific to C++-style exceptions.
    % It might not be a good choice for different target languages.
    %
:- type mlds_exception_handler
    --->    handler(
                maybe(mlds_type),
                % If `yes(T)', specifies the type of exceptions to catch.
                % If `no', it means catch all exceptions.

                maybe(string)
                % If `yes(Name)', gives the variable name to use for the
                % exception value.
                % If `no', then exception value will not be used.
            ).

%---------------------------------------------------------------------------%
%
% Info for atomic statements.
%

:- type mlds_atomic_statement
    --->    comment(string)
            % A comment to put into the generated code. Should not contain
            % newlines.

    ;       assign(mlds_lval, mlds_rval)
            % assign(Location, Value):
            % Assign the value specified by rval to the location
            % specified by lval.

    ;       assign_if_in_heap(mlds_lval, mlds_rval)
            % assign_if_in_heap(Location, Value):
            % Assign the address specified by rval to the location specified
            % by lval if the address is in the bounds of the garbage collected
            % heap. Otherwise assign a null pointer to lval.

    % Heap management.

    ;       delete_object(mlds_rval)
            % Compile time garbage collect (i.e. explicitly deallocate)
            % the memory at the address given by the rval.

    ;       new_object(
                % new_object(Target, Tag, Type, Size, CtorName,
                %   ArgAndTypes, MayUseAtomic, MaybeAllocId):
                % Allocate a memory block of the given size,
                % initialize it with a new object of the given
                % type by calling the constructor with the specified
                % arguments, and put its address in the given lval,
                % possibly after tagging the address with a given tag.
                % (Some targets might not support tags.)

                % The target to assign the new object's address to.
                mlds_lval,

                % A (primary) tag to tag the address with before assigning
                % the result to the target.
                maybe(mlds_tag),

                % Indicates whether or not the first argument in the list (see
                % below) is a secondary tag. For target languages with
                % constructors, secondary tags are implicitly initialised by
                % the constructor, not passed in.
                bool,

                % The type of the object being allocated.
                mlds_type,

                % The amount of memory that needs to be allocated for the new
                % object, measured in words (NOT bytes!).
                maybe(mlds_rval),

                % The name of the constructor to invoke.
                maybe(qual_ctor_id),

                % The arguments to the constructor, and their types.
                % Any arguments which are supposed to be packed together should
                % be packed in this list by the HLDS->MLDS code generator.
                %
                % For boxed fields, the types here should be mlds_generic_type;
                % it is the responsibility of the HLDS->MLDS code generator to
                % insert code to box/unbox the arguments.
                list(mlds_typed_rval),

                % Can we use a cell allocated with GC_malloc_atomic to hold
                % this object in the C backend?
                may_use_atomic_alloc,

                % The allocation site identifier.
                maybe(mlds_alloc_id)
            )

    ;       gc_check
            % Check to see if we need to do a garbage collection,
            % and if so, do it.
            % This is used for accurate garbage collection
            % with the MLDS->C back-end.

    ;       mark_hp(mlds_lval)
            % Tell the heap sub-system to store a marker (for later use in
            % restore_hp/1 instructions) in the specified lval.
            %
            % It is OK for the target to treat this as a no-op, and probably
            % that is what most targets will do.

    ;       restore_hp(mlds_rval)
            % The rval must be a marker as returned by mark_hp/1.
            % The effect is to deallocate all the memory which
            % was allocated since that call to mark_hp.
            %
            % It is OK for the target to treat this as a no-op,
            % and probably that is what most targets will do.

    % Trail management.

    ;       trail_op(trail_op)

    % Foreign language interfacing.

    ;       inline_target_code(mlds_target_lang, list(target_code_component))
            % Do whatever is specified by the target_code_components, which
            % can be any piece of code in the specified target language (C,
            % assembler, or whatever) that does not have any non-local flow
            % of control. This is implemented by embedding the target code
            % in the output stream of instructions or statements.

    ;       outline_foreign_proc(
                % Do whatever is specified by the string, which can be any
                % piece of code in the specified foreign language (C#,
                % managed C++, or whatever). This is implemented by calling
                % an externally defined function, which the backend must
                % generate the definition for (in some other file perhaps)
                % and calling it. The lvals are use to generate the appropriate
                % forwarding code.
                % XXX We should also store the list of mlds_rvals where
                % the input values come from.

                % The foreign language this code is written in.
                foreign_language,

                list(outline_arg),

                % Where to store return value(s).
                list(mlds_lval),

                % The user's foreign language code fragment.
                string
            ).

:- inst atomic_stmt_is_new_object for mlds_atomic_statement/0
    --->    new_object(ground, ground, ground, ground, ground, ground,
                ground, ground, ground).

    % Stores information about each argument to an outline_foreign_proc.
    %
:- type outline_arg
    --->    ola_in(
                mlds_type,      % The type of the argument.
                string,         % The name of the argument in the foreign code.
                mlds_rval       % The rval which holds the value of this
                                % argument.
            )
    ;       ola_out(
                mlds_type,      % The type of the argument.
                string,         % The name of the argument in the foreign code.
                mlds_lval       % The lval where we are to place the result
                                % calculated by the foreign code into.
            )
    ;       ola_unused.

:- type mlds_target_lang
    --->    ml_target_c
    ;       ml_target_csharp
    ;       ml_target_java.

    % This is just a random selection of possible languages
    % that we might want to target later ...
%   ;       ml_target_gnu_c
%   ;       ml_target_c_minus_minus
%   ;       ml_target_java_asm
%   ;       ml_target_java_bytecode.

:- type target_code_component
    --->    user_target_code(
                % User_target_code holds C code from the user's
                % `pragma foreign_proc' declaration.

                string,
                maybe(prog_context)
            )
    ;       raw_target_code(
                % Raw_target_code holds C code that the compiler has generated.
                % To ensure that following `#line' directives work OK, either
                % the string in a raw_target_code must end in `\n' (or `\n'
                % followed by whitespace), or the following
                % target_code_component must be a `name(Name)' component,
                % for which we do not output #line directives.

                string
            )

    ;       target_code_input(mlds_rval)
    ;       target_code_output(mlds_lval)
    ;       target_code_type(mlds_type)
    ;       target_code_function_name(qual_function_name)
    ;       target_code_alloc_id(mlds_alloc_id).

:- type mlds_alloc_id
    --->    mlds_alloc_id(int).

    % Constructor id.
    %
:- type mlds_ctor_id
    --->    ctor_id(mlds_class_name, arity).
:- type qual_ctor_id
    --->    qual_ctor_id(mlds_module_name, mlds_qual_kind, mlds_ctor_id).

    % Trail management.
    % For documentation, see the corresponding LLDS instructions in llds.m.
    %
:- type trail_op
    --->    store_ticket(mlds_lval)
    ;       reset_ticket(mlds_rval, mlds_reset_trail_reason)
    ;       discard_ticket
    ;       prune_ticket
    ;       mark_ticket_stack(mlds_lval)
    ;       prune_tickets_to(mlds_rval).
%   ;       discard_tickets_to(mlds_rval).  % used only by the library

% Note: the types `reset_trail_reason' and `tag' here are both defined
% exactly the same as the ones in llds.m. The definitions are duplicated here
% because we don't want mlds.m to depend on llds.m. (Alternatively, we could
% move both these definitions into a new module in the backend_libs.m package,
% but these definitions are small enough and simple enough that it is probably
% not worth creating a new module just for them.)

    % See runtime/mercury_trail.h.
    %
:- type mlds_reset_trail_reason
    --->    undo
    ;       commit
    ;       solve
    ;       exception
    ;       gc.

    % A tag should be a small non-negative integer.
    %
:- type mlds_tag == int.

%---------------------------------------------------------------------------%
%
% Lvals.
%

    % An lval represents a data location or variable that can be used
    % as the target of an assignment.
    %
:- type mlds_lval

    % Values on the heap or fields of a structure.

    --->    ml_field(
                % field(Tag, Address, FieldId, FieldType, PtrType):
                % Selects a field of a compound term.

                % Address is a tagged pointer to a cell on the heap.
                % The position in the cell, FieldId, is represented either
                % as a field name or a number of words offset. If Tag is yes,
                % the arg gives the value of the tag; if it is no, the tag bits
                % will have to be masked off. The value of the tag should be
                % given if it is known, since this will lead to faster code.
                % The FieldType is the type of the field. The PtrType is the
                % type of the pointer from which we are fetching the field.
                %
                % For boxed fields, the type here should be mlds_generic_type,
                % not the actual type of the field. If the actual type is
                % different, then it is the HLDS->MLDS code generator's
                % responsibility to insert the necessary code to handle
                % boxing/unboxing.

                field_tag       :: maybe(mlds_tag),
                field_addr      :: mlds_rval,
                field_field_id  :: mlds_field_id,
                field_type      :: mlds_type,
                field_ptr_type  :: mlds_type
            )

    % Values somewhere in memory.
    % This is the dereference operator (e.g. unary `*' in C).

    ;       ml_mem_ref(
                % The rval should have originally come from a mem_addr rval.
                % The type is the type of the value being dereferenced,
                % i.e. the type of the pointed-to value.

                mlds_rval,
                mlds_type
            )

    ;       ml_target_global_var_ref(
                % A reference to the value of the global variable in the target
                % language with the given name. At least for now, the global
                % variable's type must be mlds_generic_type.
                %
                % XXX This functionality is currently only supported for
                % the C backend.

                global_var_ref
            )

    % Variables.

    ;       ml_local_var(
                mlds_local_var_name,
                mlds_type
            )

    ;       ml_global_var(
                qual_global_var_name,
                mlds_type
            ).

    % An mlds_field_id represents some data within an object.
    %
:- type mlds_field_id
    --->    ml_field_offset(mlds_rval)
            % offset(N) represents the field at offset N Words.

    ;       ml_field_named(
                % ml_field_named(Name, MaybeCtorType) represents the field with
                % the specified name.
                %
                % CtorType gives the MLDS type for this particular constructor.
                % The type of the object is given by the PtrType in the
                % field(..) lval. CtorType may either be the same as PtrType,
                % or it may be a pointer to a derived class. In the latter
                % case, the MLDS->target code back-end is responsible for
                % inserting a downcast from PtrType to CtorType before
                % accessing the field.

                qual_field_var_name,
                mlds_type
            ).

:- type global_var_ref
    --->    env_var_ref(string).

%---------------------------------------------------------------------------%
%
% Rvals.
%

:- type mlds_typed_rval
    --->    ml_typed_rval(mlds_rval, mlds_type).

    % An rval is an expression that represents a value.
    %
:- type mlds_rval
    --->    ml_lval(mlds_lval)
            % The value of an `lval' rval is just the value stored in
            % the specified lval.

    ;       ml_mkword(mlds_tag, mlds_rval)
            % Given a pointer and a tag, mkword returns a tagged pointer.
            %
            % In theory, we could make `mkword' a binary_op, but this
            % representation is tighther: it makes clear that the tag
            % we want to put on is *always* a constant.

    ;       ml_const(mlds_rval_const)

    ;       ml_unop(mlds_unary_op, mlds_rval)

    ;       ml_binop(binary_op, mlds_rval, mlds_rval)

    ;       ml_mem_addr(mlds_lval)
            % The address of a variable, etc.

    ;       ml_scalar_common(mlds_scalar_common)
            % A reference to the given common structure. The reference is NOT
            % the address; that is ml_scalar_common_addr.
            %
            % This rval is intended to be used as the name of an array
            % to be indexed into. This is possible because the elements
            % of a scalar common cell are all boxed and thus of the same size.

    ;       ml_scalar_common_addr(mlds_scalar_common)
            % The address of the ml_scalar_common(...) for the same common.

    ;       ml_vector_common_row_addr(mlds_vector_common, mlds_rval)
            % ml_vector_common_row_addr(VectorCommon, Index),
            % The address of the given row (selected by the second argument)
            % of the given common structure. If VectorCommon is equal e.g.
            % to ml_vector_common(ModuleName, Type, TypeNum, StartRow, NumRows)
            % then Index must be between 0 and NumRows-1 (both inclusive),
            % and the reference will be row StartRow + Index in the 2D table
            % represented by the 2D table holding all the vector static data of
            % this type.
            %
            % The selected row should be a struct holding unboxed values,
            % which can be retrieved using field names (not field offsets).

    ;       ml_self(mlds_type).
            % The equivalent of the `this' pointer in C++ with the type of the
            % object. Note that this rval is valid iff we are targeting an
            % object oriented backend and we are in an instance method
            % (procedures which have the per_instance flag set).

:- type mlds_rval_const
    --->    mlconst_true
    ;       mlconst_false
    ;       mlconst_int(int)
    ;       mlconst_uint(uint)
    ;       mlconst_int8(int8)
    ;       mlconst_uint8(uint8)
    ;       mlconst_int16(int16)
    ;       mlconst_uint16(uint16)
    ;       mlconst_int32(int32)
    ;       mlconst_uint32(uint32)
    ;       mlconst_int64(int64)
    ;       mlconst_uint64(uint64)
    ;       mlconst_enum(int, mlds_type)
    ;       mlconst_char(int)
    ;       mlconst_float(float)
    ;       mlconst_string(string)
    ;       mlconst_multi_string(list(string))
            % A multi_string_const is a string containing embedded NULs
            % between each substring in the list.

    ;       mlconst_foreign(foreign_language, string, mlds_type)

    ;       mlconst_named_const(target_prefixes, string)
            % mlconst_named_const(TargetPrefixes, ConstName) represents
            % an enum constant in the runtime system whose name in the target
            % language is ConstName prefixed by
            %
            % - nothing, if the target language is C,
            % - TargetPrefixes ^ java_prefix, if the target language is Java,
            % - TargetPrefixes ^ charp_prefix, if the target language is C#.
            %
            % It is the responsibility of the code that creates named constants
            % to ensure that the constant's name is meaningful for the target
            % language. This includes making sure that the name is acceptable
            % as an identifier, does not need quoting, and the constant it
            % refers to is accessible.

    ;       mlconst_code_addr(mlds_code_addr)

    ;       mlconst_data_addr_local_var(mlds_local_var_name)
            % The address of a local variable.

    ;       mlconst_data_addr_global_var(mlds_module_name,
                mlds_global_var_name)
            % The address of a global variable.

    ;       mlconst_data_addr_rtti(mlds_module_name, rtti_id)
            % The address of an RTTI data structure.

    ;       mlconst_data_addr_tabling(qual_proc_label,
                proc_tabling_struct_id)
            % The address of the given tabling data structure
            % for the given procedure.

    ;       mlconst_null(mlds_type).
            % A null value, of the given type. Usually the type will be a
            % pointer (mlds_ptr_type) but it could also be string or a
            % func_type. (Null is not a valid value of type string or
            % func_type, but null values of those types may be useful as
            % placeholders in cases where the value will never be used.)

:- type mlds_code_addr
    --->    mlds_code_addr(
                qual_func_label,
                mlds_func_signature
            ).

:- type mlds_unary_op
    --->    box(mlds_type)
            % box(MLDSType); convert from MLDSType to mlds_generic_type,
            % by boxing if necessary, or just casting if not.

    ;       unbox(mlds_type)
            % unbox(MLDSType): convert from mlds_generic_type to MLDSType,
            % applying the inverse transformation to box/1, i.e. unboxing
            % if boxing was necessary, and just casting otherwise.

    ;       cast(mlds_type)
            % cast(MLDSType): Coerce the type of the rval to be MLDSType.
            % XXX It might be worthwhile adding the type that we cast from.

    ;       std_unop(builtin_ops.unary_op).

:- type ml_scalar_common_type_num
    --->    ml_scalar_common_type_num(int).
:- type ml_vector_common_type_num
    --->    ml_vector_common_type_num(int).

:- type mlds_scalar_common
    --->    ml_scalar_common(mlds_module_name, mlds_type,
                ml_scalar_common_type_num, int).
            % module name, type, type number, row number

:- type mlds_vector_common
    --->    ml_vector_common(mlds_module_name, mlds_type,
                ml_vector_common_type_num, int, int).
            % module name, type, type number,
            % starting row number, number of rows

%---------------------------------------------------------------------------%
%
% Foreign code interfacing.
%

    % Foreign code required for the foreign language interface.
    % When compiling to a language other than the foreign language,
    % this part still needs to be generated as C (or whatever) code
    % and compiled with a C (or whatever) compiler.
    %
:- type mlds_foreign_code
    --->    mlds_foreign_code(
                list(foreign_decl_code),
                list(foreign_body_code),
                list(foreign_import_module_info),
                list(mlds_pragma_export)
            ).

    % Information required to generate code for each `pragma foreign_export'.
    %
:- type mlds_pragma_export
    --->    ml_pragma_export(
                foreign_language,
                string,                         % Exported name
                qual_function_name,             % MLDS name for exported entity
                mlds_func_params,               % MLDS function parameters
                list(tvar),                     % Universally quantified type
                                                % variables.
                prog_context
            ).

%---------------------%

:- type mlds_exported_enums == list(mlds_exported_enum).

:- type mlds_exported_enum
    --->    mlds_exported_enum(
                exported_enum_lang      :: foreign_language,
                exported_enum_context   :: prog_context,

                % mlds_to_cs.m and mlds_to_java.m need to know the type_ctor.
                exported_enum_type_ctor :: type_ctor,

                % The name of each constant plus the value to initialize it to.
                exported_enum_constants :: list(mlds_exported_enum_constant)
            ).

:- type mlds_exported_enum_constant
    --->    mlds_exported_enum_constant(
                exported_enum_constant_name     :: string,
                exported_enum_constant_value    :: mlds_initializer
            ).

%---------------------------------------------------------------------------%
%
% Global variable names.
%

    % XXX Some global constants need to be visible throughout the program,
    % including outside the module that defines them, and therefore their
    % names need to be module qualified. However, some global variables and
    % constants need not be (and therefore *should* not be) visible outside
    % their modules, and these should *not* need to be module qualified.
    % Removing the module qualification from such variables would make
    % the generated code considerably less cluttered and therefore
    % easier to read.
    %
:- type qual_global_var_name
    --->    qual_global_var_name(mlds_module_name, mlds_global_var_name).
            % Global variables can only ever be module qualified.

:- type mlds_global_var_name
    --->    gvn_rtti_var(rtti_id)

    ;       gvn_tabling_var(mlds_proc_label, proc_tabling_struct_id)
            % A variable that contains a pointer that points to the table
            % used to implement memoization, loopcheck or minimal model
            % semantics for the given procedure.

    ;       gvn_const_var(mlds_global_const_var, int)
            % These MLDS variables are global variables holding constant,
            % immutable data. What kind of data is given by the first argument.
            % The integer is a sequence number (unique within the whole module)
            % allocated from a counter that is shared between all
            % gvn_const_vars.

    ;       gvn_dummy_var.
            % A reference to private_builtin.dummy_var.

:- type mlds_global_const_var
    --->    mgcv_const_var
    ;       mgcv_float
    ;       mgcv_int64
    ;       mgcv_uint64
    ;       mgcv_closure_layout
    ;       mgcv_typevar_vector
    ;       mgcv_bit_vector.

%---------------------------------------------------------------------------%
%
% Field variable names.
%

    % An mlds_field_var represents a variable or constant that is defined
    % as a field (or member) of a class.
    %
    % XXX Some classes, and therefore their public fields, need to be visible
    % throughout the program, including outside the module that defines them,
    % and therefore their names need to be module qualified. However,
    % fields in classes that are local to their modules should *not* need
    % to be module qualified. Removing the module qualification from
    % such field variables would make the generated code less cluttered
    % and therefore easier to read.
    %
:- type qual_field_var_name
    --->    qual_field_var_name(mlds_module_name, mlds_qual_kind,
                mlds_field_var_name).
            % Some field variables are module qualified, while others
            % are type qualified.

:- type mlds_field_var_name
    --->    fvn_global_data_field(int, int)
            % When implementing lookup switches (and some related HLDS
            % constructs, such as lookup disjunctions, which are like
            % lookup switches without the switch), the compiler generates
            % a vector of rows, with each row holding one solution
            % generated by the switch (or the disjunction).
            % We store all solutions that contain the same vector of types
            % in the same static (immutable) global variable. The type
            % of this variable is thus an array of structures, with the
            % types of the field being given by the types of the variables that
            % comprise the outputs of the HLDS construct being implemented.
            %
            % The two arguments of fvn_global_data_field are the type number
            % and the field number. The type number uniquely identifies
            % the list of field types in each row (among all the lists of
            % field types for which we generate static arrays of rows),
            % while the second just gives the position of the field
            % in that list. Field numbers start at 0.

    ;       fvn_reserved_obj_name(string, int)
            % This field_var is the specially reserved static class member
            % variable whose address is used to represent the function symbol
            % with the given name and arity.

    ;       fvn_du_ctor_field_hld(string)
            % When compiling with --high-level-data, we generate a type
            % in the target language for each data constructor in a
            % discriminated union type. This is the name of one of the fields
            % of this type.
            % NOTE: See the XXX on the code that turns these variables
            % into strings.

    ;       fvn_mr_value
    ;       fvn_data_tag
    ;       fvn_enum_const(string)
    ;       fvn_sectag_const(string)
            % These represent member variables in the classes we generate
            % with --high-level-data for discriminated union types
            % (both enum and non-enum). I (zs) don't know exactly what
            % they are used for.
            % NOTE: See the XXX on the code that turns some these
            % MLDS variables into strings.

    ;       fvn_base_class(int)
            % This variable name is used in the implementation of base classes
            % in the C backend, but I (zs) don't know exactly what it is
            % used for.

    ;       fvn_ptr_num
            % This variable is used by the Java backend.
            % I (zs) don't know what its semantics is.

    ;       fvn_env_field_from_local_var(mlds_local_var_name)
            % This is a field of an environment structure that represents
            % the given local variable.

    ;       fvn_prev
    ;       fvn_trace.
            % These MLDS variables represent two fixed fields in the frames
            % that hold variables when we compiler for accurate gc. fvn_prev
            % points to the previous frame (if any) in the chain of frames,
            % while fvn_trace points to the function that traces the contents
            % of the current frame.

%---------------------------------------------------------------------------%
%
% Local variable names.
%

    % An mlds_local_var represents a variable that is either a parameter
    % of a function, or is defined inside a block of a function.
    %
:- type mlds_local_var_name
    --->    lvn_prog_var(string, int)
            % This MLDS variable represents a variable in the HLDS procedure
            % being translated to MLDS. The string gives its name
            % (if it is unnamed, the string will be ""), and the int gives
            % its HLDS variable number.

    ;       lvn_prog_var_foreign(string)
            % This MLDS variable represents a HLDS variable. It does not
            % have the usual numeric suffix derived from its HLDS variable
            % number because it is one of the arguments of a foreign_proc
            % goal. Its name in the target language must be exactly the same
            % as the name written by the programmer, since that is the name
            % by which the foreign language code will refer to it.

    ;       lvn_prog_var_boxed(string, int)
            % This MLDS variable represents the boxed version of the HLDS
            % variable with the given name and number.

    ;       lvn_prog_var_conv(int, string, int)
            % This MLDS variable represents a version of a HLDS variable
            % (of the name and number given by the second and third args)
            % created by a boxing or unboxing operation. The first integer
            % is a sequence number that uniquely identifies the operation
            % within the MLDS code generated for a Mercury procedure.
            % XXX Why is lvn_prog_var_boxed separate from this?

    ;       lvn_prog_var_next_value(string, int)
            % This MLDS variable represents the next value to be assigned
            % to the HLDS variable with the given name and number.
            %
            % The compiler must generate such temporaries when generating code
            % for tail calls that switch input arguments. For example,
            % if a procedure with head p(X, Y, ...) contains a tail call
            % p(Y, X, ...), then having the tail call assign the first
            % two arguments as X := Y, Y := X would not work, since
            % the first assignment would clobber X. The compiler therefore
            % generates next_value_of_X := Y, next_value_of_Y := X,
            % followed by X := next_value_of_X, Y := next_value_of_Y.

    ;       lvn_local_var(string, int)
            % This MLDS variable represents a local copy of a HLDS variable
            % (with the given name and number) that is an output variable
            % of a commit scope. With --nondet-copy-out, the code inside
            % the scope that produces the HLDS variable assigns its value
            % to the local copy; this value is then copied to the actual
            % lvn_prog_var representing the HLDS variable when the scope
            % succeeds.

    ;       lvn_tscc_proc_input_var(proc_id_in_tscc, int, string)
    ;       lvn_tscc_output_var(int, string)
    ;       lvn_tscc_output_var_ptr(int, string)
    ;       lvn_tscc_output_var_succeeded
            % All four of these are used when we generate code for
            % a set of mutually *tail* recursive procedures.
            % (Such procedures form a TSCC, i.e. an SCC of a dependency
            % graph formed only from tail calls.)
            %
            % Our general scheme for optimizing mutually recursive tail calls
            % in TSCCs is described in notes/mlds_tail_recursion.html.
            % Here we just document the meanings of these MLDS variables.
            %
            % lvn_tscc_proc_input_var(ProcIdInTscc, ArgNum, VarName) represents
            % one of the input arguments of one of the procedures in a TSCC
            % (tail-call SCC); the ProcIdInTscc and ArgNum say which.
            % The VarName is the output of ml_local_var_name_to_string
            % on the name of the actual variable representing that argument.
            %
            % lvn_tscc_output_var(ArgNum, VarName) represents one of the
            % output arguments of a TSCC (tail-call SCC); the ArgNum say which.
            % Note that while each procedure in a TSCC has its own list
            % of inputs arguments, all procedures in a TSCC, by definition,
            % return a list of output arguments that is identical in everything
            % except possibly name. This allows us to use a single variable
            % to represent e.g. output arg N of *every* procedure in the TSCC;
            % This is why lvn_tscc_output_vars don't have a ProcIdInTscc field.
            % (The VarName recorded in a lvn_tscc_output_var is the name of
            % the output argument in *one* of the procedures in the TSCC.)
            %
            % An lvn_tscc_output_var will contain the value of the
            % corresponding output argument. For arguments passed by
            % reference, we need a way to store the address passed to us
            % by the caller as the address at which the value being output
            % should be put. An lvn_tscc_output_var_ptr contains this address.
            %
            % Model semi procedures implicitly have an extra output argument:
            % the procedure's success/failure indicator. We store this
            % indicator in a lvn_tscc_output_var_succeeded variable,
            % which functions as a special kind of lvn_tscc_output_var.
            % The success indicator is always returned by value, so we
            % don't need a lvn_tscc_output_var_succeeded_ptr variable
            % in the MLDS.

    ;       lvn_field_var_as_local(mlds_field_var_name)
            % When we (specifically, ml_elim_nested.m) create continuation
            % functions, we give them the values of the local variables
            % as fields of an environment structure. To take the values
            % stored there *out* of the environment structure, we refer
            % to them as if they were local variables, using this construct.

    ;       lvn_comp_var(mlds_compiler_var).
            % This MLDS variable was created by the compiler as part of
            % the translation process; it does NOT correspond to a variable
            % in the HLDS.

    % Identifies a procedure in a TSCC (an SCC that is computed by taking
    % only tail calls into account.
    %
    % TSCCs are typically small, most containing only two or three procedures.
    % The ids are sequentially allocated small integers starting at 1.
    %
:- type proc_id_in_tscc
    --->    proc_id_in_tscc(int).

:- inst lvn_prog_var for mlds_local_var_name/0
    --->    lvn_prog_var(ground, ground).

:- type mlds_compiler_var
    --->    lvnc_non_prog_var_boxed(string)
    ;       lvnc_non_prog_var_conv(int, string)
    ;       lvnc_non_prog_var_next_value(string)
            % What the lvn_prog_var_boxed, lvn_prog_var_conv, and
            % lvn_prog_var_next_value are to lvn_prog_vars, these are
            % to lvn_comp_vars. The string is the output of
            % ml_local_var_name_to_string of the original lvn_comp_var.

    ;       lvnc_succeeded
            % The MLDS variable indicating whether a compiler generated
            % semidet goal has succeeded.

    ;       lvnc_success_indicator
            % The MLDS variable representing the target language variable
            % SUCCESS_INDICATOR used by hand-written code in semidet
            % foreign_procs to indicate success or failure.

    ;       lvnc_tscc_proc_selector
            % The MLDS variable that says which member of the TSCC
            % a (self- or) mutually recursive tail call is targeting.
            % Used when the body of each procedure in the TSCC is made
            % one case of a switch on lvnc_tscc_proc_selector, which in turn
            % is the body of a "while (true)" loop. Tail recursive calls
            % set lvnc_tscc_proc_selector to select the target procedure,
            % set up its input arguments, and "continue" to the next iteration
            % of the loop.

    ;       lvnc_new_obj(int)
            % A temporary variable holding the address of a newly allocated
            % object, used by accurate gc. The integer is a unique sequence
            % number allocated from a per-procedure counter that is dedicated
            % for this purpose.

    ;       lvnc_cond(int)
            % This MLDS variable records whether the condition of a model_non
            % if-then-else has ever succeeded. The integer is a sequence number
            % allocated from a counter that is dedicated for this purpose.

    ;       lvnc_conv_var(int)
            % This MLDS variable holds the address of the callee of a generic
            % call (according to the comment creating it, we need a variable
            % to hold the address to work around limitations in old C
            % compilers). The integer is a sequence number allocated from
            % a counter that is dedicated for this purpose.

    ;       lvnc_arg(int)
            % This MLDS variable represents the Nth output parameter of
            % a success continuation, where N is the given integer.

    ;       lvnc_wrapper_arg(int)
            % This MLDS variable represents the Nth parameter of the wrapper
            % function around a procedure, where N is the given integer.
            % We put a wrapper function around procedures when they are used
            % as the code in a closure.

    ;       lvnc_param(int)
            % This MLDS variable represents the Nth parameter of an MLDS
            % function, where N is the given integer, when the declaration
            % of that function is standardized for printing in an automatically
            % generated header file. This standardization avoids the need
            % to regenerate every file that depends on that header file
            % when the actual names of the corresponding procedure's arguments
            % change in the HLDS.

    ;       lvnc_out_param(int)
            % This MLDS variable represents the Nth putput parameter of an MLDS
            % function being compiled to C#, where N is the given integer,
            % and N > 1. (The C# backend returns the first output parameter
            % as the return value, but returns the others as the "out"
            % parameters represented by these MLDS variables.

    ;       lvnc_return_value
            % This MLDS variable represents the return value(s) of method
            % created by the Java backend.
            % XXX document me in more detail.

    ;       lvnc_closure
    ;       lvnc_closure_arg
            % These two MLDS variables respectively represent the unboxed
            % and the boxed versions of a closure.

    ;       lvnc_closure_layout_ptr
            % This MLDS variable holds a pointer to a closure's
            % MR_Closure_Layout structure. This is used for accurate gc.

    ;       lvnc_type_params
            % This MLDS variable holds a pointer to a closure's materialized
            % type_infos. This is used for accurate gc.

    ;       lvnc_type_info
            % This MLDS variable holds a pointer to a materialized type_info.
            % This is used for accurate gc.

    ;       lvnc_cont
    ;       lvnc_cont_env_ptr
            % These MLDS variables hold respectively a nondet continuation,
            % and the pointer to the environment containing the variables
            % for the continuation.

    ;       lvnc_env
    ;       lvnc_env_ptr
            % These MLDS variables refer to environments needed to simulate
            % nested functions in targets that don't support them. The lvnc_env
            % variable is the environment structure itself, while lvnc_env_ptr
            % is its address.

    ;       lvnc_env_ptr_arg
            % This MLDS variable, like lvnc_env_ptr, points to an environment
            % structure, but its type is generic; it is not specific to the
            % structure of the environment it actually points to. Therefore
            % lvnc_env_ptr_args are cast to lvnc_env_ptrs before use.

    ;       lvnc_frame
    ;       lvnc_frame_ptr
            % These MLDS variables refer to the frames that hold variables
            % when we compile code for accurate gc. The lvnc_frame variable
            % is the frame structure itself, while lvnc_frame_ptr
            % is its address.

    ;       lvnc_this_frame
            % This MLDS variable, like lvnc_frame_ptr, points to a frame
            % that holds variables when we compile for accurate gc, but
            % its type is generic; it is not specific to the structure
            % of the frame it actually points to. Therefore lvnc_this_frames
            % are cast to lvnc_frame_ptrs before use.

    ;       lvnc_stack_chain
            % This MLDS variable represents the global variable that holds
            % the pointer to the current chain of frames.

    ;       lvnc_saved_stack_chain(int)
            % This MLDS variable represents a locally-saved copy of the
            % global variable represented by lvnc_stack_chain.

    ;       lvnc_args
            % This variable is used by the Java backend.
            % I (zs) don't know what its semantics is.

    ;       lvnc_aux_var(mlds_compiler_aux_var, int).
            % These MLDS variables contain values (most, but not all of which
            % are very short-lived) used to implement Mercury constructs
            % such as commits and switches. They contain such information
            % as the boundaries of a binary search or slot numbers in the
            % hash tables used to implement switches.
            % The integer is a sequence number (unique within the procedure)
            % allocated from a counter that is shared between all
            % lvnc_aux_vars.

:- type mlds_compiler_aux_var
    --->    mcav_commit
    ;       mcav_slot
    ;       mcav_later_slot
    ;       mcav_num_later_solns
    ;       mcav_limit
    ;       mcav_str
    ;       mcav_lo
    ;       mcav_mid
    ;       mcav_hi
    ;       mcav_stop_loop
    ;       mcav_result
    ;       mcav_case_num.

%---------------------------------------------------------------------------%

:- func ml_global_const_var_name_to_string(mlds_global_const_var, int)
    = string.
:- func ml_field_var_name_to_string(mlds_field_var_name) = string.
:- func ml_local_var_name_to_string(mlds_local_var_name) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_names.
:- import_module parse_tree.java_names.

:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

mlds_get_module_name(MLDS) = MLDS ^ mlds_name.

% For Java:
% The "package_name" is really the name of the Mercury module, and the
% "module_name" is the "package_name" plus additional qualifiers (if any).
% For example, a type `c' in module `a.b' would have package_name == `a.b'
% and module_name == `a.b.c'.
% XXX clean this up
%
% Note that modules in the Mercury standard library map get a `mercury'
% prefix e.g. `mercury.builtin', `mercury.io', `mercury.univ', etc.,
% when mapped to MLDS package names.

:- type mlds_module_name
    --->    mlds_module_name(
                mmn_package_name    :: mdbcomp.sym_name.module_name,
                mmn_module_name     :: mdbcomp.sym_name.module_name
            ).

mercury_module_name_to_mlds(MercuryModule) = Name :-
    ( if mercury_std_library_module_name(MercuryModule) then
        MLDS_Package = add_outermost_qualifier("mercury", MercuryModule)
    else
        MLDS_Package = MercuryModule
    ),
    Name = mlds_module_name(MLDS_Package, MLDS_Package).

mercury_module_and_package_name_to_mlds(MLDS_Package, MercuryModule)
    = mlds_module_name(MLDS_Package, MercuryModule).

mlds_module_name_to_sym_name(Module) = Module ^ mmn_module_name.

mlds_module_name_to_package_name(Module) = Module ^ mmn_package_name.

is_std_lib_module(Module, Name) :-
    Name0 = Module ^ mmn_module_name,
    strip_outermost_qualifier(Name0, "mercury", Name),
    mercury_std_library_module_name(Name).

%---------------------------------------------------------------------------%

mlds_append_class_qualifier(Target, ModuleName0, QualKind,
        ClassName, ClassArity) = ModuleName :-
    ModuleName0 = mlds_module_name(Package, Module0),
    % For the Java back-end, we flip the initial case of an type qualifiers,
    % in order to match the usual Java conventions.
    ( if
        Target = ml_target_java,
        QualKind = type_qual
    then
        AdjustedModule0 = flip_initial_case_of_final_part(Module0)
    else
        AdjustedModule0 = Module0
    ),
    ModuleName = mlds_append_class_qualifier_base(Package, AdjustedModule0,
        ClassName, ClassArity).

mlds_append_class_qualifier_module_qual(ModuleName0, ClassName, ClassArity)
        = ModuleName :-
    ModuleName0 = mlds_module_name(Package, Module0),
    ModuleName = mlds_append_class_qualifier_base(Package, Module0,
        ClassName, ClassArity).

:- func mlds_append_class_qualifier_base(module_name, module_name,
    mlds_class_name, arity) = mlds_module_name.
:- pragma inline(mlds_append_class_qualifier_base/4).

mlds_append_class_qualifier_base(Package, Module, ClassName, ClassArity)
        = ModuleName :-
    string.format("%s_%d", [s(ClassName), i(ClassArity)], ClassQualifier),
    ModuleName = mlds_module_name(Package, qualified(Module, ClassQualifier)).

mlds_append_name(mlds_module_name(Package, Module), Name)
    = mlds_module_name(Package, qualified(Module, Name)).

%---------------------------------------------------------------------------%

global_dummy_var = DummyVar :-
    MLDS_ModuleName =
        mercury_module_name_to_mlds(mercury_private_builtin_module),
    DummyVar = qual_global_var_name(MLDS_ModuleName, gvn_dummy_var).

%---------------------------------------------------------------------------%

get_initializer_array_size(no_initializer) = no_size.
get_initializer_array_size(init_obj(_)) = no_size.
get_initializer_array_size(init_struct(_, _)) = no_size.
get_initializer_array_size(init_array(Elems)) = array_size(list.length(Elems)).

%---------------------------------------------------------------------------%

mlds_std_tabling_proc_label(ProcLabel0) = ProcLabel :-
    % We standardize the parts of PredLabel0 that aren't computable from
    % the tabling pragma, because the code that creates the reset predicate
    % in table_info_global_var_name in add_pragma.m doesn't have access to
    % this information.
    ProcLabel0 = mlds_proc_label(PredLabel0, ProcId),
    (
        PredLabel0 = mlds_user_pred_label(PorF, MaybeModuleName, Name,
            Arity, _, _),
        PredLabel = mlds_user_pred_label(PorF, MaybeModuleName, Name,
            Arity, model_det, no)
    ;
        PredLabel0 = mlds_special_pred_label(_, _, _, _),
        unexpected($module, $pred, "mlds_special_pred_label")
    ),
    ProcLabel = mlds_proc_label(PredLabel, ProcId).

%---------------------------------------------------------------------------%

mlds_get_arg_types(Parameters) = ArgTypes :-
    GetArgType = (func(mlds_argument(_, Type, _)) = Type),
    ArgTypes = list.map(GetArgType, Parameters).

mlds_get_func_signature(Params) = Signature :-
    Params = mlds_func_params(Parameters, RetTypes),
    ParamTypes = mlds_get_arg_types(Parameters),
    Signature = mlds_func_signature(ParamTypes, RetTypes).

:- func foreign_type_to_mlds_type(module_info, foreign_type_body) = mlds_type.

foreign_type_to_mlds_type(ModuleInfo, ForeignTypeBody) = MLDSType :-
    % The body of this function is very similar to the function
    % foreign_type_body_to_exported_type in foreign.m.
    % Any changes here may require changes there as well.

    ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava,
        MaybeCSharp, _MaybeErlang),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        (
            MaybeC = yes(Data),
            Data = foreign_type_lang_data(CForeignType, _, _),
            ForeignType = c(CForeignType)
        ;
            MaybeC = no,
            % This is checked by check_foreign_type in make_hlds.
            unexpected($module, $pred, "no C foreign type")
        )
    ;
        Target = target_csharp,
        (
            MaybeCSharp = yes(Data),
            Data = foreign_type_lang_data(CSharpForeignType, _, _),
            ForeignType = csharp(CSharpForeignType)
        ;
            MaybeCSharp = no,
            % This is checked by check_foreign_type in make_hlds.
            unexpected($module, $pred, "no C# foreign type")
        )
    ;
        Target = target_java,
        (
            MaybeJava = yes(Data),
            Data = foreign_type_lang_data(JavaForeignType, _, _),
            ForeignType = java(JavaForeignType)
        ;
            MaybeJava = no,
            % This is checked by check_foreign_type in make_hlds.
            unexpected($module, $pred, "no Java foreign type")
        )
    ;
        Target = target_erlang,
        unexpected($module, $pred, "target erlang")
    ),
    MLDSType = mlds_foreign_type(ForeignType).

%---------------------------------------------------------------------------%

% There is some special-case handling for arrays, foreign types and some
% other types here, but apart from that, currently we return mlds_types
% that are just the same as Mercury types, except that we also store the type
% category, so that we can tell if the type is an enumeration or not, without
% needing to refer to the HLDS type_table.
% XXX It might be a better idea to get rid of the mercury_type/2 MLDS type
% and instead fully convert all Mercury types to MLDS types.

mercury_type_to_mlds_type(ModuleInfo, Type) = MLDSType :-
    ( if type_to_ctor_and_args(Type, TypeCtor, TypeArgs) then
        ( if
            TypeCtor = type_ctor(qualified(unqualified("array"), "array"), 1),
            TypeArgs = [ElemType]
        then
            MLDSElemType = mercury_type_to_mlds_type(ModuleInfo, ElemType),
            MLDSType = mlds_mercury_array_type(MLDSElemType)
        else if
            TypeCtor = type_ctor(qualified(mercury_private_builtin_module,
                "store_at_ref_type"), 1),
            TypeArgs = [RefType]
        then
            MLDSRefType = mercury_type_to_mlds_type(ModuleInfo, RefType),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),
            (
                Target = target_csharp,
                % `mlds_ptr_type' confuses the C# backend because it is also
                % used for `out' parameters. `store_at_ref_type' is not
                % applicable on that backend anyway.
                MLDSType = MLDSRefType
            ;
                ( Target = target_c
                ; Target = target_java
                ; Target = target_erlang
                ),
                MLDSType = mlds_ptr_type(MLDSRefType)
            )
        else
            module_info_get_type_table(ModuleInfo, TypeTable),
            ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
                hlds_data.get_type_defn_body(TypeDefn, TypeBody),
                ( if TypeBody = hlds_foreign_type(ForeignTypeBody) then
                    MLDSType = foreign_type_to_mlds_type(ModuleInfo,
                        ForeignTypeBody)
                else if TypeBody = hlds_abstract_type(_) then
                    Category = classify_type_ctor(ModuleInfo, TypeCtor),
                    ExportedType = non_foreign_type(Type),
                    MLDSType = mercury_type(Type, Category, ExportedType)
                else
                    Category = classify_type_defn_body(TypeBody),
                    ExportedType = non_foreign_type(Type),
                    MLDSType = mercury_type(Type, Category, ExportedType)
                )
            else
                Category = classify_type_ctor(ModuleInfo, TypeCtor),
                ExportedType = non_foreign_type(Type),
                MLDSType = mercury_type(Type, Category, ExportedType)
            )
        )
    else
        Category = ctor_cat_variable,
        ExportedType = non_foreign_type(Type),
        MLDSType = mercury_type(Type, Category, ExportedType)
    ).
%---------------------------------------------------------------------------%

ml_global_const_var_name_to_string(ConstVar, Num) = Str :-
    (
        ConstVar = mgcv_const_var,
        ConstVarStr = "const_var"
    ;
        ConstVar = mgcv_float,
        ConstVarStr = "float"
    ;
        ConstVar = mgcv_int64,
        ConstVarStr = "int64"
    ;
        ConstVar = mgcv_uint64,
        ConstVarStr = "uint64"
    ;
        ConstVar = mgcv_closure_layout,
        ConstVarStr = "closure_layout"
    ;
        ConstVar = mgcv_typevar_vector,
        ConstVarStr = "typevar_vector"
    ;
        ConstVar = mgcv_bit_vector,
        ConstVarStr = "bit_vector"
    ),
    Str = string.format("%s_%d", [s(ConstVarStr), i(Num)]).

ml_field_var_name_to_string(FieldVar) = Str :-
    (
        FieldVar = fvn_reserved_obj_name(CtorName, CtorArity),
        Str = string.format("obj_%s_%d", [s(CtorName), i(CtorArity)])
    ;
        FieldVar = fvn_global_data_field(TypeRawNum, FieldNum),
        Str = string.format("vct_%d_f_%d", [i(TypeRawNum), i(FieldNum)])
    ;
        FieldVar = fvn_du_ctor_field_hld(FieldName),
        % XXX There is nothing to stop the variable names we generate here
        % from clashing with the strings we generate for other kinds of
        % mlds_var_names. In particular, the values of FieldName that
        % ml_gen_hld_field_name generates for unnamed field in a Mercury
        % type, which are generated with "F" ++ .int_to_string(ArgNum),
        % *will* conflict with the lvn_prog_vars that correspond
        % to any variables that the user names "F1", "F2" and so on.
        Str = FieldName
    ;
        FieldVar = fvn_mr_value,
        Str = "MR_value"
    ;
        FieldVar = fvn_data_tag,
        Str = "data_tag"
    ;
        FieldVar = fvn_enum_const(ConstName),
        % XXX There is nothing to stop the variable names we generate here
        % from clashing with the strings we generate for other kinds of
        % mlds_var_names.
        Str = ConstName
    ;
        FieldVar = fvn_sectag_const(ConstName),
        % XXX There is nothing to stop the variable names we generate here
        % from clashing with the strings we generate for other kinds of
        % mlds_var_names.
        Str = ConstName
    ;
        FieldVar = fvn_ptr_num,
        Str = "ptr_num"
    ;
        FieldVar = fvn_env_field_from_local_var(LocalVar),
        Str = ml_local_var_name_to_string(LocalVar)
    ;
        FieldVar = fvn_base_class(BaseNum),
        Str = string.format("base_%d", [i(BaseNum)])
    ;
        FieldVar = fvn_prev,
        Str = "prev"
    ;
        FieldVar = fvn_trace,
        Str = "trace"
    ).

ml_local_var_name_to_string(LocalVar) = Str :-
    (
        LocalVar = lvn_prog_var(ProgVarName, ProgVarNum),
        ( if ProgVarName = "" then
            Str = string.format("Var_%d", [i(ProgVarNum)])
        else
            Str = string.format("%s_%d", [s(ProgVarName), i(ProgVarNum)])
        )
    ;
        LocalVar = lvn_prog_var_foreign(ProgVarName),
        ( if ProgVarName = "" then
            unexpected($pred, "lvn_prog_var_foreign with empty name")
        else
            Str = ProgVarName
        )
    ;
        LocalVar = lvn_prog_var_boxed(ProgVarName, ProgVarNum),
        ( if ProgVarName = "" then
            Str = string.format("boxed_Var_%d", [i(ProgVarNum)])
        else
            Str = string.format("boxed_%s_%d", [s(ProgVarName), i(ProgVarNum)])
        )
    ;
        LocalVar = lvn_prog_var_conv(ConvSeq, ProgVarName, ProgVarNum),
        ( if ProgVarName = "" then
            Str = string.format("conv%d_Var_%d", [i(ConvSeq), i(ProgVarNum)])
        else
            Str = string.format("conv%d_%s_%d",
                [i(ConvSeq), s(ProgVarName), i(ProgVarNum)])
        )
    ;
        LocalVar = lvn_prog_var_next_value(ProgVarName, ProgVarNum),
        % Before the change to structure representations of mlds_var_names,
        % we used to add "__tmp_copy" as a suffix, not "next_value_of_"
        % as a prefix. Using a suffix created the possibility of conflict
        % with other compiler generated names (which could have added a
        % known prefix to a string controlled by the user, which in turn
        % *could* have ended in "__tmp_copy"), and the "tmp_copy" part
        % was also misleading, since these variables are not copied *from*
        % the variable they are named after; in fact, they are copied *to*
        % that variable.
        ( if ProgVarName = "" then
            Str = string.format("next_value_of_Var_%d", [i(ProgVarNum)])
        else
            Str = string.format("next_value_of_%s_%d",
                [s(ProgVarName), i(ProgVarNum)])
        )
    ;
        LocalVar = lvn_local_var(ProgVarName, ProgVarNum),
        ( if ProgVarName = "" then
            Str = string.format("local_Var_%d", [i(ProgVarNum)])
        else
            Str = string.format("local_%s_%d", [s(ProgVarName), i(ProgVarNum)])
        )
    ;
        LocalVar = lvn_tscc_proc_input_var(proc_id_in_tscc(ProcNum), ArgNum,
            VarName),
        Str = string.format("tscc_proc_%d_input_%d_%s",
            [i(ProcNum), i(ArgNum), s(VarName)])
    ;
        LocalVar = lvn_tscc_output_var(ArgNum, VarName),
        Str = string.format("tscc_output_%d_%s", [i(ArgNum), s(VarName)])
    ;
        LocalVar = lvn_tscc_output_var_ptr(ArgNum, VarName),
        Str = string.format("tscc_output_ptr_%d_%s", [i(ArgNum), s(VarName)])
    ;
        LocalVar = lvn_tscc_output_var_succeeded,
        Str = string.format("tscc_output_succeeded", [])
    ;
        LocalVar = lvn_field_var_as_local(FieldVar),
        Str = ml_field_var_name_to_string(FieldVar)
    ;
        LocalVar = lvn_comp_var(CompVar),
        (
            CompVar = lvnc_non_prog_var_boxed(BaseVarStr),
            Str = string.format("boxed_%s", [s(BaseVarStr)])
        ;
            CompVar = lvnc_non_prog_var_conv(ConvSeq, BaseVarStr),
            Str = string.format("conv%d_%s", [i(ConvSeq), s(BaseVarStr)])
        ;
            CompVar = lvnc_non_prog_var_next_value(BaseVarStr),
            Str = string.format("next_value_of_%s", [s(BaseVarStr)])
        ;
            CompVar = lvnc_succeeded,
            Str = "succeeded"
        ;
            CompVar = lvnc_success_indicator,
            Str = "SUCCESS_INDICATOR"
        ;
            CompVar = lvnc_tscc_proc_selector,
            Str = "tscc_proc_selector"
        ;
            CompVar = lvnc_new_obj(Id),
            Str = string.format("new_obj_%d", [i(Id)])
        ;
            CompVar = lvnc_cond(CondNum),
            Str = string.format("cond_%d", [i(CondNum)])
        ;
            CompVar = lvnc_conv_var(ConvVarNum),
            Str = string.format("func_%d", [i(ConvVarNum)])
        ;
            CompVar = lvnc_arg(ArgNum),
            Str = string.format("arg%d", [i(ArgNum)])
        ;
            CompVar = lvnc_wrapper_arg(ArgNum),
            Str = string.format("wrapper_arg_%d", [i(ArgNum)])
        ;
            CompVar = lvnc_param(ArgNum),
            Str = string.format("param_%d", [i(ArgNum)])
        ;
            CompVar = lvnc_out_param(ArgNum),
            Str = string.format("out_param_%d", [i(ArgNum)])
        ;
            CompVar = lvnc_return_value,
            Str = "return_value"
        ;
            CompVar = lvnc_closure,
            Str = "closure"
        ;
            CompVar = lvnc_closure_arg,
            Str = "closure_arg"
        ;
            CompVar = lvnc_closure_layout_ptr,
            Str = "closure_layout_ptr"
        ;
            CompVar = lvnc_type_params,
            Str = "type_params"
        ;
            CompVar = lvnc_type_info,
            Str = "type_info"
        ;
            CompVar = lvnc_cont,
            Str = "cont"
        ;
            CompVar = lvnc_cont_env_ptr,
            Str = "cont_env_ptr"
        ;
            CompVar = lvnc_env,
            Str = "env"
        ;
            CompVar = lvnc_env_ptr,
            Str = "env_ptr"
        ;
            CompVar = lvnc_env_ptr_arg,
            Str = "env_ptr_arg"
        ;
            CompVar = lvnc_frame,
            Str = "frame"
        ;
            CompVar = lvnc_frame_ptr,
            Str = "frame_ptr"
        ;
            CompVar = lvnc_this_frame,
            Str = "this_frame"
        ;
            CompVar = lvnc_stack_chain,
            Str = "stack_chain"
        ;
            CompVar = lvnc_saved_stack_chain(Id),
            Str = string.format("saved_stack_chain_%d", [i(Id)])
        ;
            CompVar = lvnc_args,
            Str = "args"
        ;
            CompVar = lvnc_aux_var(AuxVar, Num),
            (
                AuxVar = mcav_commit,
                AuxVarStr = "commit"
            ;
                AuxVar = mcav_slot,
                AuxVarStr = "slot"
            ;
                AuxVar = mcav_later_slot,
                AuxVarStr = "later_slot"
            ;
                AuxVar = mcav_num_later_solns,
                AuxVarStr = "num_later_solns"
            ;
                AuxVar = mcav_limit,
                AuxVarStr = "limit"
            ;
                AuxVar = mcav_str,
                AuxVarStr = "str"
            ;
                AuxVar = mcav_lo,
                AuxVarStr = "lo"
            ;
                AuxVar = mcav_mid,
                AuxVarStr = "mid"
            ;
                AuxVar = mcav_hi,
                AuxVarStr = "hi"
            ;
                AuxVar = mcav_stop_loop,
                AuxVarStr = "stop_loop"
            ;
                AuxVar = mcav_result,
                AuxVarStr = "result"
            ;
                AuxVar = mcav_case_num,
                AuxVarStr = "case_num"
            ),
            Str = string.format("%s_%d", [s(AuxVarStr), i(Num)])
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds.
%---------------------------------------------------------------------------%
