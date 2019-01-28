%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_module_interface_files.m.
% Main author: fjh (when this code was in modules.m).
%
% This module writes the automatically generated .int0, .int3, .int2 and .int
% files for each Mercury source module.
%
% The interface file system works as follows:
%
% 1. a .int3 file is written, which contains all the types, typeclasses, insts
% and modes defined in the interface. Equivalence types, solver types, insts
% and modes are written in full, others are written in abstract form. These
% are module qualified as far as possible given the information present in the
% current module. The datestamp on the .date3 file gives the last time the
% .int3 file was checked for consistency.
%
% 2. The .int and .int2 files are created, using the .int3 files
% of imported modules to fully module qualify all items.
% The .int2 file is mostly just a fully qualified version of the .int3 file,
% however it also includes some extra information, such as functors for
% discriminated union types, which may be needed for mode analysis.
% The .int3 file must be kept for datestamping purposes. The datestamp
% on the .date file gives the last time the .int and .int2 files
% were checked.
%
% 3. The .int0 file is similar to the .int file except that it also
% includes declarations (but not clauses) from the implementation section.
% It is used when compiling submodules. The datestamp on the .date0
% file gives the last time the .int0 file was checked.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_module_interface_files.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % write_private_interface_file_in0(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling submodules.)
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use
    % these to qualify all the declarations as much as possible. Then write
    % out the .int0 file.
    %
:- pred write_private_interface_file_int0(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, raw_compilation_unit::in,
    io::di, io::uo) is det.

    % write_interface_file_int1_int2(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use these
    % to qualify all items in the interface as much as possible. Then write out
    % the .int and .int2 files.
    %
:- pred write_interface_file_int1_int2(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, raw_compilation_unit::in,
    io::di, io::uo) is det.

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred write_short_interface_file_int3(globals::in, file_name::in,
    raw_compilation_unit::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.check_raw_comp_unit.% undesirable dependency
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.modules.            % undesirable dependency
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.read_modules.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Write out .int0 files.
%

write_private_interface_file_int0(Globals, SourceFileName,
        SourceFileModuleName, MaybeTimestamp, RawCompUnit0, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, ModuleNameContext, _),
    grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        RawCompUnit0, ModuleAndImports, !IO),

    % Check whether we succeeded.
    module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit1,
        Specs0, Errors),
    ( if set.is_non_empty(Errors) then
        module_name_to_file_name(Globals, do_not_create_dirs, ".int0",
            ModuleName, FileName, !IO),
        % XXX _NumErrors
        write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        io.write_strings(["Error reading interface files.\n",
            "`", FileName, "' not written.\n"], !IO)
    else
        % Module-qualify all items.
        module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit,
            map.init, _EventSpecMap, "", _, _, _, _, _, Specs0, Specs),
        (
            Specs = [_ | _],
            module_name_to_file_name(Globals, do_not_create_dirs, ".m",
                ModuleName, FileName, !IO),
            % XXX _NumErrors
            write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            io.write_strings(["`", FileName, "' not written.\n"], !IO)
        ;
            Specs = [],

            % Write out the `.int0' file.

            AugCompUnit = aug_compilation_unit(AugModuleName,
                _ModuleNameContext, ModuleVersionNumbers, SrcItemBlocks,
                _DirectIntItemBlocks, _IndirectIntItemBlocks,
                _OptItemBlocks, _IntForOptItemBlocks),
            expect(unify(ModuleName, AugModuleName), $module, $pred,
                "AugModuleName != ModuleName"),
            % XXX ITEM_LIST Should pass AugCompUnit2
            process_item_blocks_for_private_interface(ModuleName,
                SrcItemBlocks,
                cord.init, IntInclsCord, cord.init, ImpInclsCord,
                cord.init, IntAvailsCord, cord.init, ImpAvailsCord,
                cord.init, IntItemsCord, cord.init, ImpItemsCord),
            ( if
                map.search(ModuleVersionNumbers, ModuleName, VersionNumbers)
            then
                MaybeVersionNumbers = yes(VersionNumbers)
            else
                MaybeVersionNumbers = no
            ),
            IntIncls = cord.list(IntInclsCord),
            ImpIncls = cord.list(ImpInclsCord),
            IntAvails = cord.list(IntAvailsCord),
            ImpAvails = cord.list(ImpAvailsCord),
            IntItems = cord.list(IntItemsCord),
            ImpItems = cord.list(ImpItemsCord),
            ParseTreeInt0 = parse_tree_int(ModuleName, ifk_int0,
                ModuleNameContext, MaybeVersionNumbers, IntIncls, ImpIncls,
                IntAvails, ImpAvails, IntItems, ImpItems),
            actually_write_interface_file(Globals, SourceFileName,
                ParseTreeInt0, MaybeTimestamp, !IO),
            touch_interface_datestamp(Globals, ModuleName, ".date0", !IO)
        )
    ).

    % process_items_for_private_interface processes each item in the item
    % list of a module, as part of the process of creating .int0 files.
    %
    % The `.int0' file contains items which are available to any module in the
    % interface section, and items which are only available to submodules in
    % the implementation section. The term "private interface" is ambiguous:
    % sometimes it refers to the `.int0' file which, as just explained,
    % contains the public interface as well. The term "private interface
    % proper" may be used to refer to the information in the implementation
    % section of the `.int0' file.
    %
    % (Historically, the `.int0' file did not distinguish between the public
    % and private interfaces.)
    %
    % This predicate has several jobs.
    %
    % - It removes items that do not belong in the private interface,
    %   in either sense. This includes clauses, pragmas that function as
    %   clauses, and initialise and finalise declarations, since effectively
    %   they also represent code.
    %
    % - It expands any mutable declarations into the pred and mode declarations
    %   for their access predicates, since only these components of a
    %   mutable declaration should be written to a private interface file.
    %
    % - It makes any instance declarations abstract.
    %
    % - It removes the items that divide sections from each other, and then
    %   collects the remaining items in two lists, containing the items
    %   that appear in the interface section and in the implementation section
    %   respectively.
    %
:- pred process_item_blocks_for_private_interface(module_name::in,
    list(src_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    cord(item)::in, cord(item)::out) is det.

process_item_blocks_for_private_interface(_ModuleName, [],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).
process_item_blocks_for_private_interface(ModuleName, [ItemBlock | ItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord) :-
    ItemBlock = item_block(SrcSection, _, Incls, Avails, Items),
    (
        (
            SrcSection = sms_interface,
            !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
            !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
            % XXX ITEM_LIST Document why we need to add ImportAvails
            % to !ImpAvailsCord.
            list.filter(avail_is_import, Avails, ImportAvails),
            !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(ImportAvails),
            Section = ms_interface
        ;
            SrcSection = sms_implementation,
            !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
            !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
            Section = ms_implementation
        ),
        process_items_for_private_interface(ModuleName, Section, Items,
            !IntItemsCord, !ImpItemsCord)
    ;
        % XXX ITEM_LIST Is this the right thing to do for sms_impl_but_...?
        SrcSection = sms_impl_but_exported_to_submodules
        % Do nothing.
    ),
    process_item_blocks_for_private_interface(ModuleName, ItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).

:- pred process_items_for_private_interface(module_name::in,
    module_section::in, list(item)::in,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out) is det.

process_items_for_private_interface(_ModuleName, _Section, [],
        !IntItemsCord, !ImpItemsCord).
process_items_for_private_interface(ModuleName, Section, [Item | Items],
        !IntItemsCord, !ImpItemsCord) :-
    process_item_for_private_interface(ModuleName, Section, Item,
        !IntItemsCord, !ImpItemsCord),
    process_items_for_private_interface(ModuleName, Section, Items,
        !IntItemsCord, !ImpItemsCord).

:- pred process_item_for_private_interface(module_name::in,
    module_section::in, item::in,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out) is det.

process_item_for_private_interface(ModuleName, Section, Item,
        !IntItemsCord, !ImpItemsCord) :-
    (
        ( Item = item_clause(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        )
        % Don't include in either section of the private interface.
    ;
        Item = item_type_repn(_)
        % process_item_for_private_interface should be invoked only on items
        % from the *source file*, not on items read from any interface file.
        % Any occurrence of item_type_repns in the source file should have been
        % caught and reported by the parsing code. If through some bug
        % they make it here, neither reporting them again nor aborting the
        % compiler is likely to be helpful.
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no
        ;
            AllowedInInterface = yes,
            add_item_to_section_items(Section, Item,
                !IntItemsCord, !ImpItemsCord)
        )
    ;
        % XXX ITEM_LIST The action here follows what this predicate used
        % to do before the item list change. I (zs) don't think that
        % it does the right thing for item_nothing, but then again
        % I don't think that such items actually reach here ...
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_foreign_import_module(_)
        ; Item = item_nothing(_)
        ),
        add_item_to_section_items(Section, Item,
            !IntItemsCord, !ImpItemsCord)
    ;
        Item = item_instance(InstanceInfo),
        AbstractInstanceInfo = make_instance_abstract(InstanceInfo),
        AbstractItem = item_instance(AbstractInstanceInfo),
        add_item_to_section_items(Section, AbstractItem,
            !IntItemsCord, !ImpItemsCord)
    ;
        Item = item_mutable(ItemMutable),
        ItemMutable = item_mutable_info(MutableName,
            _OrigType, Type, _OrigInst, Inst, _Value, _Varset, Attrs,
            Context, _SeqNum),
        ConstantInterface = mutable_var_constant(Attrs),
        (
            ConstantInterface = mutable_constant,
            ConstantGetPredDecl = constant_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantSetPredDecl = constant_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantGetPredDeclItem = item_pred_decl(ConstantGetPredDecl),
            ConstantSetPredDeclItem = item_pred_decl(ConstantSetPredDecl),
            add_item_to_section_items(Section, ConstantGetPredDeclItem,
                !IntItemsCord, !ImpItemsCord),
            add_item_to_section_items(Section, ConstantSetPredDeclItem,
                !IntItemsCord, !ImpItemsCord)
        ;
            ConstantInterface = mutable_not_constant,
            StdGetPredDecl = std_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdSetPredDecl = std_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdGetPredDeclItem = item_pred_decl(StdGetPredDecl),
            StdSetPredDeclItem = item_pred_decl(StdSetPredDecl),
            add_item_to_section_items(Section, StdGetPredDeclItem,
                !IntItemsCord, !ImpItemsCord),
            add_item_to_section_items(Section, StdSetPredDeclItem,
                !IntItemsCord, !ImpItemsCord),

            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = mutable_attach_to_io_state,
                IOGetPredDecl = io_get_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOSetPredDecl = io_set_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOGetPredDeclItem = item_pred_decl(IOGetPredDecl),
                IOSetPredDeclItem = item_pred_decl(IOSetPredDecl),
                add_item_to_section_items(Section, IOGetPredDeclItem,
                    !IntItemsCord, !ImpItemsCord),
                add_item_to_section_items(Section, IOSetPredDeclItem,
                    !IntItemsCord, !ImpItemsCord)
            ;
                IOStateInterface = mutable_dont_attach_to_io_state
            )
        )
    ).

:- pred add_item_to_section_items(module_section::in, item::in,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out) is det.
:- pragma inline(add_item_to_section_items/6).

add_item_to_section_items(Section, Item, !IntItemsCord, !ImpItemsCord) :-
    (
        Section = ms_interface,
        !:IntItemsCord = cord.snoc(!.IntItemsCord, Item)
    ;
        Section = ms_implementation,
        !:ImpItemsCord = cord.snoc(!.ImpItemsCord, Item)
    ).

%---------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

write_interface_file_int1_int2(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, RawCompUnit0, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, ModuleNameContext,
        _RawItemBlocks0),
    get_interface(include_impl_types, RawCompUnit0, IntRawCompUnit),

    % Get the .int3 files for imported modules.
    grab_unqual_imported_modules(Globals, SourceFileName,
        SourceFileModuleName, IntRawCompUnit, ModuleAndImports, !IO),

    some [!Specs, !IntIncls, !ImpIncls, !IntAvails, !ImpAvails,
        !IntItems, !ImpItems]
    (
        % Check whether we succeeded.
        module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit0,
            !:Specs, Errors),
        ( if set.is_non_empty(Errors) then
            % XXX _NumErrors
            write_error_specs(!.Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            module_name_to_file_name(Globals, do_not_create_dirs, ".int",
                ModuleName, IntFileName, !IO),
            module_name_to_file_name(Globals, do_not_create_dirs, ".int2",
                ModuleName, Int2FileName, !IO),
            io.write_strings(["Error reading short interface files.\n",
                "`", IntFileName, "' and ",
                "`", Int2FileName, "' not written.\n"], !IO)
        else
            % Module-qualify all items.
            module_qualify_aug_comp_unit(Globals, AugCompUnit0, AugCompUnit,
                map.init, _, "", _, _, _, _, _, !Specs),

            % We want to finish writing the interface file (and keep
            % the exit status at zero) if we found some warnings.
            globals.set_option(halt_at_warn, bool(no),
                Globals, NoHaltAtWarnGlobals),
            write_error_specs(!.Specs, NoHaltAtWarnGlobals,
                0, _NumWarnings, 0, NumErrors, !IO),
            ( if NumErrors > 0 then
                module_name_to_file_name(Globals, do_not_create_dirs, ".int",
                    ModuleName, IntFileName, !IO),
                io.write_strings(["`", IntFileName, "' ", "not written.\n"],
                    !IO)
            else
                % Strip out the imported interfaces. Assertions are also
                % stripped since they should only be written to .opt files.
                % Check for some warnings, and then write out the `.int'
                % and `int2' files and touch the `.date' file.

                % XXX ITEM_LIST Why do we augment the raw comp unit
                % if we throw away the augmented part?
                AugCompUnit = aug_compilation_unit(_, _,
                    _ModuleVersionNumbers, SrcItemBlocks,
                    _DirectIntItemBlocks, _IndirectIntItemBlocks,
                    _OptItemBlocks, _IntForOptItemBlocks),
                src_item_blocks_to_int_imp_items(SrcItemBlocks,
                    do_strip_assertions, !:IntIncls, !:ImpIncls,
                    !:IntAvails, !:ImpAvails, !:IntItems, !:ImpItems),
                strip_unnecessary_impl_defns(!IntAvails, !ImpAvails,
                    !IntItems, !ImpItems),
                report_and_strip_clauses_in_items(!IntItems,
                    [], InterfaceSpecs0),
                report_and_strip_clauses_in_items(!ImpItems,
                    InterfaceSpecs0, InterfaceSpecs1),
                % XXX ITEM_LIST Constructing ToCheckIntCompUnit
                % seems a roundabout way to do the check.
                ToCheckIntItemBlock = item_block(ms_interface,
                    ModuleNameContext, !.IntIncls, !.IntAvails, !.IntItems),
                ToCheckIntCompUnit = raw_compilation_unit(ModuleName,
                    ModuleNameContext, [ToCheckIntItemBlock]),
                check_int_for_no_exports(ToCheckIntCompUnit,
                    InterfaceSpecs1, InterfaceSpecs),
                write_error_specs(InterfaceSpecs, Globals,
                    0, _NumWarnings2, 0, _NumErrors2, !IO),
                % XXX _NumErrors
                % The MaybeVersionNumbers we put into ParseTreeInt1 and
                % ParseTreeInt2 are dummies. If we want to generate version
                % numbers in interface files, the two calls below to
                % actually_write_interface_file will do it.
                % XXX BOTH of those calls will do it. This should not
                % be necessary; since the .int2 file is a shorter version
                % of the .int file, it should be faster to compute the
                % version number record in ParseTreeInt2 by taking the one
                % in ParseTreeInt1 and deleting the irrelevant entries
                % than to compute it from the items in ParseTreeInt2 itself.
                % This would best be done by having the code that
                % cuts !.IntItems and !.ImpItems down to ShortIntItems
                % and ShortImpItems delete entries from ParseTreeInt1's
                % version number record as it deletes the items they are for
                % themselves.
                %
                % When creating an augmented compilation unit, we should
                % never read in both the .int file and the .int2 file for
                % the same module, so the fact that both files will have
                % version number info for the same module shouldn't cause
                % a collision in the augmented compilation unit's version
                % number map.
                DummyMaybeVersionNumbers = no,
                ParseTreeInt1 = parse_tree_int(ModuleName, ifk_int,
                    ModuleNameContext, DummyMaybeVersionNumbers,
                    !.IntIncls, !.ImpIncls, !.IntAvails, !.ImpAvails,
                    !.IntItems, !.ImpItems),
                actually_write_interface_file(Globals, SourceFileName,
                    ParseTreeInt1, MaybeTimestamp, !IO),
                % XXX ITEM_LIST Couldn't we get ShortIntItems and
                % ShortImpItems without constructing BothRawItemBlocks?
                % XXX Yes, we could, but we call
                % get_short_interface_from_raw_item_blocks
                % from someplace else as well.
                int_imp_items_to_item_blocks(ModuleNameContext,
                    ms_interface, ms_implementation,
                    !.IntIncls, !.ImpIncls, !.IntAvails, !.ImpAvails,
                    !.IntItems, !.ImpItems, BothRawItemBlocks),
                get_short_interface_from_raw_item_blocks(sifk_int2,
                    BothRawItemBlocks,
                    ShortIntIncls, ShortImpIncls,
                    ShortIntAvails, ShortImpAvails,
                    ShortIntItems, ShortImpItems),
                % The MaybeVersionNumbers in ParseTreeInt is a dummy.
                % If the want to generate version numbers in interface files,
                % this will be by the call to actually_write_interface_file
                % below.
                ParseTreeInt2 = parse_tree_int(ModuleName, ifk_int2,
                    ModuleNameContext, DummyMaybeVersionNumbers,
                    ShortIntIncls, ShortImpIncls,
                    ShortIntAvails, ShortImpAvails,
                    ShortIntItems, ShortImpItems),
                actually_write_interface_file(Globals, SourceFileName,
                    ParseTreeInt2, MaybeTimestamp, !IO),
                touch_interface_datestamp(Globals, ModuleName, ".date", !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out .int3 files.
%

write_short_interface_file_int3(Globals, SourceFileName, RawCompUnit, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.

    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext, _),
    some [!Specs] (
        !:Specs = [],
        get_interface(dont_include_impl_types, RawCompUnit, IntRawCompUnit),
        IntRawCompUnit = raw_compilation_unit(_, _, IFileRawItemBlocks0),
        % Assertions are also stripped since they should only be written
        % to .opt files.
        strip_assertions_in_item_blocks(IFileRawItemBlocks0,
            IFileRawItemBlocks1),
        report_and_strip_clauses_in_item_blocks(IFileRawItemBlocks1,
            IFileRawItemBlocks, !Specs),
        get_short_interface_from_raw_item_blocks(sifk_int3, IFileRawItemBlocks,
            IntIncls0, ImpIncls0, IntAvails0, ImpAvails0,
            IntItems0, ImpItems0),
        MaybeVersionNumbers = no,
        ParseTreeInt0 = parse_tree_int(ModuleName, ifk_int3,
            ModuleNameContext, MaybeVersionNumbers, IntIncls0, ImpIncls0,
            IntAvails0, ImpAvails0, IntItems0, ImpItems0),
        module_qualify_parse_tree_int(Globals, ParseTreeInt0, ParseTreeInt,
            !Specs),
        write_error_specs(!.Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        % XXX why do we do this even if there are some errors?
        actually_write_interface_file(Globals, SourceFileName,
            ParseTreeInt, no, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".date3", !IO)
    ).

%---------------------------------------------------------------------------%

:- type maybe_strip_assertions
    --->    dont_strip_assertions
    ;       do_strip_assertions.

:- pred src_item_blocks_to_int_imp_items(list(src_item_block)::in,
    maybe_strip_assertions::in,
    list(item_include)::out, list(item_include)::out,
    list(item_avail)::out, list(item_avail)::out,
    list(item)::out, list(item)::out) is det.

src_item_blocks_to_int_imp_items(SrcItemBlocks, MaybeStripAssertions,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems) :-
    src_item_blocks_to_int_imp_items_loop(SrcItemBlocks, MaybeStripAssertions,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord,
        cord.init, IntItemsCord, cord.init, ImpItemsCord),
    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails = cord.list(ImpAvailsCord),
    IntItems = cord.list(IntItemsCord),
    ImpItems = cord.list(ImpItemsCord).

:- pred src_item_blocks_to_int_imp_items_loop(list(src_item_block)::in,
    maybe_strip_assertions::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    cord(item)::in, cord(item)::out) is det.

src_item_blocks_to_int_imp_items_loop([], _,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).
src_item_blocks_to_int_imp_items_loop([SrcItemBlock | SrcItemBlocks],
        MaybeStripAssertions,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord) :-
    SrcItemBlock = item_block(SrcSection, _Context, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        (
            MaybeStripAssertions = dont_strip_assertions,
            !:IntItemsCord = !.IntItemsCord ++ cord.from_list(Items)
        ;
            MaybeStripAssertions = do_strip_assertions,
            strip_assertions_in_items(Items, StrippedItems),
            !:IntItemsCord = !.IntItemsCord ++ cord.from_list(StrippedItems)
        )
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        (
            MaybeStripAssertions = dont_strip_assertions,
            !:ImpItemsCord = !.ImpItemsCord ++ cord.from_list(Items)
        ;
            MaybeStripAssertions = do_strip_assertions,
            strip_assertions_in_items(Items, StrippedItems),
            !:ImpItemsCord = !.ImpItemsCord ++ cord.from_list(StrippedItems)
        )
    ),
    src_item_blocks_to_int_imp_items_loop(SrcItemBlocks, MaybeStripAssertions,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).

%---------------------------------------------------------------------------%

:- pred strip_assertions_in_item_blocks(list(item_block(MS))::in,
    list(item_block(MS))::out) is det.

strip_assertions_in_item_blocks([], []).
strip_assertions_in_item_blocks([ItemBlock0 | ItemBlocks0],
        [ItemBlock | ItemBlocks]) :-
    ItemBlock0 = item_block(Section, Context, Incls, Avails, Items0),
    strip_assertions_in_items(Items0, Items),
    ItemBlock = item_block(Section, Context, Incls, Avails, Items),
    strip_assertions_in_item_blocks(ItemBlocks0, ItemBlocks).

:- pred strip_assertions_in_items(list(item)::in, list(item)::out) is det.

strip_assertions_in_items(Items0, Items) :-
    strip_assertions_in_items_acc(Items0, [], RevItems),
    list.reverse(RevItems, Items).

:- pred strip_assertions_in_items_acc(list(item)::in,
    list(item)::in, list(item)::out) is det.

strip_assertions_in_items_acc([], !RevItems).
strip_assertions_in_items_acc([Item | Items], !RevItems) :-
    % If this code ever changes to care about the order of the items,
    % you will need to modify strip_imported_items_and_assertions.
    ( if
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(promise_type_true, _, _, _, _, _)
    then
        true
    else
        !:RevItems = [Item | !.RevItems]
    ),
    strip_assertions_in_items_acc(Items, !RevItems).

%---------------------------------------------------------------------------%

:- pred strip_unnecessary_impl_defns(
    list(item_avail)::in, list(item_avail)::out,
    list(item_avail)::in, list(item_avail)::out,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

strip_unnecessary_impl_defns(!IntAvails, !ImpAvails, !IntItems, !ImpItems) :-
    some [!IntTypesMap, !ImpTypesMap] (
        map.init(!:IntTypesMap),
        map.init(!:ImpTypesMap),
        gather_type_defns_in_section(ms_interface,
            !IntItems, !IntTypesMap),
        gather_type_defns_in_section(ms_implementation,
            !ImpItems, !ImpTypesMap),
        BothTypesMap = multi_map.merge(!.IntTypesMap, !.ImpTypesMap),

        % Work out which module imports in the implementation section of
        % the interface are required by the definitions of equivalence
        % types and dummy types in the implementation.
        get_requirements_of_impl_exported_types(!.IntTypesMap, !.ImpTypesMap,
            BothTypesMap, NecessaryDummyTypeCtors,
            NecessaryAbsImpExpTypeCtors, NecessaryTypeImpImports),
        set.union(NecessaryDummyTypeCtors, NecessaryAbsImpExpTypeCtors,
            AllNecessaryTypeCtors),

        % Work out which module imports in the implementation section of
        % the interface file are required by the definitions of typeclasses
        % in the implementation. Specifically, we require the ones
        % that are needed by any constraints on the typeclasses.
        get_requirements_of_impl_typeclasses_in_items(!.ImpItems,
            NecessaryTypeclassImpImports),

        NecessaryImpImports = set.union(NecessaryTypeImpImports,
            NecessaryTypeclassImpImports),

        % If a type in the implementation section isn't dummy and doesn't have
        % foreign type alternatives, make it abstract.
        map.map_values_only(make_impl_type_abstract(BothTypesMap),
            !ImpTypesMap),

        % If there is an exported type declaration for a type with an abstract
        % declaration in the implementation (usually it will originally
        % have been a d.u. type), remove the declaration in the implementation.
        % Don't remove `type_is_abstract_enum' declarations, though.
        %
        % XXX This comment doesn't match the code.
        map.foldl(find_removable_abstract_exported_types(!.IntTypesMap),
            !.ImpTypesMap, set.init, RemovableAbstractExportedTypes),
        set.foldl(multi_map.delete, RemovableAbstractExportedTypes,
            !ImpTypesMap),

        map.foldl_values(add_type_defn_items, !.ImpTypesMap, !ImpItems),

        find_need_imports(!.ImpItems, NeedImports, NeedForeignImports),
        (
            NeedImports = need_imports,
            strip_unnecessary_impl_imports(NecessaryImpImports, !ImpAvails)
        ;
            NeedImports = dont_need_imports,
            !:ImpAvails = []
        ),
        strip_unnecessary_impl_defns_in_items(!.ImpItems, NeedForeignImports,
            !.IntTypesMap, AllNecessaryTypeCtors, cord.init, ItemsCord),
        !:ImpItems = cord.list(ItemsCord),

        (
            !.ImpItems = []
        ;
            !.ImpItems = [_ | _],
            standardize_items(!ImpItems)
        ),
        standardize_imports(!IntAvails),
        standardize_imports(!ImpAvails)
    ).

    % See the comment on the one call above.
    %
:- pred find_removable_abstract_exported_types(type_defn_map::in,
    type_ctor::in, list(item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

find_removable_abstract_exported_types(IntTypesMap,
        ImpTypeCtor, ImpItemTypeDefnInfos, !AbstractExportedTypes) :-
    ( if
        all [ImpItemTypeDefnInfo] (
            list.member(ImpItemTypeDefnInfo, ImpItemTypeDefnInfos)
        => (
            ImpItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
            TypeDefn = parse_tree_abstract_type(Details),
            Details \= abstract_type_fits_in_n_bits(_)
        )),
        multi_map.contains(IntTypesMap, ImpTypeCtor)
    then
        set.insert(ImpTypeCtor, !AbstractExportedTypes)
    else
        true
    ).

:- pred add_type_defn_items(list(item_type_defn_info)::in,
    list(item)::in, list(item)::out) is det.

add_type_defn_items([], !ImpItems).
add_type_defn_items([ItemTypeDefnInfo | ItemTypeDefnInfos], !ImpItems) :-
    !:ImpItems = [item_type_defn(ItemTypeDefnInfo) | !.ImpItems],
    add_type_defn_items(ItemTypeDefnInfos, !ImpItems).

%---------------------------------------------------------------------------%

:- pred standardize_imports(list(item_avail)::in, list(item_avail)::out)
    is det.

standardize_imports(Avails0, Avails) :-
    standardize_imports_build_map(Avails0, map.init, Map),
    map.to_assoc_list(Map, AssocList),
    rebuild_imports(AssocList, Avails).

:- type module_import_info
    --->    module_import_info(import_or_use, prog_context, int).

:- pred standardize_imports_build_map(list(item_avail)::in,
    map(module_name, module_import_info)::in,
    map(module_name, module_import_info)::out) is det.

standardize_imports_build_map([], !Map).
standardize_imports_build_map([Avail | Avails], !Map) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, SeqNum)),
        ImportOrUse = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, SeqNum)),
        ImportOrUse = use_decl
    ),
    ( if map.search(!.Map, ModuleName, OldInfo) then
        OldInfo = module_import_info(OldImportOrUse, OldContext, _OldSeqNum),
        ( if ImportOrUse = OldImportOrUse then
            % If we see two `:- import_module' or two `:- use_module'
            % declarations for the same module, we keep the textually
            % earlier one. (It shouldn't matter which one we keep, since
            % our caller shouldn't use the contexts, but just in case
            % that changes later, ...)
            ( if compare((<), Context, OldContext) then
                Info = module_import_info(ImportOrUse, Context, SeqNum),
                map.det_update(ModuleName, Info, !Map)
            else
                true
            )
        else
            % If we see both `:- import_module' and `:- use_module' for a
            % module, we keep the import, since the use doesn't allow
            % any references the import doesn't.
            ( if ImportOrUse = import_decl, OldImportOrUse = use_decl then
                Info = module_import_info(ImportOrUse, Context, SeqNum),
                map.det_update(ModuleName, Info, !Map)
            else
                % The import is already the one in the map.
                true
            )
        )
    else
        Info = module_import_info(ImportOrUse, Context, SeqNum),
        map.det_insert(ModuleName, Info, !Map)
    ),
    standardize_imports_build_map(Avails, !Map).

:- pred rebuild_imports(assoc_list(module_name, module_import_info)::in,
    list(item_avail)::out) is det.

rebuild_imports([], []).
rebuild_imports([Pair | Pairs], [Avail | Avails]) :-
    Pair = ModuleName - module_import_info(ImportOrUse, Context, SeqNum),
    (
        ImportOrUse = import_decl,
        Avail = avail_import(avail_import_info(ModuleName, Context, SeqNum))
    ;
        ImportOrUse = use_decl,
        Avail = avail_use(avail_use_info(ModuleName, Context, SeqNum))
    ),
    rebuild_imports(Pairs, Avails).

%---------------------------------------------------------------------------%

:- pred standardize_items(list(item)::in, list(item)::out) is det.

standardize_items(Items0, Items) :-
    do_standardize_impl_items(Items0, [], RevRemainderItems,
        [], TypeDefnInfos),
    list.reverse(RevRemainderItems, RemainderItems),
    TypeDefnItems = list.map(wrap_type_defn_item, TypeDefnInfos),
    Items = TypeDefnItems ++ RemainderItems.

:- func wrap_type_defn_item(item_type_defn_info) = item.

wrap_type_defn_item(ItemTypeDefn) = item_type_defn(ItemTypeDefn).

:- pred do_standardize_impl_items(list(item)::in,
    list(item)::in, list(item)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

do_standardize_impl_items([], !RevRemainderItems, !TypeDefns).
do_standardize_impl_items([Item | Items], !RevRemainderItems, !TypeDefns) :-
    ( if Item = item_type_defn(ItemTypeDefn) then
        insert_type_defn(ItemTypeDefn, !TypeDefns)
    else
        !:RevRemainderItems = [Item | !.RevRemainderItems]
    ),
    do_standardize_impl_items(Items, !RevRemainderItems, !TypeDefns).

:- pred insert_type_defn(item_type_defn_info::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

insert_type_defn(New, [], [New]).
insert_type_defn(New, [Head | Tail], Result) :-
    New = item_type_defn_info(NewSymName, NewParams, _, _, _, _),
    Head = item_type_defn_info(HeadSymName, HeadParams, _, _, _, _),
    compare(CompareSymName, NewSymName, HeadSymName),
    ( if
        (
            CompareSymName = (<)
        ;
            CompareSymName = (=),
            list.length(NewParams, NewParamsLength),
            list.length(HeadParams, HeadParamsLength),
            compare(Compare, NewParamsLength, HeadParamsLength),
            Compare = (<)
        )
    then
        Result = [New, Head | Tail]
    else
        insert_type_defn(New, Tail, NewTail),
        Result = [Head | NewTail]
    ).

:- pred make_impl_type_abstract(type_defn_map::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

make_impl_type_abstract(TypeDefnMap, !ItemTypeDefnInfos) :-
    ( if !.ItemTypeDefnInfos = [ItemTypeDefnInfo0] then
        % XXX TYPE_REPN We should also generate fits_in_n_bits if the
        % original type is a less-than-word-sized builtin, such as int8.

        ItemTypeDefnInfo0 = item_type_defn_info(_, _, TypeDefn0, _, _, _),
        (
            TypeDefn0 = parse_tree_du_type(DetailsDu0),
            DetailsDu0 = type_details_du(Ctors, MaybeEqCmp,
                MaybeDirectArgCtors),
            ( if
                constructor_list_represents_dummy_argument_type(TypeDefnMap,
                    Ctors, MaybeEqCmp, MaybeDirectArgCtors)
            then
                % Leave dummy types alone.
                true
            else
                ( if du_type_is_enum(DetailsDu0, NumBits) then
                    DetailsAbs = abstract_type_fits_in_n_bits(NumBits)
                else
                    DetailsAbs = abstract_type_general
                ),
                TypeDefn = parse_tree_abstract_type(DetailsAbs),
                ItemTypeDefnInfo = ItemTypeDefnInfo0 ^ td_ctor_defn
                    := TypeDefn,
                !:ItemTypeDefnInfos = [ItemTypeDefnInfo]
            )
        ;
            TypeDefn0 = parse_tree_eqv_type(_)
            % XXX TYPE_REPN We currently leave the type definition alone.
            % However, in the future we should test whether the type
            % equivalence is to a type that requires special treatment,
            % either with respect to type representation (because it is smaller
            % than a word, because it is bigger than a word, or because it is
            % guaranteed to be an aligned pointer) or because it needs to be
            % passed in an FP register.
            %
            % If the type does require special treatment, we should generate
            % an item that specifies that treatment, and no more.
            % If the type does not require special treatment, we should
            % generate an item that specifies the absence of a need for
            % special treatment: a simple abstract type definition
            % should suffice.
        ;
            TypeDefn0 = parse_tree_abstract_type(_)
            % This type is already abstract.
        ;
            ( TypeDefn0 = parse_tree_solver_type(_)
            ; TypeDefn0 = parse_tree_foreign_type(_)
            )
            % XXX TYPE_REPN Keeping these in their non-abstract form
            % looks like a bug to me. -zs
        )
    else
        % This type constructor has either no definition, or two or more
        % definitions. In either case, the error should be reported somewhere
        % else.
        true
    ).

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % See the documentation for `type_util.check_dummy_type' for the definition
    % of a dummy type.
    %
    % NOTE: changes here may require changes to `type_util.check_dummy_type'.
    %
:- pred constructor_list_represents_dummy_argument_type(type_defn_map::in,
    list(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in) is semidet.

constructor_list_represents_dummy_argument_type(TypeDefnMap,
        Ctors, MaybeCanonical, MaybeDirectArgCtors) :-
    constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
        Ctors, MaybeCanonical, MaybeDirectArgCtors, []).

:- pred constructor_list_represents_dummy_argument_type_2(type_defn_map::in,
    list(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in, list(mer_type)::in) is semidet.

constructor_list_represents_dummy_argument_type_2(TypeDefnMap, [Ctor],
        canon, no, CoveredTypes) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _Name, Args, _Arity,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    (
        % A single zero-arity constructor.
        Args = []
    ;
        % A constructor with a single dummy argument.
        Args = [ctor_arg(_, ArgType, _)],
        ctor_arg_is_dummy_type(TypeDefnMap, ArgType, CoveredTypes) = yes
    ).

:- func ctor_arg_is_dummy_type(type_defn_map, mer_type, list(mer_type)) = bool.

ctor_arg_is_dummy_type(TypeDefnMap, Type, CoveredTypes0) = IsDummyType :-
    (
        Type = defined_type(SymName, TypeArgs, _Kind),
        ( if list.member(Type, CoveredTypes0) then
            % The type is circular.
            IsDummyType = no
        else
            Arity = list.length(TypeArgs),
            TypeCtor = type_ctor(SymName, Arity),
            ( if
                is_type_ctor_a_builtin_dummy(TypeCtor)
                    = is_builtin_dummy_type_ctor
            then
                IsDummyType = yes
            else if
                % Can we find a definition of the type that tells us it is a
                % dummy type?
                multi_map.search(TypeDefnMap, TypeCtor, ItemTypeDefnInfos),
                list.member(ItemTypeDefnInfo, ItemTypeDefnInfos),
                ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn,
                    _, _, _),
                TypeDefn = parse_tree_du_type(DetailsDu),
                DetailsDu = type_details_du(TypeCtors, MaybeEqCmp,
                    MaybeDirectArgCtors),
                CoveredTypes = [Type | CoveredTypes0],
                constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
                    TypeCtors, MaybeEqCmp, MaybeDirectArgCtors, CoveredTypes)
            then
                IsDummyType = yes
            else
                IsDummyType = no
            )
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        IsDummyType = no
    ).

    % get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
    %   BothTypeMap, DummyTypeCtors, NecessaryTypeCtors, Modules):
    %
    % Figure out the set of abstract equivalence type constructors
    % (i.e. the types that are exported as abstract types and which are defined
    % in the implementation section as equivalence types or as foreign types).
    % Return in NecessaryTypeCtors the smallest set containing those
    % constructors, and the set of private type constructors referred to
    % by the right hand side of any type in NecessaryTypeCtors.
    %
    % Return in DummyTypeCtors the set of dummy type constructors.
    %
    % Given a du type definition in the implementation section, we should
    % include it in AbsImplExpLhsTypeCtors if the type constructor is abstract
    % exported and the implementation section also contains a foreign_type
    % definition of the type constructor.
    %
    % Given a enumeration type definition in the implementation section, we
    % should include it in AbsImplExpEnumTypeCtors if the type constructor is
    % abstract exported.
    %
    % Return in Modules the set of modules that define the type constructors
    % in NecessaryTypeCtors.
    %
:- pred get_requirements_of_impl_exported_types(type_defn_map::in,
    type_defn_map::in, type_defn_map::in,
    set(type_ctor)::out, set(type_ctor)::out, set(module_name)::out) is det.

get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
        BothTypeMap, DummyTypeCtors, NecessaryTypeCtors, Modules) :-
    multi_map.to_flat_assoc_list(ImplTypeMap, ImplTypes),
    list.foldl3(
        accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap, BothTypeMap),
        ImplTypes, set.init, AbsImplExpLhsTypeCtors,
        set.init, AbsImplExpEnumTypeCtors, set.init, DummyTypeCtors),
    set.fold3(accumulate_abs_impl_exported_type_rhs(ImplTypeMap),
        AbsImplExpLhsTypeCtors,
        set.init, AbsEqvRhsTypeCtors, set.init, ForeignDuFieldTypeCtors,
        set.init, Modules),
    NecessaryTypeCtors = set.union_list([AbsImplExpLhsTypeCtors,
        AbsEqvRhsTypeCtors, ForeignDuFieldTypeCtors,
        AbsImplExpEnumTypeCtors]).

:- pred accumulate_abs_impl_exported_type_lhs(type_defn_map::in,
    type_defn_map::in, pair(type_ctor, item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap, BothTypesMap,
        TypeCtor - ItemTypeDefnInfo, !AbsEqvLhsTypeCtors,
        !AbsImplExpEnumTypeCtors, !DummyTypeCtors) :-
    % A type may have multiple definitions because it may be defined both
    % as a foreign type and as a Mercury type. We grab any equivalence types
    % that are in there.
    ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
    (
        TypeDefn = parse_tree_eqv_type(_),
        ( if map.search(InterfaceTypeMap, TypeCtor, _) then
            set.insert(TypeCtor, !AbsEqvLhsTypeCtors)
        else
            true
        )
    ;
        TypeDefn = parse_tree_foreign_type(_),
        ( if map.search(InterfaceTypeMap, TypeCtor, _) then
            set.insert(TypeCtor, !AbsEqvLhsTypeCtors)
        else
            true
        )
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(Ctors, MaybeEqCmp, MaybeDirectArgCtors),
        ( if
            map.search(InterfaceTypeMap, TypeCtor, _),
            du_type_is_enum(DetailsDu, _NumBits)
        then
            set.insert(TypeCtor, !AbsImplExpEnumTypeCtors)
        else if
            constructor_list_represents_dummy_argument_type(BothTypesMap,
                Ctors, MaybeEqCmp, MaybeDirectArgCtors)
        then
            set.insert(TypeCtor, !DummyTypeCtors)
        else
            true
        )
    ;
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ).

:- pred accumulate_abs_impl_exported_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_impl_exported_type_rhs(ImplTypeMap, TypeCtor,
        !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules) :-
    ( if map.search(ImplTypeMap, TypeCtor, TypeDefns) then
        list.foldl3(accumulate_abs_eqv_type_rhs_2(ImplTypeMap), TypeDefns,
            !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules)
    else
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_2(type_defn_map::in,
    item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_2(ImplTypeMap, ItemTypeDefnInfo,
        !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules) :-
    ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
    (
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(RhsType),
        type_to_type_ctor_set(RhsType, set.init, RhsTypeCtors),
        set.difference(RhsTypeCtors, !.AbsEqvRhsTypeCtors, NewRhsTypeCtors),
        set.fold(accumulate_modules, NewRhsTypeCtors, !Modules),
        set.union(NewRhsTypeCtors, !AbsEqvRhsTypeCtors),
        set.fold3(accumulate_abs_impl_exported_type_rhs(ImplTypeMap),
            NewRhsTypeCtors, !AbsEqvRhsTypeCtors, set.init, _, !Modules)
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(Ctors, _, _),
        % There must exist a foreign type alternative to this type. As the du
        % type will be exported, we require the types of all the fields.
        ctors_to_type_ctor_set(Ctors, set.init, RhsTypeCtors),
        set.union(RhsTypeCtors, !ForeignDuFieldTypeCtors),
        set.fold(accumulate_modules, RhsTypeCtors, !Modules)
    ;
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        )
    ).

:- pred accumulate_modules(type_ctor::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules(TypeCtor, !Modules) :-
    % NOTE: This assumes that everything has been module qualified.
    TypeCtor = type_ctor(SymName, _Arity),
    (
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified type encountered")
    ).

    % Given a type, return the set of user-defined type constructors
    % occurring in it.
    %
:- pred type_to_type_ctor_set(mer_type::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

type_to_type_ctor_set(Type, !TypeCtors) :-
    ( if type_to_ctor_and_args(Type, TypeCtor, Args) then
        TypeCtor = type_ctor(SymName, _Arity),
        ( if
            type_ctor_is_higher_order(TypeCtor, _, _, _)
        then
            % Higher-order types are builtin so just get the type_ctors
            % from the arguments.
            true
        else if
            type_ctor_is_tuple(TypeCtor)
        then
            % Tuples are builtin so just get the type_ctors from the
            % arguments.
            true
        else if
            is_builtin_type_sym_name(SymName)
        then
            % We don't need to import these modules as the types are builtin.
            true
        else
            set.insert(TypeCtor, !TypeCtors)
        ),
        list.foldl(type_to_type_ctor_set, Args, !TypeCtors)
    else
        true
    ).

:- pred ctors_to_type_ctor_set(list(constructor)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctors_to_type_ctor_set([], !TypeCtors).
ctors_to_type_ctor_set([Ctor | Ctors], !TypeCtors) :-
    Ctor = ctor(_, _, _, ConsArgs, _, _),
    cons_args_to_type_ctor_set(ConsArgs, !TypeCtors),
    ctors_to_type_ctor_set(Ctors, !TypeCtors).

:- pred cons_args_to_type_ctor_set(list(constructor_arg)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

cons_args_to_type_ctor_set([], !TypeCtors).
cons_args_to_type_ctor_set([Arg | Args], !TypeCtors) :-
    Arg = ctor_arg(_, Type, _),
    type_to_type_ctor_set(Type, !TypeCtors),
    cons_args_to_type_ctor_set(Args, !TypeCtors).

%---------------------------------------------------------------------------%

:- type type_defn_map == multi_map(type_ctor, item_type_defn_info).
:- type type_defn_pair == pair(type_ctor, item_type_defn_info).

:- pred gather_type_defns_in_section(module_section::in,
    list(item)::in, list(item)::out,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defns_in_section(Section, Items0, Items, !TypesMap) :-
    gather_type_defns_in_section_loop(Section, Items0, cord.init, ItemsCord,
        !TypesMap),
    Items = cord.list(ItemsCord).

:- pred gather_type_defns_in_section_loop(module_section::in, list(item)::in,
    cord(item)::in, cord(item)::out,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defns_in_section_loop(_, [], !ItemsCord, !TypesMap).
gather_type_defns_in_section_loop(Section, [Item | Items],
        !ItemsCord, !TypesMap) :-
    ( if Item = item_type_defn(ItemTypeDefn) then
        ItemTypeDefn = item_type_defn_info(Name, Args, _, _, _, _),
        TypeCtor = type_ctor(Name, list.length(Args)),
        (
            Section = ms_interface,
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
        ;
            Section = ms_implementation
            % We don't add this to !ItemsCord yet -- we may be removing it.
        ),
        multi_map.set(TypeCtor, ItemTypeDefn, !TypesMap)
    else
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ),
    gather_type_defns_in_section_loop(Section, Items, !ItemsCord, !TypesMap).

%---------------------------------------------------------------------------%

:- pred get_requirements_of_impl_typeclasses_in_items(list(item)::in,
    set(module_name)::out) is det.

get_requirements_of_impl_typeclasses_in_items(ImpItems, Modules) :-
    list.foldl(accumulate_requirements_of_impl_typeclass_in_item,
        ImpItems, set.init, Modules).

:- pred accumulate_requirements_of_impl_typeclass_in_item(item::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_requirements_of_impl_typeclass_in_item(Item, !Modules) :-
    (
        Item = item_typeclass(ItemTypeClass),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(accumulate_requirements_of_impl_from_constraint,
            Constraints, !Modules)
    ;
        ( Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_foreign_import_module(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        )
    ).

:- pred accumulate_requirements_of_impl_from_constraint(prog_constraint::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_requirements_of_impl_from_constraint(Constraint, !Modules) :-
    Constraint = constraint(ClassName, ArgTypes),
    % NOTE: This assumes that everything has been module qualified.
    (
        ClassName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        ClassName = unqualified(_),
        unexpected($module, $pred, "unknown typeclass in constraint")
    ),
    accumulate_modules_from_constraint_arg_types(ArgTypes, !Modules).

:- pred accumulate_modules_from_constraint_arg_types(list(mer_type)::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_from_constraint_arg_types(ArgTypes, !Modules) :-
    list.foldl(accumulate_modules_from_constraint_arg_type, ArgTypes,
        !Modules).

:- pred accumulate_modules_from_constraint_arg_type(mer_type::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_from_constraint_arg_type(ArgType, !Modules) :-
    (
        % Do nothing for these types - they cannot affect the set of
        % implementation imports in an interface file.
        ( ArgType = type_variable(_, _)
        ; ArgType = builtin_type(_)
        )
    ;
        ArgType = defined_type(TypeName, Args, _),
        det_sym_name_get_module_name(TypeName, ModuleName),
        set.insert(ModuleName, !Modules),
        accumulate_modules_from_constraint_arg_types(Args, !Modules)
    ;
        (
            ArgType = tuple_type(Args, _)
        ;
            ArgType = apply_n_type(_, Args, _)
        ;
            ArgType = kinded_type(KindedType, _), Args = [KindedType]
        ;
            ArgType = higher_order_type(_, Args, _HOInstInfo, _, _)
            % XXX accumulate modules from ho_inst_info
        ),
        accumulate_modules_from_constraint_arg_types(Args, !Modules)
    ).

%---------------------------------------------------------------------------%

    % XXX ITEM_LIST Integrate this traversal with other traversals.
    %
:- pred report_and_strip_clauses_in_item_blocks(
    list(item_block(MS))::in, list(item_block(MS))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_and_strip_clauses_in_item_blocks([], [], !Specs).
report_and_strip_clauses_in_item_blocks([ItemBlock0 | ItemBlocks0],
        [ItemBlock | ItemBlocks], !Specs) :-
    ItemBlock0 = item_block(Section, Context, Incls, Avails, Items0),
    report_and_strip_clauses_in_items(Items0, Items, !Specs),
    ItemBlock = item_block(Section, Context, Incls, Avails, Items),
    report_and_strip_clauses_in_item_blocks(ItemBlocks0, ItemBlocks, !Specs).

:- pred report_and_strip_clauses_in_items(list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_and_strip_clauses_in_items(Items0, Items, !Specs) :-
    report_and_strip_clauses_in_items_loop(Items0, [], RevItems, !Specs),
    list.reverse(RevItems, Items).

:- pred report_and_strip_clauses_in_items_loop(list(item)::in,
    list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_and_strip_clauses_in_items_loop([], !RevItems, !Specs).
report_and_strip_clauses_in_items_loop([Item0 | Items0],
        !RevItems, !Specs) :-
    % We either add Item0 to !RevItems, or a new spec to !Specs.
    (
        Item0 = item_clause(ItemClause0),
        Context = ItemClause0 ^ cl_context,
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs]
    ;
        Item0 = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, Context, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs]
        ;
            AllowedInInterface = yes,
            !:RevItems = [Item0 | !.RevItems]
        )
    ;
        ( Item0 = item_type_defn(_)
        ; Item0 = item_inst_defn(_)
        ; Item0 = item_mode_defn(_)
        ; Item0 = item_pred_decl(_)
        ; Item0 = item_mode_decl(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_typeclass(_)
        ; Item0 = item_instance(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_mutable(_)
        ; Item0 = item_foreign_import_module(_)
        ; Item0 = item_type_repn(_)
        ; Item0 = item_nothing(_)
        ),
        !:RevItems = [Item0 | !.RevItems]
    ),
    report_and_strip_clauses_in_items_loop(Items0, !RevItems, !Specs).

:- func clause_in_interface_warning(string, prog_context) = error_spec.

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%---------------------------------------------------------------------------%

:- pred actually_write_interface_file(globals::in, file_name::in,
    parse_tree_int::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file(Globals, _SourceFileName, ParseTreeInt0,
        MaybeTimestamp, !IO) :-
    order_parse_tree_int_contents(ParseTreeInt0, ParseTreeInt1),

    % Create (e.g.) `foo.int.tmp'.
    ModuleName = ParseTreeInt1 ^ pti_module_name,
    IntFileKind = ParseTreeInt1 ^ pti_int_file_kind,
    Suffix = int_file_kind_to_extension(IntFileKind),
    TmpSuffix = Suffix ++ ".tmp",
    module_name_to_file_name(Globals, do_create_dirs, Suffix,
        ModuleName, OutputFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, TmpSuffix,
        ModuleName, TmpOutputFileName, !IO),

    globals.set_option(line_numbers, bool(no),
        Globals, NoLineNumGlobals0),
    globals.set_option(line_numbers_around_foreign_code, bool(no),
        NoLineNumGlobals0, NoLineNumGlobals),
    globals.lookup_bool_option(NoLineNumGlobals, generate_item_version_numbers,
        GenerateVersionNumbers),
    io_get_disable_generate_item_version_numbers(DisableVersionNumbers, !IO),
    ( if
        GenerateVersionNumbers = yes,
        DisableVersionNumbers = no
        % XXX ITEM_LIST We do this for .int2 files as well as .int files.
        % Should we?
    then
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = no,
            unexpected($module, $pred,
                "with `--smart-recompilation', timestamp not read")
        ;
            MaybeTimestamp = yes(Timestamp)
        ),

        % Read in the previous version of the file.
        read_module_int(NoLineNumGlobals,
            "Reading old interface for module", ignore_errors, do_search,
            ModuleName, IntFileKind, _OldIntFileName,
            always_read_module(dont_return_timestamp), _OldTimestamp,
            OldParseTreeInt, _OldSpecs, OldErrors, !IO),
        ( if set.is_empty(OldErrors) then
            MaybeOldParseTreeInt = yes(OldParseTreeInt)
        else
            % If we can't read in the old file, the timestamps will
            % all be set to the modification time of the source file.
            MaybeOldParseTreeInt = no
        ),
        recompilation.version.compute_version_numbers(Timestamp,
            ParseTreeInt1, MaybeOldParseTreeInt, VersionNumbers),
        MaybeVersionNumbers = yes(VersionNumbers)
    else
        MaybeVersionNumbers = no
    ),
    ParseTreeInt = ParseTreeInt1 ^ pti_maybe_version_numbers
        := MaybeVersionNumbers,
    convert_to_mercury_int(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt, !IO),
    % Start using the original globals again.
    update_interface(Globals, OutputFileName, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Given a module interface (the contents of its .int file), extract
    % the short interface part of that module (the contents of its .int2 file).
    % (XXX This is wrong; it is ALSO used to extract the .int3 file from
    % a source file.)
    %
    % This should be the exported type/typeclass/inst/mode declarations,
    % but not the exported pred or constructor declarations. If the module
    % interface imports other modules, then the short interface needs to
    % include those import_module declarations only if the short interface
    % contains some equivalence types or some mode or inst definitions
    % that might use declarations in the imported modules. If the short
    % interface is empty, or only contains abstract type declarations,
    % then it doesn't need any import_module declarations.
    %
:- pred get_short_interface_from_raw_item_blocks(short_int_file_kind::in,
    list(raw_item_block)::in,
    list(item_include)::out, list(item_include)::out,
    list(item_avail)::out, list(item_avail)::out,
    list(item)::out, list(item)::out) is det.

get_short_interface_from_raw_item_blocks(_Kind, [], [], [], [], [], [], []).
get_short_interface_from_raw_item_blocks(Kind, [RawItemBlock | RawItemBlocks],
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems) :-
    get_short_interface_from_raw_item_blocks(Kind, RawItemBlocks,
        IntInclsTail, ImpInclsTail, IntAvailsTail, ImpAvailsTail,
        IntItemsTail, ImpItemsTail),
    RawItemBlock = item_block(Section, _Context, Incls, Avails1, Items0),
    get_short_interface_from_items_acc(Kind, Items0, cord.init, ItemsCord),
    Items1 = cord.list(ItemsCord),
    % XXX ITEM_LIST Integrate maybe_strip_import_decls into
    % get_short_interface_from_items_acc.
    maybe_strip_import_decls(Avails1, Avails, Items1, Items),
    (
        Section = ms_interface,
        IntIncls = Incls ++ IntInclsTail,
        ImpIncls = ImpInclsTail,
        IntAvails = Avails ++ IntAvailsTail,
        ImpAvails = ImpAvailsTail,
        IntItems = Items ++ IntItemsTail,
        ImpItems = ImpItemsTail
    ;
        Section = ms_implementation,
        IntIncls = IntInclsTail,
        ImpIncls = Incls ++ ImpInclsTail,
        IntAvails = IntAvailsTail,
        ImpAvails = Avails ++ ImpAvailsTail,
        IntItems = IntItemsTail,
        ImpItems = Items ++ ImpItemsTail
    ).

:- pred get_short_interface_from_items_acc(short_int_file_kind::in,
    list(item)::in, cord(item)::in, cord(item)::out) is det.

get_short_interface_from_items_acc(_Kind, [], !ItemsCord).
get_short_interface_from_items_acc(Kind, [Item | Items], !ItemsCord) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        maybe_make_abstract_type_defn(Kind,
            ItemTypeDefnInfo, MaybeAbstractItemTypeDefnInfo),
        MaybeAbstractItem = item_type_defn(MaybeAbstractItemTypeDefnInfo),
        !:ItemsCord = cord.snoc(!.ItemsCord, MaybeAbstractItem)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        make_abstract_typeclass(ItemTypeClassInfo, AbstractItemTypeClassInfo),
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        !:ItemsCord = cord.snoc(!.ItemsCord, AbstractItem)
    ;
        Item = item_instance(ItemInstanceInfo),
        maybe_make_abstract_instance(Kind,
            ItemInstanceInfo, MaybeAbstractItemInstanceInfo),
        MaybeAbstractItem = item_instance(MaybeAbstractItemInstanceInfo),
        !:ItemsCord = cord.snoc(!.ItemsCord, MaybeAbstractItem)
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_foreign_import_module(_)
        ),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ;
        ( Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        )
        % Do not include Item in !ItemsCord.
        % XXX TYPE_REPN Is this the right thing to do for item_type_repn?
    ),
    get_short_interface_from_items_acc(Kind, Items, !ItemsCord).

%---------------------------------------------------------------------------%

:- type maybe_need_imports
    --->    dont_need_imports
    ;       need_imports.

:- type maybe_need_foreign_imports
    --->    dont_need_foreign_imports
    ;       need_foreign_imports.

:- pred maybe_strip_import_decls(list(item_avail)::in, list(item_avail)::out,
    list(item)::in, list(item)::out) is det.

maybe_strip_import_decls(Avails0, Avails, Items0, Items) :-
    find_need_imports(Items0, NeedImports, NeedForeignImports),
    (
        NeedImports = need_imports,
        Avails = Avails0
    ;
        NeedImports = dont_need_imports,
        Avails = []
    ),
    (
        NeedForeignImports = need_foreign_imports,
        Items = Items0
    ;
        NeedForeignImports = dont_need_foreign_imports,
        strip_foreign_import_items(Items0, cord.init, ItemsCord),
        Items = cord.list(ItemsCord)
    ).

:- pred find_need_imports(list(item)::in,
    maybe_need_imports::out, maybe_need_foreign_imports::out) is det.

find_need_imports(Items, NeedImports, NeedForeignImports) :-
    find_need_imports_acc(Items,
        dont_need_imports, NeedImports,
        dont_need_foreign_imports, NeedForeignImports).

:- pred find_need_imports_acc(list(item)::in,
    maybe_need_imports::in, maybe_need_imports::out,
    maybe_need_foreign_imports::in, maybe_need_foreign_imports::out) is det.

find_need_imports_acc([], !NeedImports, !NeedForeignImports).
find_need_imports_acc([Item | Items], !NeedImports, !NeedForeignImports) :-
    % XXX ITEM_LIST Should do with one call and one switch.
    ItemNeedsImports = item_needs_imports(Item),
    ItemNeedsForeignImports = item_needs_foreign_imports(Item),
    (
        ItemNeedsImports = yes,
        !:NeedImports = need_imports
    ;
        ItemNeedsImports = no
    ),
    (
        ItemNeedsForeignImports = [_ | _],
        !:NeedForeignImports = need_foreign_imports
    ;
        ItemNeedsForeignImports = []
    ),
    find_need_imports_acc(Items, !NeedImports, !NeedForeignImports).

    % strip_unnecessary_impl_imports(NecessaryModules, !Avails):
    %
    % Remove all import_module and use_module declarations for modules
    % that are not in NecessaryModules.
    %
:- pred strip_unnecessary_impl_imports(set(module_name)::in,
    list(item_avail)::in, list(item_avail)::out) is det.

strip_unnecessary_impl_imports(NecessaryImports, !Avails) :-
    list.filter(is_not_unnecessary_impl_import(NecessaryImports), !Avails).

:- pred is_not_unnecessary_impl_import(set(module_name)::in, item_avail::in)
    is semidet.

is_not_unnecessary_impl_import(NecessaryImports, Avail) :-
    ModuleName = item_avail_module_name(Avail),
    set.member(ModuleName, NecessaryImports).

    % strip_unnecessary_impl_defns_in_items(Items,
    %     NeedForeignImports, IntTypesMap, NecessaryTypeCtors, !ItemsCord):
    %
    % Put all Items into !ItemsCord, except those that are caught by one of
    % these three criteria.
    %
    % 1. If NeedForeignImports is dont_need_foreign_imports, remove all
    %    pragma_foreign_import_module items.
    %
    % 2. Retain only those foreign_enum pragmas that correspond to types
    %    that are actually defined in the interface of the module. (IntTypesMap
    %    maps the types defined in the interface to the information about them
    %    that is visible in the interface.)
    %
    % 3. Remove all type declarations for type constructors that are
    %    not in NecessaryTypeCtors.
    %
:- pred strip_unnecessary_impl_defns_in_items(list(item)::in,
    maybe_need_foreign_imports::in, type_defn_map::in, set(type_ctor)::in,
    cord(item)::in, cord(item)::out) is det.

strip_unnecessary_impl_defns_in_items([], _, _, _, !ItemsCord).
strip_unnecessary_impl_defns_in_items([Item | Items],
        NeedForeignImports, IntTypesMap, NecessaryTypeCtors, !ItemsCord) :-
    (
        Item = item_foreign_import_module(_),
        % XXX ITEM_LIST The foreign imports should be stored outside
        % the item list.
        (
            NeedForeignImports = need_foreign_imports,
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
        ;
            NeedForeignImports = dont_need_foreign_imports
        )
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        ( if Pragma = pragma_foreign_enum(FEInfo) then
            FEInfo = pragma_info_foreign_enum(_Lang, TypeCtor, _Values),
            ( if
                map.search(IntTypesMap, TypeCtor, Defns),
                not (
                    Defns = [Defn],
                    Defn = item_type_defn_info(_, _, Body, _, _, _),
                    Body = parse_tree_abstract_type(_)
                )
            then
                !:ItemsCord = cord.snoc(!.ItemsCord, Item)
            else
                true
            )
        else
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
        )
    ;
        Item = item_type_defn(ItemTypeDefn),
        % Remove all type declarations for type constructors that are
        % not in NecessaryTypeCtors.
        ItemTypeDefn = item_type_defn_info(SymName, Params, _, _, _, _),
        TypeCtor = type_ctor(SymName, list.length(Params)),
        ( if set.member(TypeCtor, NecessaryTypeCtors) then
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
        else
            true
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        ),
        % XXX TYPE_REPN Is this the right thing to do for item_type_repn?
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ),
    strip_unnecessary_impl_defns_in_items(Items,
        NeedForeignImports, IntTypesMap, NecessaryTypeCtors, !ItemsCord).

    % strip_foreign_import_items(Items, !ItemsCord):
    %
    % Does only the first job of strip_unnecessary_impl_defns_in_items
    % when given NeedForeignImports = dont_need_foreign_imports.
    %
:- pred strip_foreign_import_items(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

strip_foreign_import_items([], !ItemsCord).
strip_foreign_import_items([Item | Items], !ItemsCord) :-
    (
        Item = item_foreign_import_module(_)
    ;
        ( Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        ),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ),
    strip_foreign_import_items(Items, !ItemsCord).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%---------------------------------------------------------------------------%
