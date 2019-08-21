%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines a type used by several modules that parse items
% and/or markers.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_types.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module list.

    % This type represents the result of parsing one term.
:- type item_or_marker
    --->    iom_item(item)
            % The term contains an item.

    ;       iom_marker_include(one_or_more(item_include))
            % The term contains an `:- include_module' declaration.

    ;       iom_marker_avail(one_or_more(item_avail))
            % The term contains an `:- import_module' or `:- use_module'
            % declaration.

    ;       iom_marker_fim(item_fim)
            % The term contains a `:- pragma foreign_import_module'
            % declaration.

    ;       iom_marker_version_numbers(version_numbers)
            % The term was a record of the version numbers of the items
            % in an interface file.

    ;       iom_marker_src_file(string)
            % The term was a pragma specifying the new filename.

    ;       iom_marker_module_start(module_name, prog_context, int)
            % The term was a `:- module' declaration. The arguments give
            % the module's name, and the context and sequence number of the
            % declaration. The module name is exactly what was in the
            % declaration; it is NOT implicitly module qualified by the
            % enclosing module.

    ;       iom_marker_module_end(module_name, prog_context, int)
            % The term was a `:- end_module' declaration. The arguments give
            % the module's name, and the context and sequence number of the
            % declaration. Again, the module name as is, and not implicitly
            % module qualified.

    ;       iom_marker_section(module_section, prog_context, int)
            % The term was a `:- interface' or `:- implementation' declaration.
            % The arguments give the section's kind, and the context
            % and sequence number of the declaration.

    ;       iom_handled(list(error_spec)).
            % The term was completely dealt with during parsing, which
            % may have generated some messages. If it did, they are attached;
            % otherwise, the list will be empty.
            %
            % As of this writing, this is used only for require_feature_set
            % pragmas that don't require any features, and for version_number
            % items recorded by old compiler versions in a now-obsolete format.

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_types.
%---------------------------------------------------------------------------%
