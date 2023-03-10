NEWS since Mercury 22.01
========================

Changes that may break compatibility
------------------------------------

* The predicates `read_term/3`, `read_term/4`, `read_term_with_op_table/4`,
  `read_term_with_op_table/5` have been removed from the `term_io` module
  of the Mercury standard library.

* The `io` module is being reorganized. Some predicates have been marked
  as obsolete, while other predicates that had been marked as obsolete
  before the release of Mercury 22.01 have been removed.

* The old random number generator in the `random` module has been removed.

* The system of operator priorities in the `ops` module has been inverted.
  Previously, given two operators of different priorities, the one that
  bound more tightly was the one with the lower numerical priority.
  Now, operators with higher numerical priorities bind more tightly.
  There are other changes as well, such as priorities no longer being
  interchangeable with other integers, some types and function symbols
  being renamed, and an updated `op_table` type class with a somewhat
  different list of methods.

  The old contents of the `ops` module is still available as
  `extras/old_library_modules/old_ops`.

* The `mercury_term_parser` module now expects the operator table to be
  specified using the updated `op_table` type class in the new `ops` module.

  The old contents of the `mercury_term_parser` module is still available as
  `extras/old_library_modules/old_mercury_term_parser`.

* The `sparse_bitset`, `fat_sparse_bitset` and `tree_bitset` modules
  now require the items in those sets to be instances of the `uenum` typeclass,
  not the `enum` typeclass.

* The `digraph_key` type in the `digraph` module is now an instance of
  the `uenum` typeclass, not the `enum` typeclass.

* The character sequences `<<u` and `>>u` are now recognized as single tokens.
  (They are the names of new versions of the left and right shift operators
  that expect the shift amount to be specified by an unsigned integer;
  previously, shift amounts were always specified by signed integers.)
  This means that existing code in which a `<<` or `>>` operator was followed
  immediately, without an intervening space, by a `u` character, will now
  be parsed differently.

* The compiler no longer accepts `:` as the module name separator in backquoted
  operators. (Mercury switched to using `.` as the module name separator
  around 2003.)

Changes to the Mercury standard library
---------------------------------------

### Changes to the `array` module

* The following predicates have been removed:

    - pred `random_permutation/4`
      (replacement: `random.shuffle_array/4` or `random.shuffle_array/5`)

    - pred `sort_fix_2014/0`
      (no replacement needed)

* The following function has been marked obsolete:

    - func `array_to_doc/1` (replacement: `pretty_printer.array_to_doc`/1)

### Changes to the `benchmarking` module

* The following predicates have been added:

    - pred `report_stats/3`
    - pred `report_stats/4`
    - pred `report_standard_stats/2`
    - pred `report_standard_stats/3`
    - pred `report_full_memory_stats/2`
    - pred `report_full_memory_stats/3`
    - pred `report_tabling_statistics/2`
    - pred `report_tabling_statistics/3`

* The following predicates have been marked obsolete:

    - pred `report_stats/0`
    - pred `report_full_memory_stats/0`

### Changes to the `bitmap` module

* The following predicates have been added:

    - pred `read_bitmap/6`
    - pred `read_bitmap/7`
    - pred `read_bitmap_range/8`
    - pred `read_bitmap_range/9`
    - pred `write_bitmap/3`
    - pred `write_bitmap/4`
    - pred `write_bitmap_range/5`
    - pred `write_bitmap_range/6`

### Changes to the `builtin` module

* The following obsolete predicate and function have been removed:

    - func `promise_only_solution/1`
    - pred `promise_only_solution_io/4`

### Changes to the `char` module

* The following type has had its typeclass memberships changed:

    - The type `character` is now an instance of the new `uenum` typeclass.

* The following predicate and functions have been added:

    - func `to_uint/1`
    - pred `from_uint/2`
    - func `det_from_uint/1`

* The following function has been marked obsolete:

    - func `char_to_doc/1`   (replacement: `pretty_printer.char_to_doc`/1)

### Changes to the `cord` module

* The following predicates have been added:

    - pred `foldr2/6`
    - pred `foldr3/8`
    - pred `head/2`
    - pred `is_singleton/2`

### Changes to the `counter` module

* The following type has been added:

    - type `ucounter/0`

* The following predicate and function have been added:

    - func `uinit/1`
    - pred `uinit/2`
    - pred `uallocate/1`

### Changes to the `diet` module

* The following obsolete predicate has been removed:

    - pred `empty/1`

### Changes to the `digraph` module

* The following type has had its typeclass memberships changed:

    - The `digraph_key(T)` type is now an instance of the `uenum` typeclass,
     and is no longer an instance of the `enum` typeclass.

* The following functions have been added:

    - func `symmetric_closure/1`            (synonym for `sc/1`)
    - func `transitive_closure/1`           (synonym for `tc/1`)
    - func `reflexive_transitive_closure/1` (synonym for `rtc/1`)

* We have improved the implementations of transitive closure and reflexive
  transitive closure.

### Changes to the `enum` module

* The following typeclass has been added:

    - typeclass `uenum/1`

* The following function has been added:

    - func `det_from_uint/1`

### Changes to the `fat_sparse_bitset` module

* The following type has had its typeclass memberships changed:

    - The `fat_sparse_bitset(T)` type now requires `T` to be an instance of the
     `uenum` typeclass, replacing the earlier requirement that it be
     a member of the `enum` typeclass.

* The documentation of the following predicates and functions have been
  clarified:

    - func `sorted_list_to_set/1`
    - pred `sorted_list_to_set/2`

* The following obsolete predicate has been removed:

    - pred `empty/1`

### New `fatter_sparse_bitset` module

* This new module defines a more compact version of the data structure
  defined by the `fat_sparse_bitset` module, and implements the same set of
  operations on this data structure.

### Changes to the `float` module

* The following function has been marked obsolete:

    - func `float_to_doc/1`   (replacement: `pretty_printer.float_to_doc`/1)

### Changes to the `hash_table` module

* The following predicate has been added:

    - pred `lookup/3`

* The following obsolete predicates have been removed:

    - pred `char_hash/2`
    - pred `float_hash/2`
    - pred `generic_hash/2`
    - pred `int_hash/2`
    - pred `string_hash/2`
    - pred `uint_hash/2`

### Changes to the `int` module

* The following type has had its typeclass memberships changed:

    - The type `int` is now an instance of the new `uenum` typeclass.

* The following predicates and functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`
    - func `ubits_per_int/0`
    - pred `ubits_per_int/1`

* The following function has been marked obsolete:

    - func `int_to_doc/1`    (replacement: `pretty_printer.int_to_doc`/1)

### Changes to the `int8` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `int8_to_doc/1`   (replacement: `pretty_printer.int8_to_doc`/1)

### Changes to the `int16` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `int16_to_doc/1`  (replacement: `pretty_printer.int16_to_doc`/1)

### Changes to the `int32` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `int32_to_doc/1`  (replacement: `pretty_printer.int32_to_doc`/1)

### Changes to the `int64` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `int64_to_doc/1`  (replacement: `pretty_printer.int64_to_doc`/1)

### Changes to the `io` module

* The following predicates and functions have been added:

    - pred `make_io_error_from_system_error/5`
    - pred `make_io_error_from_windows_error/5`
    - pred `get_system_error/2`
    - pred `get_errno_error/2`
    - pred `get_windows_error/2`
    - pred `get_exception_object_error/2`
    - pred `get_system_error_name/2`
    - pred `system_error_is_success/1`
    - func `init_posn/0`
    - pred `write_binary_string_utf8/3`
    - pred `write_binary_string_utf8/4`

* The following obsolete predicates have been removed:

    - pred `see/3`                  (replacement: `prolog.see/3`)
    - pred `see_binary/3`           (replacement: `prolog.see_binary/3`)
    - pred `seen/2`                 (replacement: `prolog.seen/2`)
    - pred `seen_binary/2`          (replacement: `prolog.seen_binary/2`)
    - pred `tell/4`                 (replacement: `prolog.tell/4`)
    - pred `tell_binary/4`          (replacement: `prolog.tell_binary/4`)
    - pred `told/2`                 (replacement: `prolog.told/2`)
    - pred `told_binary/2`          (replacement: `prolog.told_binary/2`)

* The following predicates have been marked obsolete:

    - pred `read_bitmap/6`          (replacement: `bitmap.read_bitmap/6`)
    - pred `read_bitmap/7`          (replacement: `bitmap.read_bitmap/7`)
    - pred `read_bitmap/8`          (replacement: `bitmap.read_bitmap_range/8`)
    - pred `read_bitmap/9`          (replacement: `bitmap.read_bitmap_range/9`)
    - pred `write_bitmap/3`         (replacement: `bitmap.write_bitmap/3`)
    - pred `write_bitmap/4`         (replacement: `bitmap.write_bitmap/4`)
    - pred `write_bitmap/5`         (replacement: `bitmap.write_bitmap_range/5`)
    - pred `write_bitmap/6`         (replacement: `bitmap.write_bitmap_range/6`)

    - pred `report_stats/3`
            (replacement: `benchmarking.report_stats/3`)
    - pred `report_stats/4`
            (replacement: `benchmarking.report_stats/4`)
    - pred `report_standard_stats/2`
            (replacement: `benchmarking.report_standard_stats/2`)
    - pred `report_standard_stats/3`
            (replacement: `benchmarking.report_standard_stats/3`)
    - pred `report_full_memory_stats/2`
            (replacement: `benchmarking.report_full_memory_stats/2`)
    - pred `report_full_memory_stats/3`
            (replacement: `benchmarking.report_full_memory_stats/3`)
    - pred `report_tabling_statistics/2`
            (replacement: `benchmarking.report_tabling_statistics/2`)
    - pred `report_tabling_statistics/3`
            (replacement: `benchmarking.report_tabling_statistics/3`)

    - pred `remove_file/4`
            (replacement: `io.file.remove_file/4`)
    - pred `remove_file_recursively/4`
            (replacement: `io.file.remove_file_recursively/4`)
    - pred `rename_file/5`
            (replacement: `io.file.rename_file/5`)
    - pred `have_symlinks/0`
            (replacement: `io.file.have_symlinks/0`)
    - pred `make_symlink/5`
            (replacement: `io.file.make_symlink/5`)
    - pred `read_symlink/4`
            (replacement: `io.file.read_symlink/4`)
    - pred `check_file_accessibility/5`
            (replacement: `io.file.check_file_accessibility/5`)
    - pred `file_type/5`
            (replacement: `io.file.file_type/5`)
    - pred `file_modification_time/4`
            (replacement: `io.file.file_modification_time/4`)
    - pred `make_temp_file/3`
            (replacement: `io.file.make_temp_file/3`)
    - pred `make_temp_file/6`
            (replacement: `io.file.make_temp_file/6`)
    - pred `make_temp_directory/3`
            (replacement: `io.file.make_temp_directory/3`)
    - pred `make_temp_directory/6`
            (replacement: `io.file.make_temp_directory/6`)
    - pred `have_make_temp_directory/0`
            (replacement: `io.file.have_make_temp_directory/0`)
    - pred `get_temp_directory/3`
            (replacement: `io.file.get_temp_directory/3`)

    - pred `get_environment_var/4`
            (replacement: `io.environment.get_environment_var/4`)
    - pred `set_environment_var/5`
            (replacement: `io.environment.set_environment_var/5`)
    - pred `set_environment_var/4`
            (replacement: `io.environment.set_environment_var/4`)
    - pred `have_set_environment_var/0`
            (replacement: `io.environment.have_set_environment_var/0`)
    - pred `get_environment_var_map/3`
            (replacement: `io.environment.get_environment_var_map/3`)

    - pred `call_system/4`
            (replacement: `io.call_system.call_system/4`)
    - pred `call_system_return_signal/4`
            (replacement: `io.call_system.call_system_return_signal/4`)

    - pred `get_globals/3`          (replacement: a mutable)
    - pred `set_globals/3`          (replacement: a mutable)
    - pred `update_globals/3`       (replacement: a mutable)

    - pred `input_stream_foldl/5`
           (replacement: `stream.input_stream_fold/6`)
    - pred `input_stream_fold/6`
           (replacement: `stream.input_stream_fold/6`)
    - pred `input_stream_foldl_io/4`
           (replacement: `stream.input_stream_fold_state/5`)
    - pred `input_stream_foldl_io/5`
           (replacement: `stream.input_stream_fold_state/5`)
    - pred `input_stream_foldl2_io/5`
           (replacement: `stream.input_stream_foldl_state/6`)
    - pred `input_stream_foldl2_io/6`
           (replacement: `stream.input_stream_fold_state/6`)
    - pred `input_stream_foldl2_io_maybe_stop/5`
           (replacement: `stream.input_stream_fold2_state_maybe_stop/6`)
    - pred `binary_input_stream_foldl/5`)
           (replacement: `stream.input_stream_fold/6`)
    - pred `binary_input_stream_foldl/6`)
           (replacement: `stream.input_stream_fold/6`)
    - pred `binary_input_stream_foldl_io/4`
           (replacement: `stream.input_stream_fold_state/5`)
    - pred `binary_input_stream_foldl2_io/5`
           (replacement: `stream.input_stream_fold2_state/6`)
    - pred `binary_input_stream_foldl2_io/6`
           (replacement: `stream.input_stream_fold2_state/6`)
    - pred `binary_input_stream_foldl2_io_maybe_stop/5`
           (replacement: `stream.input_stream_fold2_state_maybe_stop/6`)
    - pred `binary_input_stream_foldl2_io_maybe_stop/6`
           (replacement: `stream.input_stream_fold2_state_maybe_stop/6`)

### New `io.call_system` module

* This new module has these predicates and functions:

    - pred `call_system/4`
    - pred `call_system_return_signal/4`
    - func `decode_system_command_exit_code/1`

### New `io.environment` module

* This new module has these predicates:

    - pred `get_environment_var/4`
    - pred `set_environment_var/5`
    - pred `set_environment_var/4`
    - pred `have_set_environment_var/0`
    - pred `get_environment_var_map/3`

### New `io.file` module

* This new module has these predicates:

    - pred `remove_file/4`
    - pred `remove_file_recursively/4`
    - pred `rename_file/5`
    - pred `have_symlinks/0`
    - pred `make_symlink/5`
    - pred `read_symlink/4`
    - pred `check_file_accessibility/5`
    - pred `file_type/5`
    - pred `file_modification_time/4`
    - pred `make_temp_file/3`
    - pred `make_temp_file/6`
    - pred `make_temp_directory/3`
    - pred `make_temp_directory/6`
    - pred `have_make_temp_directory/0`
    - pred `get_temp_directory/3`

### Changes to the `list` module

* The following predicates and functions have been added:

    - pred `chunk_foldl/5`
    - pred `chunk_foldl2/7`
    - pred `chunk_foldl3/9`
    - pred `chunk_foldl4/11`
    - pred `gap_foldl/5`
    - pred `head/2`
    - pred `tail/2`
    - pred `det_head/2`
    - pred `det_tail/2`
    - func `inst_preserving_condense/1`
    - pred `intersperse/3`
    - pred `intersperse_list/3`
    - pred `intersperse_list_last/4`
    - pred `is_singleton/2`
    - pred `last_gap_foldl/6`
    - func `take_while_not/2`
    - pred `take_while_not/3`
    - pred `take_while_not/4`

* The following function has been marked obsolete:

    - func `list_to_doc/1`  (replacement: `pretty_printer.list_to_doc`/1)

### Changes to the `one_or_more` module

* The following function has been marked obsolete:

    - func `one_or_more_to_doc/1`
                        (replacement: `pretty_printer.one_or_more_to_doc`/1)

### Changes to the `map` module

* The following predicate has been added:

    - pred `sorted_keys_match/2`

### Changes to the `ops` module

* The representation of operator priorities has been changed. Priorities
  used to be represented as signed integers between 0 and 1200, with
  higher numbers representing operators that bind less tightly.
  Priorities are now represented using unsigned integers between 0u and 1500u,
  with higher numbers representing operators that bind more tightly.
  To guard against priorities being unintentionally confused with
  other kinds of numbers, they are now wrapped in the `prio` function symbol.

* The representation of the set of operators that a string may represent
  has been changed. Operator tables used to map each operator string
  (such as "-") to a list of operator descriptions, each of which could be
  infix, binary prefix, prefix or postfix. This allowed a table entry
  to declare an operator string to be e.g. an infix operator using
  two separate entries, with different priorities. Operator tables now map
  each operator string to a structure which has
  
  - one slot for a description of an infix operator,
  - one slot for a description of a binary prefix operator,
  - one slot for a description of a unary prefix operator, and
  - one slot for a description of a unary postfix operator.

  Besides making some nonsensical entries that could exist in old op tables
  unrepresentable, the new op table also allows users of operator information
  to test more simply and quickly whether an operator string represents
  e.g. a prefix operator.

* The `assoc` type has been renamed to `arg_prio_gt_or_ge`, which
  reflects its semantics. Its function symbols `x` and `y` have been
  renamed to `arg_gt` and `arg_ge` respectively.

* The list of methods in the `op_table` type class has been changed.

 - The method `max_priority/1` has been replaced by the method
   `loosest_op_priority/1`.

 - The method `lookup_op/2` has been renamed to `is_op/2`.

 - The new method `universal_priority/1` has been added. It is intended to
   replace uses of `max_priority + 1` to denote a context that accept terms
   using any operator, regardless of its priority.

 - The new method `tightest_op_priority/1` has been added. It is intended to
   replace uses of the integer constant `0` as a priority.

 - The new method `comma_priority/1` has been added. It is intended to denote
   the priority of the comma character, which separates term arguments,
   when used as an operator.

 - The argument types of the `lookup_op_infos/2` method have been changed.

* The following predicates and functions have been added:

    - func `decrement_priority/1`
    - func `increment_priority/1`
    - func `mercury_op_table_arg_priority/0`
    - func `mercury_op_table_comma_priority/0`
    - func `mercury_op_table_loosest_op_priority/0`
    - func `mercury_op_table_tightest_op_priority/0`
    - func `mercury_op_table_universal_priority/0`
    - pred `mercury_op_table_binary_prefix_op/5`
    - pred `mercury_op_table_infix_op/5`
    - pred `mercury_op_table_lookup_operator_term/3`
    - pred `mercury_op_table_postfix_op/4`
    - pred `mercury_op_table_prefix_op/4`
    - pred `mercury_op_table_search_binary_prefix_op/4`
    - pred `mercury_op_table_search_infix_op/4`
    - pred `mercury_op_table_search_op/1`
    - pred `mercury_op_table_search_op_infos/3`
    - pred `mercury_op_table_search_postfix_op/3`
    - pred `mercury_op_table_search_prefix_op/3`
    - func `min_priority_for_arg/2`
    - pred `priority_ge/2`
    - pred `priority_gt/2`
    - pred `priority_le/2`
    - pred `priority_lt/2`

* The following predicates have been renamed:

    - pred `mercury_op_table_prefix_op/5` to    `op_infos_prefix_op/5`
    - pred `mercury_op_table_binary_prefix_op/5` to
                                                `op_infos_binary_prefix_op/5`
    - pred `mercury_op_table_infix_op/5` to     `op_infos_infix_op/5`
    - pred `mercury_op_table_postfix_op/5` to   `op_infos_postfix_op/5`

### Changes to the `pretty_printer` module

* The following predicates and functions have been added:

    - func `array_to_doc/1`
    - func `char_to_doc/1`
    - func `float_to_doc/1`
    - func `get_formatter_map_entry_types/1`
    - func `int_to_doc/1`
    - func `int8_to_doc/1`
    - func `int16_to_doc/1`
    - func `int32_to_doc/1`
    - func `int64_to_doc/1`
    - func `list_to_doc/1`
    - func `one_or_more_to_doc/1`
    - func `string_to_doc/1`
    - func `tree234_to_doc/1`
    - func `uint_to_doc/1`
    - func `uint8_to_doc/1`
    - func `uint16_to_doc/1`
    - func `uint32_to_doc/1`
    - func `uint64_to_doc/1`
    - func `version_array_to_doc/1`
    - pred `write_doc_formatted/3`
    - pred `write_doc_formatted/4`

### New `ra_list` module

* This new module contains operations on random access lists.

### Changes to the `random` module

* The old random number generator has been removed. It has been replaced by the
  new type class random number generation framework defined in this module.
  As a result, the following predicates have been removed:

    - pred `init/2`
    - pred `random/3`
    - pred `random/5`
    - pred `randmax/3`
    - pred `randcount/3`
    - pred `permutation/4`
            (replacement: `random.shuffle_list/4` or `random.shuffle_list/5`)

### Changes to the `set` module

* The following obsolete predicates and function have been removed:

    - pred `empty/1`
    - pred `non_empty/1` 
    - func `set/1`

### Changes to the `set_bbbtree` module

* The following obsolete predicates have been removed:

    - pred `empty/1`
    - pred `non_empty/1`

### Changes to the `set_ctree234` module

* The following obsolete predicates have been removed:

    - pred `empty/1`
    - pred `non_empty/1`

### Changes to the `set_ordlist` module

* The following obsolete predicates have been removed:

    - pred `empty/1`
    - pred `non_empty/1`

### Changes to the `set_tree234` module

* The following obsolete predicates have been removed:

    - pred `empty/1`
    - pred `non_empty/1`

### Changes to the `set_unordlist` module

* The following obsolete predicates have been removed:

    - pred `empty/1`
    - pred `non_empty/1`

### Changes to the `sparse_bitset` module

* The following type has had its typeclass memberships changed:

    - The `sparse_bitset(T)` type now requires `T` to be an instance of the
     `uenum` typeclass, replacing the earlier requirement that it be
     a member of the `enum` typeclass.

* The following obsolete predicate has been removed:

    - pred `empty/1`

* The documentation of the following predicates and functions have been
  clarified:

    - func `sorted_list_to_set/1`
    - pred `sorted_list_to_set/2`

### Changes to the `std_util` module

* We have changed the behaviour of `pow/3` so that it throws an
  exception if it is called with a negative integer as its second
  argument.

### Changes to the `string` module

* The following predicates and functions have been added:

    - func `between_code_points/3`
    - pred `between_code_points/4`
    - pred `contains_match/2`
    - pred `code_point_offset/3`
    - pred `code_point_offset/4`
    - func `count_code_points/1`
    - pred `count_code_points/2`
    - func `left_by_code_point/2`
    - pred `left_by_code_point/3`
    - func `right_by_code_point/2`
    - pred `right_by_code_point/3`
    - pred `split_by_code_point/3`
    - pred `split_by_code_point/4`

* The following obsolete modes have been removed from the following predicates:

    - pred `to_char_list(uo, in)`
    - pred `to_rev_char_list(in, out)`
    - pred `append(out, out, in)`
    - pred `prefix(in, out)`
    - pred `suffix(in, out)`

* The following predicates and functions have been marked obsolete:

    - func `string_to_doc/1`
                            (replacement: `pretty_printer.string_to_doc`/1)
    - func `between_codepoints/3`    (replacement: `between_code_points`/3)
    - pred `between_codepoints/4`    (replacement: `between_code_points`/4)
    - pred `codepoint_offset/3`      (replacement: `code_point_offset`/3)
    - pred `codepoint_offset/4`      (replacement: `code_point_offset`/4)
    - func `count_codepoints/1`      (replacement: `count_code_points`/1)
    - pred `count_codepoints/2`      (replacement: `count_code_points`/2)
    - func `left_by_codepoint/2`     (replacement: `left_by_code_point`/2)
    - pred `left_by_codepoint/3`     (replacement: `left_by_code_point`/3)
    - func `right_by_codepoint/2`    (replacement: `right_by_code_point`/2)
    - pred `right_by_codepoint/3`    (replacement: `right_by_code_point`/3)
    - pred `split_by_codepoint/3`    (replacement: `split_by_code_point`/3)
    - pred `split_by_codepoint/4`    (replacement: `split_by_code_point'/4`)

### Changes to the `term` module

* The following type has had its typeclass memberships changed:

    - The type `var/1` is now an instance of the new `uenum` typeclass.

* The following predicates have been marked obsolete:

    - pred `generic_term/1`
            (replacement: `with_type` annotations on expressions)

    - pred `decimal_term_to_int/2`
            (replacement: `term_int.decimal_term_to_int/2`)
    - pred `term_to_int/2`
            (replacement: `term_int.term_to_int/2`)
    - pred `term_to_int8/2`
            (replacement: `term_int.term_to_int8/2`)
    - pred `term_to_int16/2`
            (replacement: `term_int.term_to_int16/2`)
    - pred `term_to_int32/2`
            (replacement: `term_int.term_to_in32t/2`)
    - pred `term_to_int64/2`
            (replacement: `term_int.term_to_int64/2`)
    - pred `term_to_uint/2`
            (replacement: `term_int.term_to_uint/2`)
    - pred `term_to_uint8/2`
            (replacement: `term_int.term_to_uint8/2`)
    - pred `term_to_uint16/2`
            (replacement: `term_int.term_to_uint16/2`)
    - pred `term_to_uint32/2`
            (replacement: `term_int.term_to_uin32t/2`)
    - pred `term_to_uint64/2`
            (replacement: `term_int.term_to_uint64/2`)
    - func `int_to_decimal_term/2`
            (replacement: `term_int.int_to_decimal_term/2`)
    - func `int8_to_decimal_term/2`
            (replacement: `term_int.int8_to_decimal_term/2`)
    - func `int16_to_decimal_term/2`
            (replacement: `term_int.int16_to_decimal_term/2`)
    - func `int32_to_decimal_term/2`
            (replacement: `term_int.int32_to_decimal_term/2`)
    - func `int64_to_decimal_term/2`
            (replacement: `term_int.int64_to_decimal_term/2`)
    - func `uint_to_decimal_term/2`
            (replacement: `term_int.uint_to_decimal_term/2`)
    - func `uint8_to_decimal_term/2`
            (replacement: `term_int.uint8_to_decimal_term/2`)
    - func `uint16_to_decimal_term/2`
            (replacement: `term_int.uint16_to_decimal_term/2`)
    - func `uint32_to_decimal_term/2`
            (replacement: `term_int.uint32_to_decimal_term/2`)
    - func `uint64_to_decimal_term/2`
            (replacement: `term_int.uint64_to_decimal_term/2`)

    - pred `occurs/3`
            (replacement: `term_subst.var_occurs_in_subst_term/2`)
    - pred `occurs_list/3`
            (replacement: `term_subst.var_occurs_in_subst_terms/2`)
    - pred `is_ground/1`
            (replacement: `term_subst.term_is_ground/1`)
    - pred `is_ground_in_bindings/2`
            (replacement: `term_subst.term_is_ground_in_bindings/2`)
    - pred `rename_var_in_term/4`
            (replacement: `term_subst.rename_var_in_term/4`)
    - pred `rename_var_in_terms/4`
            (replacement: `term_subst.rename_var_in_terms/4`)
    - pred `apply_renaming_in_var/3`
            (replacement: `term_subst.apply_renaming_in_var/3`)
    - pred `apply_renaming_in_vars/3`
            (replacement: `term_subst.apply_renaming_in_vars/3`)
    - pred `apply_renaming_in_term/3`
            (replacement: `term_subst.apply_renaming_in_term/3`)
    - pred `apply_renaming_in_terms/3`
            (replacement: `term_subst.apply_renaming_in_terms/3`)
    - pred `substitute_var_in_term/4`
            (replacement: `term_subst.substitute_var_in_term/4`)
    - pred `substitute_var_in_terms/4`
            (replacement: `term_subst.substitute_var_in_terms/4`)
    - pred `substitute_corresponding_in_term/3`
            (replacement: `term_subst.substitute_corresponding_in_term/3`)
    - pred `substitute_corresponding_in_terms/3`
            (replacement: `term_subst.substitute_corresponding_in_terms/3`)
    - pred `apply_substitution_in_term/3`
            (replacement: `term_subst.apply_substitution_in_term/3`)
    - pred `apply_substitution_in_terms/3`
            (replacement: `term_subst.apply_substitution_in_terms/3`)
    - pred `apply_rec_substitution_in_term/3`
            (replacement: `term_subst.apply_rec_substitution_in_term/3`)
    - pred `apply_rec_substitution_in_terms/3`
            (replacement: `term_subst.apply_rec_substitution_in_terms/3`)
    - func `term_list_to_var_list/2`
            (replacement: `term_subst.term_list_to_var_list/1`)
    - pred `term_list_to_var_list/2`
            (replacement: `term_subst.term_list_to_var_list/2`)
    - func `var_list_to_term_list/2`
            (replacement: `term_subst.var_list_to_term_list/1`)
    - pred `var_list_to_term_list/2`
            (replacement: `term_subst.var_list_to_term_list/2`)

    - pred `unify_term/4`
            (replacement: `term_unify.unify_terms/4`)
    - pred `unify_term_list/4`
            (replacement: `term_unify.unify_term_lists/4`)
    - pred `unify_term_dont_bind/5`
            (replacement: `term_unify.unify_terms_dont_bind/5`)
    - pred `unify_term_list_dont_bind/5`
            (replacement: `term_unify.unify_term_lists_dont_bind/5`)
    - pred `list_subsumes/3`
            (replacement: `term_unify.first_term_list_subsumes_second/3`)

    - func `vars/1`
            (replacement: `term_vars.vars_in_term/1`)
    - pred `vars/2`
            (replacement: `term_vars.vars_in_term/2`)
    - func `vars_2/2`
            (replacement: `term_vars.vars_in_term_acc/3`)
    - pred `vars_2/3`
            (replacement: `term_vars.vars_in_term_acc/3`)
    - func `vars_list/1`
            (replacement: `term_vars.vars_in_terms/1`)
    - pred `vars_list/2`
            (replacement: `term_vars.vars_in_terms/2`)
    - pred `contains_var/2`
            (replacement: `term_vars.term_contains_var/2`)
    - pred `contains_var_list/2`
            (replacement: `term_vars.terms_contain_var/2`)

    - func `context_init/2`
            (replacement: `term_context.context_init/2`)
    - pred `context_init/3`
            (replacement: `term_context.context_init/2`)
    - func `context_init/0`
            (replacement: `term_context.dummy_context/0`)
    - pred `context_init/1`
            (replacement: `term_context.dummy_context/0`)
    - func `dummy_context_init/0`
            (replacement: `term_context.dummy_context/0`)
    - pred `is_dummy_context/1`
            (replacement: `term_context.is_dummy_context/1`)
    - func `context_file/1`
            (replacement: `term_context.context_file/1`)
    - pred `context_file/2`
            (replacement: `term_context.context_file/1`)
    - func `context_line/1`
            (replacement: `term_context.context_line/1`)
    - pred `context_line/2`
            (replacement: `term_context.context_line/1`)

### New `term_context` module

* This new module defines the `term_context` type, and contains predicates
  and functions that operate on that type.

### New `term_int` module

* This new module contains predicates that recognize terms that contain
  integers, and functions that construct such terms.

### Changes to the `term_io` module

* The following predicates have been removed:

    - pred `read_term/3`
            (replacement: `mercury_term_parser.read_term/3`)
    - pred `read_term/4`
            (replacement: `mercury_term_parser.read_term/4`)
    - pred `read_term_with_op_table/4`
            (replacement: `mercury_term_parser.read_term_with_op_table/4`)
    - pred `read_term_with_op_table/5`
            (replacement: `mercury_term_parser.read_term_with_op_table/5`)

### New `term_subst` module

* This new module contains predicates that perform substitutions
  of various kinds on terms.

### New `term_unify` module

* This new module contains predicates that perform unifications.

### New `term_vars` module

* This new module contains predicates and functions that find
  variables in terms.

### Changes to the `tree_bitset` module

* The following predicates have been added:

    - pred `remove_leq/3`
    - pred `remove_gt/3`

* The documentation of the following predicates and functions have been
  clarified:

    - func `sorted_list_to_set/1`
    - pred `sorted_list_to_set/2`

### Changes to the `tree234` module

* The following predicate has been added:

    - pred `sorted_keys_match/2`

* The following function has been marked obsolete:

    - func `tree234_to_doc/1`
                            (replacement: `pretty_printer.tree234_to_doc`/1)

### Changes to the `tree_bitset` module

* The following obsolete predicate has been removed:

    - pred `empty/1`

### Changes to the `uint` module

* The following type has had its typeclass memberships changed:

    - The type `uint` is now an instance of the new `uenum` typeclass.

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`
    - func `ubits_per_uint/0`

* The following function has been marked obsolete:

    - func `uint_to_doc/1`   (replacement: `pretty_printer.uint_to_doc`/1)

### Changes to the `uint8` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `uint8_to_doc/1`  (replacement: `pretty_printer.uint8_to_doc`/1)

### Changes to the `uint16` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `uint16_to_doc/1` (replacement: `pretty_printer.uint16_to_doc`/1)

### Changes to the `uint32` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `uint32_to_doc/1` (replacement: `pretty_printer.uint32_to_doc`/1)

### Changes to the `uint64` module

* The following functions have been added:

    - func `<<u/2`
    - func `>>u/2`
    - func `unchecked_left_ushift/2`
    - func `unchecked_right_ushift/2`

* The following function has been marked obsolete:

    - func `uint64_to_doc/1` (replacement: `pretty_printer.uint64_to_doc`/1)

### Changes to the `version_array` module

* The following predicate has been added:

    - pred `lookup/3`

* The following function has been marked obsolete:

    - func `version_array_to_doc/1`
                       (replacement: `pretty_printer.version_array_to_doc`/1)

### Changes to the `version_hash_table` module

* The following predicate has been added:

    - pred `lookup/3`

* The following obsolete predicates have been removed:

    - pred `char_hash/2`
    - pred `float_hash/2`
    - pred `generic_hash/2`
    - pred `int_hash/2`
    - pred `string_hash/2`
    - pred `uint_hash/2`

Changes to the Mercury language
-------------------------------

* The new pragma `format_call` can now be used to ask the compiler
  to perform the same checks on calls to a named predicate or function
  as the compiler performs on calls to `string.format`, `io.format` and
  `stream.string_writer.format`. These checks test whether the format string
  matches the provided list of values.

* You can now disable another kind of warning with a `disable_warning` scope.
  Code such as

      disable_warning [unknown_format_calls] (
          ...
      )

  will disable warnings about any calls in the scope to `string.format`,
  `io.format` and/or `stream.string_writer.format` for which the compiler
  cannot tell whether there are any mismatches between the format string
  and the supplied values, even if the module is compiled with
  `--warn-unknown-format-calls`.

Changes to the Mercury compiler
-------------------------------

* We have fixed a bug where `--warn-unused-imports` did not warn about
  unused modules that are also imported by an ancestor of the current module.

* We have added a new option `--warn-ambiguous-pragmas`
  that tells the compiler to generate warnings for pragmas that specify
  a name/arity pair, when there is both a predicate and a function
  with that name and arity, and the pragma does not specify which one
  it is for.

* We have added a new option `--reverse-error-order` that tells the compiler
  to output error messages for higher line numbers before error messages
  for lower line numbers.

* We have added a new option `--show-local-call-tree`
  that tells the compiler to output a representation of the call tree
  of the predicates and functions of the module as ordered by a depth-first
  left-to-right traversal of the bodies of the first (i.e. main) procedures
  of those predicates and functions. This can suggest a good order
  for the positions of those predicates and functions in the source code.

* The deprecated option `--trail-segments` has been deleted and the grade
  component `trseg` is no longer accepted as a synonym for `tr`.

* `--javac-flags` is now accepted as a synonym for `--java-flags`. Similarly
  `--javac-flag` is now accepted as a synonym for `--java-flags`.

* The new options `--java-runtime-flags` and `--java-runtime-flags` can be
  used to set flags for the Java interpreter in the launcher shell scripts or
  batch files generated by the compiler when creating executables in the `java`
  grade.

* On MacOS systems, the configure script now uses the value of the
  `MACOSX_DEPLOYMENT_TARGET` environment variable as the default value for the
  deployment target, if set.

Changes to the Mercury debugger
-------------------------------

* The `dump` command has a new option: `dump -p Var` will dump the value
  of the given variable in a prettyprinted form.

Portability improvements
------------------------

* We have updated the script `tools/configure_cross` to support
  cross-compiling using clang.

Changes to the extras distribution
----------------------------------

* We have deleted the `old_term_parser` library.

NEWS for Mercury 22.01.6
========================

This is a bug-fix release.

* We have fixed the implementations of transitive closure and reflexive
  transitive closure in the `digraph` module.

* We have a fixed a bug in `string.format` and related predicates where `int8`,
  `int16` and `int32` values were being incorrectly promoted to `int` and sign
  extended when one of the unsigned conversion specifiers was applied.

NEWS for Mercury 22.01.5
========================

This is a bug-fix release.

* We have fixed a bug that was causing invalid fact table file names
  to be written to dependency files.

* We have fixed a bug with the MLDS backend that was causing
  crashes with certain switches on strings.

* We have fixed a bug that was causing some transitive intermodule
  optimization interfaces to be silently ignored.

* We have fixed a bug where termination analysis could cause a compiler abort
  when invoked on some rarely-occurring predicate structures.

NEWS for Mercury 22.01.4
========================

This is a bug-fix release.

* We have fixed and clarified the behaviour of the following `string`
  module predicates when called on a string containing ill-formed code
  unit sequences:

    - pred `all_match/2`
    - pred `index_next_repl/5`
    - pred `unsafe_index_next_repl/5`
    - pred `prev_index_repl/5`
    - pred `unsafe_prev_index_repl/5`

* We have fixed a bug in the `getopt` and `getopt_io` modules where
  negating a `maybe_string` option would set its option table entry
  to an incorrect value.

* We have fixed an issue where flags specific to GCC were being
  passed to MSVC when building Mercury using MSVC.

NEWS for Mercury 22.01.3
========================

This is a bug-fix release.

* [Github issue #103]. We have disabled the use of some GCC optimizations
  that were causing segmentation faults in code compiled in the `asm_fast`
  grades with GCC versions 11 and 12 on x86_64 systems.

NEWS for Mercury 22.01.2
========================

This is a bug-fix release.

* [Mantis bug #558]. We have fixed a bug that was causing some of the
  manual pages to sometimes build incorrectly.

* [Mantis bug #560]. We have disabled the use of the `asm_fast*` grades on
  AArch64 when using GCC 9+ due to incompatibilities with position-independent
  code.

* We have fixed a bug that was causing assertion failures or segmentation
  violations in the Mercury runtime on AArch64 systems with versions of
  GCC before 10.

* [Github issue #102]. We have disabled the use of the GCC option
  `-Warray-bounds` for GCC version 12 and onwards. This is due to GCC
  emitting spurious warnings when that option is enabled.  (The use of
  that option was already disabled for GCC versions 9-11.)

NEWS for Mercury 22.01.1
========================

This is a bug-fix release.

* [Mantis bug #557]. We have fixed a bug in module qualification that was
  causing the compiler to fail without printing an error message.

NEWS for Mercury 22.01
======================

Changes that may break compatibility
------------------------------------

* We have removed `is` as a synonym for unification.

* We have reserved `=<`/2 as a type name.

* A term with a top-level functor `coerce/1` is now treated as a
  type conversion expression. To call a function named `coerce/1`,
  you can module qualify the name at the call site,
  or wrap parentheses around the name, e.g. `(coerce)(Arg)`.

* We have renamed the `lexer` and `parser` modules of the Mercury standard
  library to `mercury_term_lexer` and `mercury_term_parser` respectively.

* We have made slight changes to the names and/or the functionality
  of several predicates in the `getopt` and `getopt_io` modules.

* We have removed the legacy support for the Alpha architecture.

* We have dropped support for macOS 10.8 and earlier.

* We have removed the Erlang backend as it was unmaintained.

Changes to the Mercury standard library
---------------------------------------

### New module: `random.system_rng`

* This module provides an interface to a platform specific cryptographically
  secure random number generator that is seeded from the OS entropy pool.

### Changes to the `array` module

* The following obsolete predicates and functions have been removed:

    - func `bsearch/3`              (replacement: `binary_search/3`)
    - pred `bsearch/4`              (replacement: `binary_search/4`)
    - func `least_index/1`          (replacement: `min/1`)
    - func `greatest_index/1`       (replacement: `max/1`)

* The following predicate has been added:

    - pred `generate_foldl2/7`

### Changes to the `array2d` module

* The following predicates and functions have been added:

    - func `lookup/3`               (synonym for `Array ^ elem(R, C)`)
    - pred `lookup/4`               (synonym for `Array ^ elem(R, C)`)
    - func `unsafe_lookup/3`        (synonym for `Array ^ unsafe_elem(R, C)`)
    - pred `unsafe_lookup/4`        (synonym for `Array ^ unsafe_elem(R, C)`)

* The `lists/1` function now returns an empty list for a 0x0 array.

### Changes to the `assoc_list` module

* The following predicates and functions have been added:

    - func `common_subset/2`
    - pred `maybe_from_corresponding_lists/3`

### Changes to the `bag` module

* The following obsolete predicates and functions have been removed:

    - func `to_set_without_duplicates/1`    (replacement: func `to_set/1`)
    - pred `to_set_without_duplicates/2`    (replacement: func `to_set/1`)

### Changes to the `bitmap` module

* The following predicates and functions have been added:

    - func `get_bit/2`
    - func `get_bits/2`
    - func `get_byte/2`
    - pred `set_bit/4`
    - pred `set_bits/4`
    - pred `set_byte/4`
    - func `unsafe_get_bit/2`
    - func `unsafe_get_bits/2`
    - func `unsafe_get_byte/2`
    - pred `unsafe_set_bit/4`
    - pred `unsafe_set_bits/4`
    - pred `unsafe_set_byte/4`

### Changes to the `char` module

* The following predicates have been added:

    - pred `unsafe_base_digit_to_int/3`
    - pred `to_utf8_uint8/2`
    - pred `to_utf16_uint16/2`

* The following obsolete predicates and functions have been removed:

    - func `det_int_to_digit/1`     (replacement: `det_int_to_decimal_digit/1`)
    - pred `det_int_to_digit/2`     (replacement: `det_int_to_decimal_digit/2`)
    - pred `digit_to_int/2`         (replacement: `decimal_digit_to_int/2`)
    - pred `int_to_digit/2`         (replacement: `int_to_decimal_digit/2`)

### Changes to the `cord` module

* The following predicates have been added:

    - pred `foldl2/6`
    - pred `foldl3/8`

### Changes to the `dir` module

* The following predicate has been added:

    - pred `general_foldl2/8`

### Changes to the `getopt` module

* The following new predicates have been added:

    - pred `record_arguments/8`
    - pred `expand_file_specials/8`

* The following variants of the existing process_options predicate
  have been added:

    - pred `process_options_io/6`
    - pred `process_options_io/7`
    - pred `process_options_track_io/9`
    - pred `process_options_userdata/8`
    - pred `process_options_userdata_io/10`

    The ones whose names have an `_io` suffix do the same jobs as the
    corresponding predicates without the `_io` suffix, with the exception
    that they also have a pair of I/O state arguments that allow them
    to implement `file_special` options. This functionality used to be
    available only from the `getopt_io` module.

* The following predicates have had their argument types changed:

    - pred `process_options/6`
    - pred `process_options/7`
    - pred `process_options_track/7`

    All these predicates used to return error indications in the form of a
    simple string. They now return error indications using the existing
    structured type `option_error`, which can be converted into a string
    on demand.

* The following predicates have been deleted:

    - pred `process_options_se/6`
    - pred `process_options_se/7`
    - pred `process_options_track_se/7`

    Their functionality is now available from the predicates with the same name
    minus the `_se` suffix.

### Changes to the `getopt_io` module

* This module has been deprecated. For now, it exports the same functionality
  as the updated `getopt` module, but it is scheduled to be deleted after
  the next release.

### Changes to the `int` module

* The following functions have been added:

    - func `uint_to_lc_hex_string/2`    (synonym for `uint_to_hex_string/2`)
    - func `uint64_to_lc_hex_string/2`  (synonym for `uint64_to_hex_string/2`)

* The following obsolete predicates and functions have been removed:

    - pred `is/2`                   (replacement: `=`, i.e. unification)
    - func `legacy_left_shift/2`    (replacement: `<<`)
    - func `legacy_right_shift/2`   (replacement: `>>`)

### Changes to the `int32` module

* The following functions have been added:

    - func `cast_to_int8`
    - func `cast_from_int8`
    - func `cast_to_int16`
    - func `cast_from_int16`
    - func `cast_to_int64`
    - func `cast_from_int64`

### Changes to the `integer` module

* The following obsolete functions have been removed:

    - func `from_base_string/2`     (replacement: pred `from_base_string/3`)
    - func `from_string/2`          (replacement: pred `from_string/2`)
    - func `int/2`                  (replacement: `det_to_int/1`)

### Changes to the `io` module

* The following predicates have been added to this module:

    - pred `get_environment_var_map/3`
    - pred `read_binary_int8_unboxed/5`
    - pred `read_binary_uint8_unboxed/5`
    - pred `read_named_file_as_string/4`
    - pred `read_named_file_as_lines/4`
    - pred `write_line_cc/4`

* The following obsolete predicates have been removed:

    - pred `make_temp/3`            (replacement: `make_temp_file/3`)
    - pred `make_temp/5`            (replacement: `make_temp_file/5`)

* The following predicates have been marked as obsolete:

    - pred `see/4`                  (replacement: `prolog.see/4`)
    - pred `see_binary/4`           (replacement: `prolog.see_binary/4`)
    - pred `seen/2`                 (replacement: `prolog.seen/2`)
    - pred `seen_binary/2`          (replacement: `prolog.seen_binary/2`)
    - pred `tell/4`                 (replacement: `prolog.tell/4`)
    - pred `tell_binary/4`          (replacement: `prolog.tell_binary/4`)
    - pred `told/2`                 (replacement: `prolog.told/2`)
    - pred `told_binary/2`          (replacement: `prolog.told_binary/2`)

* The following predicate has been renamed:

    - pred `report_stats/2` to `report_standard_stats/2`.

### Changes to the `lexer` module

* This module has been renamed to `mercury_term_lexer`, to make
  name clashes between it and user-written modules less likely.

### Changes to the `list` module

* The following predicates have been added:

    - pred `delete_nth/3`
    - pred `foldl7/16`
    - pred `foldl8/18`
    - pred `foldl4_corresponding/11`

* The following obsolete predicate has been removed:

    - pred `takewhile/4`            (replacement: `take_while/4`)

### Changes to the `map` module

* The following predicates have been added to this module:

    - pred `foldl6/14`
    - pred `foldl6_values/14`
    - pred `foldr6/14`

### Changes to the `maybe` module

* The following predicates have been added to this module:

    - pred `foldl3_maybe/8`
    - pred `foldl4_maybe/10`
    - pred `foldl5_maybe/12`
    - pred `map_foldl4_maybe/11`
    - pred `map_foldl5_maybe/13`

### Changes to the `parser` module

* This module has been renamed to `mercury_term_parser`, to make
  name clashes between it and user-written modules less likely.

### Changes to the `prolog` module

* The following predicate has been added to this module:

    - pred `is/2`                   (moved here from the `int` module)
    - pred `see/4`                  (moved here from the `io` module)
    - pred `see_binary/4`           (moved here from the `io` module)
    - pred `seen/2`                 (moved here from the `io` module)
    - pred `seen_binary/2`          (moved here from the `io` module)
    - pred `tell/4`                 (moved here from the `io` module)
    - pred `tell_binary/4`          (moved here from the `io` module)
    - pred `told/2`                 (moved here from the `io` module)
    - pred `told_binary/2`          (moved here from the `io` module)

### Changes to the `random` module

* The following obsolete predicate has been removed:

    - pred `test/4`                 (replacement: none)

### Changes to the `std_util` module

* The following obsolete predicates and functions have been removed:

    - func `maybe_func/2`           (replacement: func `maybe.func_to_maybe/1`)
    - pred `maybe_pred/3`           (replacement: func `maybe.pred_to_maybe/1`)

### Changes to the `string` module

* The following functions have been added:

    - func `add_suffix/2`
    - func `split_into_lines/1`
    - pred `to_uint/2`
    - func `det_to_uint/1`
    - pred `base_string_to_uint/3`
    - func `det_base_string_to_uint/2`
    - func `uint_to_hex_string/1`
    - func `uint_to_uc_hex_string/1`
    - func `uint_to_octal_string/1`
    - func `uint64_to_hex_string/1`
    - func `uint64_to_uc_hex_string/1`
    - func `uint64_to_octal_string/1`

* The following function symbols have been added to the type `poly_type`:

    - `i8(int8)`
    - `i16(int16)`
    - `i32(int32)`
    - `i64(int64)`
    - `u8(uint8)`
    - `u16(uint16)`
    - `u32(uint32)`
    - `u64(uint64)`

  This allows predicates such as `string.format` and `io.format` to operate
  on values of not just the word sized integer types `int` and `uint`, but on
  sized versions of them as well.

### Changes to the `term` module

* The following obsolete predicates and functions have been removed:

    - func `var_id/1`               (replacement: `var_to_int/1`)

    - func `relabel_variable/3`     (replacement: `rename_var_in_term/4`)
    - pred `relabel_variable/4`     (replacement: `rename_var_in_term/4`)
    - func `relabel_variables/3`    (replacement: `rename_var_in_terms/4`)
    - pred `relabel_variables/4`    (replacement: `rename_var_in_terms/4`)

    - func `rename/3`               (replacement: `rename_var_in_term/4`)
    - pred `rename/4`               (replacement: `rename_var_in_term/4`)
    - func `rename_list/3`          (replacement: `rename_var_in_terms/4`)
    - pred `rename_list/4`          (replacement: `rename_var_in_terms/4`)

    - func `apply_renaming/3`       (replacement: `apply_renaming_in_term/3`)
    - pred `apply_renaming/3`       (replacement: `apply_renaming_in_term/3`)
    - func `apply_renaming_to_list/3`
                                    (replacement: `apply_renaming_in_terms/3`)
    - pred `apply_renaming_to_list/3`
                                    (replacement: `apply_renaming_in_terms/3`)

    - func `apply_variable_renaming/2`
                                    (replacement: `apply_renaming_in_term/3`)
    - pred `apply_variable_renaming/3`
                                    (replacement: `apply_renaming_in_term/3`)
    - func `apply_variable_renaming_to_list/2`
                                    (replacement: `apply_renaming_in_terms/3`)
    - pred `apply_variable_renaming_to_list/3`
                                    (replacement: `apply_renaming_in_terms/3`)

    - func `apply_variable_renaming_to_var/2`
                                    (replacement: `apply_renaming_in_var/3`)
    - pred `apply_variable_renaming_to_var/3`
                                    (replacement: `apply_renaming_in_var/3`)
    - func `apply_variable_renaming_to_vars/2`
                                    (replacement: `apply_renaming_in_vars/3`)
    - pred `apply_variable_renaming_to_vars/3`
                                    (replacement: `apply_renaming_in_vars/3`)

    - func `substitute/3`           (replacement: `substitute_var_in_term/4`)
    - pred `substitute/4`           (replacement: `substitute_var_in_term/4`)
    - func `substitute_list/3`      (replacement: `substitute_var_in_terms/4`)
    - pred `substitute_list/4`      (replacement: `substitute_var_in_terms/4`)

    - func `substitute_corresponding/3`
                        (replacement: `substitute_corresponding_in_term/4`)
    - pred `substitute_corresponding/4`
                        (replacement: `substitute_corresponding_in_term/4`)
    - func `substitute_corresponding_list/3`
                        (replacement: `substitute_corresponding_in_terms/4`)
    - pred `substitute_corresponding_list/4`
                        (replacement: `substitute_corresponding_in_terms/4`)

    - func `apply_substitution/2`
                        (replacement: `apply_substitution_in_term/3`)
    - pred `apply_substitution/3`
                        (replacement: `apply_substitution_in_term/3`)
    - func `apply_substitution_to_list/2`
                        (replacement: `apply_substitution_in_terms/3`)
    - pred `apply_substitution_to_list/3`
                        (replacement: `apply_substitution_in_terms/3`)

    - func `apply_rec_substitution/2`
                        (replacement: `apply_rec_substitution_in_term/3`)
    - pred `apply_rec_substitution/3`
                        (replacement: `apply_rec_substitution_in_term/3`)
    - func `apply_rec_substitution_to_list/2`
                        (replacement: `apply_rec_substitution_in_terms/3`)
    - pred `apply_rec_substitution_to_list/3`
                        (replacement: `apply_rec_substitution_in_terms/3`)

### Changes to the `thread` module

* The following predicate and functions have been added:

    - func `init_thread_options/0`
    - pred `set_min_stack_size/3`
    - pred `spawn_native/5`

### Changes to the `thread.mvar` module

* The following obsolete function has been removed:

    - func `init/1`                 (replacement: `impure_init/1`)

### Changes to the `thread.semaphore` module

* The following obsolete function has been removed:

    - func `init/1`                 (replacement: `impure_init/1`)

### Changes to the `time` module

* The following obsolete functions have been removed:

    - func `ctime/1`                (replacement: `localtime/4` and `asctime/1`)
    - func `localtime/1`            (replacement: `localtime/4`)
    - func `mktime/1`               (replacement: `mktime/4`)

### Changes to the `tree234` module

* The following predicates have been added to this module:

    - pred `foldl6/14`
    - pred `foldl6_values/14`
    - pred `foldr6/14`

### Changes to the `uint16` module

* The following functions have been added:

    - pred `from_uint/2`
    - func `det_from_uint/1`
    - func `cast_from_uint/1`
    - func `rotate_left/2`
    - func `rotate_right/2`
    - func `unchecked_rotate_left/2`
    - func `unchecked_rotate_right/2`
    - func `set_bit/2`
    - func `unchecked_set_bit/2`
    - func `clear_bit/2`
    - func `unchecked_clear_bit/2`
    - func `flip_bit/2`
    - func `unchecked_flip_bit/2`
    - func `bit_is_set/2`
    - func `unchecked_bit_is_set/2`
    - func `bit_is_clear/2`
    - func `unchecked_bit_is_clear/2`
    - func `cast_from_uint8/1`
    - func `cast_to_uint8/1`

### Changes to the `uint32` module

* The following functions have been added:

    - pred `from_uint/2`
    - func `det_from_uint/1`
    - func `rotate_left/2`
    - func `rotate_right/2`
    - func `unchecked_rotate_left/2`
    - func `unchecked_rotate_right/2`
    - func `set_bit/2`
    - func `unchecked_set_bit/2`
    - func `clear_bit/2`
    - func `unchecked_clear_bit/2`
    - func `flip_bit/2`
    - func `unchecked_flip_bit/2`
    - func `bit_is_set/2`
    - func `unchecked_bit_is_set/2`
    - func `bit_is_clear/2`
    - func `unchecked_bit_is_clear/2`
    - func `cast_from_uint8/1`
    - func `cast_to_uint8/1`
    - func `cast_from_uint16/1`
    - func `cast_to_uint16/1`

### Changes to the `uint64` module

* The following functions have been added:

    - func `cast_from_uint/1`
    - func `rotate_left/2`
    - func `rotate_right/2`
    - func `unchecked_rotate_left/2`
    - func `unchecked_rotate_right/2`
    - func `set_bit/2`
    - func `unchecked_set_bit/2`
    - func `clear_bit/2`
    - func `unchecked_clear_bit/2`
    - func `flip_bit/2`
    - func `unchecked_flip_bit/2`
    - func `bit_is_set/2`
    - func `unchecked_bit_is_set/2`
    - func `bit_is_clear/2`
    - func `unchecked_bit_is_clear/2`
    - func `cast_from_uint8/1`
    - func `cast_to_uint8/1`

### Changes to the `uint8` module

* The following functions have been added:

    - pred `from_uint/2`
    - func `det_from_uint/1`
    - func `cast_from_uint/1`
    - func `rotate_left/2`
    - func `rotate_right/2`
    - func `unchecked_rotate_left/2`
    - func `unchecked_rotate_right/2`
    - func `set_bit/2`
    - func `unchecked_set_bit/2`
    - func `clear_bit/2`
    - func `unchecked_clear_bit/2`
    - func `flip_bit/2`
    - func `unchecked_flip_bit/2`
    - func `bit_is_set/2`
    - func `unchecked_bit_is_set/2`
    - func `bit_is_clear/2`
    - func `unchecked_bit_is_clear/2`

### Changes to the `varset` module

* The following obsolete predicates have been removed:

    - pred `merge_subst/4`      (replacement: `merge_renaming/4`)
    - pred `merge_subst_without_names/4`
                                (replacement: `merge_renaming_without_names/4`)

* The following functions and predicates have been added:

    - func `unname_var/2`
    - pred `unname_var/3`
    - pred `undo_default_names/2`

Changes to the Mercury language
-------------------------------

* The type system now supports subtypes, which work in tandem with
  type conversion expressions ("coerce"). For example, the following
  defines a subtype `real_color` of a discriminated union type `color`:

        :- type color
            --->    rgb(float, float, float)
            ;       cmyk(float, float, float, float)
            ;       named(string).

        :- type real_color =< color
            --->    rgb(float, float, float)
            ;       cmyk(float, float, float, float).

  A term of type `real_color` can be converted to a term of type `color`
  with `coerce(Term)`. A term of type `color` can be converted to a term of
  type `real_color` if it has an appropriate inst. Subtypes share a common
  data representation with their base types, so the type conversions do not
  cost anything at runtime.

* Field names no longer need to be unique within a module.

* The compiler can implement tabling only when generating C code.
  When compiling a predicate that has a `pragma memo` specified for it
  in a non-C grade, it necessarily ignores the pragma, but normally
  it prints a warning about this fact. The compiler now supports
  a new attribute, `disable_warning_if_ignored`, that suppresses
  such warnings for a `pragma memo` if included in the pragma's
  attribute list, like this:

        :- pragma memo(predname/arity, [disable_warning_if_ignored])]).

* A `pragma foreign_proc` declaration can now include an attribute
  `may_not_export_body` that prevents its body (i.e. the foreign code)
  from being duplicated outside of the target file for that module
  by intermodule optimization. This is useful when the foreign code
  refers to types, functions, etc. that should be kept local to the
  target file of that module. Unlike the `may_not_duplicate` attribute,
  `may_not_export_body` does not prevent inlining of the foreign procedure
  into other procedures in the same module.

* Many pragmas contain a name/arity pair for specifying the predicate
  or function they apply to; an example is

        :- pragma inline(init/1).

  If the module in which this pragma occurs contains both
  a function `init/1` and a predicate `init/1`, then this pragma
  is ambiguous. Traditionally, the Mercury compiler applied such
  ambiguous pragmas to both the function and the predicate.
  The programmers who wished the pragma to apply to only one of them
  had to rename the other.

  Now, pragmas that take a name/arity pair can specify whether they are
  intended to apply to a function or to a predicate by putting a `func()`
  or `pred()` wrapper around the name/arity pair, like this:

        :- pragma inline(func(init/1)).     % applies ONLY to func init/1.
        :- pragma inline(pred(init/1)).     % applies ONLY to pred init/1.

  This new syntax applies to all of the following kinds of pragmas:

        check_termination
        consider_used
        does_not_terminate
        fact_table
        inline
        loop_check
        memo
        minimal_model
        mode_check_clauses
        no_determinism_warning
        no_inline
        obsolete
        promise_equivalent_clauses
        promise_pure
        promise_semipure
        require_tail_recursion
        terminates
        type_spec

Changes to the Mercury compiler
-------------------------------

* If a command line first enables an optimization (say `opt1`), and then
  sets the optimization level to `N` with `-O<N>`, then the compiler will now
  keep `opt1` enabled even if `opt1` is not normally enabled at optimization
  level `N`.

* Due to a bug fix, the `--warn-unused-imports` option is now stricter in
  warning about modules that are imported in the interface section but are
  not used in the interface section.

* We have fixed parsing of reverse implication goals (A <= B).

* By default, the compiler now checks some aspects of a module semantics
  when generating the .int and .int2 interface files for that module.
  (For example, it generates error messages for references to undefined
  types, insts and modes.) This new behavior can be switched off for now
  with the new option `--no-halt-at-invalid-interface`. This new option
  replaces the old `--no-print-errors-warnings-when-generating-interface`
  option, but once any problems caused by the new approach have been
  ironed out, we intend to delete the `--halt-at-invalid-interface`
  option as well.

* In an earlier release, we extended the syntax of `:- inst` declarations
  to allow programmers to specify which type constructor's values the inst
  is meant for. At that time, these functioned only as documentation,
  but the compiler now reports error messages for situations in which
  an inst that was declared to be intended for values of one type constructor
  is applied to values of another type constructor.

* The new option `--output-stdlib-grades` outputs the grades in which
  the Mercury standard library is available with this compiler.

* By default, the compiler now warns if the module being compiled has a name
  that shadows that of Mercury standard library module. The warning can be
  disabled using the new option `--no-warn-stdlib-shadowing`.

* The new options `--output-java-class-dir` and `--output-java-class-directory`
  are now supported as synonyms for `--output-class-dir`.

* The new option `--halt-at-warn-make-interface` causes the compiler to
  treat all warnings as if they were errors when generating interface files.

* The new option `--halt-at-warn-make-opt` causes the compiler to
  treat all warnings as if they were errors when generating optimization files.

* The new option `--warn-potentially-ambiguous-pragma` causes the compiler to
  generate a warning for all pragmas that include a name/arity pair,
  but lack a `func()` or `pred()` wrapper around it to indicate whether they
  are intended to apply to a function or to a predicate.

Portability improvements
------------------------

* We have ported Mercury to Linux AArch64 (ARM64) systems.

* The `tools/configure_mingw_cross` script has been replaced by
  `tools/configure_cross`. It now supports aarch64-linux-gnu and
  aarch64-linux-musl as targets (i.e. Linux on aarch64 with GNU or
  musl C libraries).

Changes to the Mercury debugger
-------------------------------

* The `list` command may now call an external command to print source listings;
  the command is set using `list_cmd`.  For example, the command could
  produce syntax highlighted source listings.

* We have removed support for browsing terms as XML (`browse --xml`)
  as it was unmaintained and did not work any more. The `browse --web`
  command provides an alternative method for interactively exploring a term.

For news about earlier versions, see the HISTORY file.
