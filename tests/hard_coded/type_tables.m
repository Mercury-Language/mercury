%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for runtime/mercury_type_tables.h
%
% Author: zs

:- module type_tables.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type list
    --->    a
    ;       b.

:- pragma foreign_decl("C", "
extern const struct MR_TypeCtorInfo_Struct
    mercury_data_list__type_ctor_info_list_1;
extern const struct MR_TypeCtorInfo_Struct
    mercury_data_type_tables__type_ctor_info_list_0;
").

:- pragma foreign_proc("C",
    main(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    MR_TypeCtorInfo tc1;
    MR_TypeCtorInfo tc2;

    MR_register_type_ctor_info(&mercury_data_list__type_ctor_info_list_1);
    MR_register_type_ctor_info(&mercury_data_type_tables__type_ctor_info_list_0);

    tc1 = MR_lookup_type_ctor_info(""list"", ""list"", 1);
    tc2 = MR_lookup_type_ctor_info(""type_tables"", ""list"", 0);

    printf(""%s %s\\n"",
        MR_type_ctor_module_name(tc1),
        MR_type_ctor_name(tc1));
    printf(""%s %s\\n"",
        MR_type_ctor_module_name(tc2),
        MR_type_ctor_name(tc2));
").
