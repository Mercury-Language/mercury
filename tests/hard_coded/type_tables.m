% Test case for runtime/mercury_type_tables.h
% 
% Author: zs

:- module type_tables.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- type list	--->	a ; b.

:- pragma(c_code, main(IO0::di, IO::uo), "
extern const struct MR_TypeCtorInfo_Struct mercury_data_list__type_ctor_info_list_1;
extern const struct MR_TypeCtorInfo_Struct mercury_data_type_tables__type_ctor_info_list_0;
	MR_TypeCtorInfo	tc1;
	MR_TypeCtorInfo	tc2;

	MR_register_type_ctor_info(&mercury_data_list__type_ctor_info_list_1);
	MR_register_type_ctor_info(&mercury_data_type_tables__type_ctor_info_list_0);

	tc1 = MR_lookup_type_ctor_info(""list"", ""list"", 1);
	tc2 = MR_lookup_type_ctor_info(""type_tables"", ""list"", 0);

	printf(""%s %s\\n"", tc1->type_ctor_module_name, tc1->type_ctor_name);
	printf(""%s %s\\n"", tc2->type_ctor_module_name, tc2->type_ctor_name);
	IO = IO0;
").
