//
// Copyright (C) 2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

// This corresponds to the C typedef "MR_TypeClassDeclStruct"
// in runtime/mercury_types.h, i.e. the C struct
// "struct MR_TypeClassDecl_Struct" in runtime/mercury_typeclass_info.h.

public class TypeClassDeclStruct {
	public TypeClassId		tc_decl_id;
	public int			tc_decl_version_number;
	public int			tc_decl_num_supers; // redundant
	public TypeClassConstraint	tc_decl_supers;

	public TypeClassDeclStruct(TypeClassId id, int version_number,
		int num_supers, TypeClassConstraint supers)
	{
		tc_decl_id = id;
		tc_decl_version_number = version_number;
		tc_decl_num_supers = num_supers;
		tc_decl_supers = supers;
	}
}
