//
// Copyright (C) 2001-2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class DuExistInfo {
	
	public int exist_typeinfos_plain;
	public int exist_typeinfos_in_tci;
	public int exist_tcis;
	public /* final */ DuExistLocn[] exist_typeinfo_locns;
	public /* final */ TypeClassConstraint[] exist_constraints;

	public DuExistInfo()
	{
	}

	public void init(int typeinfos_plain, int typeinfos_in_tci, int tcis,
		DuExistLocn[] typeinfo_locns,
		TypeClassConstraint constraints[])
	{
		exist_typeinfos_plain = typeinfos_plain;
		exist_typeinfos_in_tci = typeinfos_in_tci;
		exist_tcis = tcis;
		exist_typeinfo_locns = typeinfo_locns;
		exist_constraints = constraints;
	}
}
