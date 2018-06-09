//
// Copyright (C) 2001-2003 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// Corresponds to MR_DuExistLocn in runtime/mercury_type_info.h

public class DuExistLocn implements java.io.Serializable {
	public int exist_arg_num;
	public int exist_offset_in_tci;

	public DuExistLocn(int arg_num, int offset_in_tci) {
		exist_arg_num = arg_num;
		exist_offset_in_tci = offset_in_tci;
	}
}
