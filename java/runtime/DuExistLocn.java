//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

// Corresponds to MR_DuExistLocn in runtime/mercury_type_info.h

public class DuExistLocn {
	public int exist_arg_num;
	public int exist_offset_in_tci;
	public DuExistLocn(int arg_num, int offset_in_tci) {
		exist_arg_num = arg_num;
		exist_offset_in_tci = offset_in_tci;
	}
}
