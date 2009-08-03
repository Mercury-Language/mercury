//
// Copyright (C) 2009 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class Exception extends java.lang.Error {
    // This is to be set when the exception module is initialised, to avoid
    // having the runtime depend on the standard library.
    public static MethodPtr getMessageHook = null;

    // Should be univ.Univ_0 but we don't want to depend on the standard
    // library.
    public Object exception;

    public Exception(Object exception) {
        this.exception = exception;
    }

    public String getMessage() {
        if (getMessageHook != null) {
            Object[] args = new Object[] { exception };
            return (String) getMessageHook.call___0_0(args);
        } else {
            return null;
        }
    }
}
