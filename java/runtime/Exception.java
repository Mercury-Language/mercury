//
// Copyright (C) 2009 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class Exception extends java.lang.Error {
    // Should be mercury.univ.Univ_0 but we don't want to depend on the
    // standard library.
    public Object exception;

    public Exception(Object exception) {
        this.exception = exception;
    }
}
