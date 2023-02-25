// This source file is hereby placed in the public domain.  -fjh (the author).

#include <iostream>

#include "cpp_main.h"

// Use some C++ features
class Foo { };

void cpp_main(void) {
    Foo *p = new Foo;
    std::cout << "In cpp_main()." << std::endl;
    delete p;
}
