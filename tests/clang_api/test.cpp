
/**
 * foo
 */

#include <iostream>

extern "C" {
#include "clang-c/Index.h"
}

#include <stdio.h>
#include <stdlib.h>

using std::cout;
using std::endl;

void hfoo(int x = 0) {}

int main(int argc, char const *argv)
{
    return 0;
}