/**
 * Testing which loops are optimized away by llvm(-gcc)
 */

#include <stdio.h>
#include <stdlib.h>

void foo() {
    float f;
    for( f = 0.0; f < 101.0f; f += 1.0f ) {
    }
}

int main(int argc, char* argv[]) {
    foo();

    return 0;
}

