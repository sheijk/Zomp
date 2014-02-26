/**
 * Testing forward declarations
 */

#include <stdio.h>
#include <stdlib.h>

void hello();

int main(int argc, char* argv[]) {
    hello();
    return 0;
}

void hello() {
    printf("hello\n");
}

