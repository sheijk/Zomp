/**
 * Testing how escaped strings are handled by llvm-gcc
 */

#include <stdio.h>
#include <stdlib.h>

char* msg = "line0\nline1\n";

int main(int argc, char* argv[]) {
    printf("%s", msg);
    return 0;
}

