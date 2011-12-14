
/**
 * Testing LLVM debug information
 */

#include <stdio.h>

int numA(int x) { return x + 1; }
int numB(int x) { return x * 3; }

int main(int argc, char* argv[])
{
    int x = numA( 10 );
    int y = numB( 20 );
    return x + y;
}



