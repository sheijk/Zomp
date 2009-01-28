/**
 * floating points in .ll files
 */

#include <stdio.h>
#include <stdlib.h>

void printFloat(float f) {
    printf("%f", f);
}

int main() {
    float f = 1.1;
    printFloat(f);
    return 0;
}

