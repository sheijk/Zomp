//
// See how LLVM generates code for structs
//

#include <stdlib.h>

typedef struct {
    int x, y, z;
} Point;

Point globalPoint;

int numbers[4] = { 1, 2, 3, 4 };
int undef_numbers[10];
int xxx = 100;

int main() {
    /* Point* points = malloc(sizeof(Point) * 10); */
    Point p = globalPoint;
    numbers[0] = numbers[1] + 2;
    /* points[0] = p; */

    return 0;
}


