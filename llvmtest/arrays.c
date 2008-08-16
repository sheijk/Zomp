//
// See how LLVM generates code for structs
//

#include <stdlib.h>

typedef struct {
    int x, y, z;
} Point;

Point globalPoint;

int main() {
    /* Point* points = malloc(sizeof(Point) * 10); */
    Point p = globalPoint;
    /* points[0] = p; */

    return 0;
}


