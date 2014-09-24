#include <stdint.h>

struct foo { int field; };
typedef struct foo* fooptr;

void foo(struct foo* ptr) {}

int getField(struct foo* ptr) {
    return ptr->field;
} 

int main() {
    struct foo xxx;
    fooptr ptr = &xxx;
    return 0;
}

