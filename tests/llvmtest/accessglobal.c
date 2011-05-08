#include <stdio.h>

int result = 10;
char* text = "this be text\n";

char* foobar() {
    return text;
}

int main() {
    printf("foobar");
    return result;
}

