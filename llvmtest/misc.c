#include <stdio.h>

void foo(int x) {}

char* dasfas = "\"foo\"";

int f = 1;
char* blub = "blubblubh";
float someFloat = 0.3;
  
int add(int l, int r) {
    return l + r;
}

int constant() {
    foo(f);
    
    return 10;
}

int assign() {
    int a = 3;
    int b = 4;
    a = b;
    return 0;
}

int shift(int l, int r) {
    return l << r;
}

int ifthenelse(int t) {
    if( t ) {
        return 1;
    }
    else {
        return 2;
    }
}
int loop() {
    int sum = 0;

    while( sum < 100 ) {
        sum += sum;
    }

    return sum;
}

int compare(int x) {
    return 3 < x;
}

void printftest() {
  printf("blah\n");
}

typedef struct {
  int a;
  float b;
} foo_t;

foo_t createfoo() {
  foo_t foo;
  return foo;
}

void arrayAccess(float* ptr) {
/*   float* ptr = NULL; */
  ptr[99] = 0.0f;
/*   float x = ptr[27]; */
}


struct rectype {
  int num;
  struct rectype* childs;
};

int rectypeTest(struct rectype* foo) {
  return foo->num;
}


