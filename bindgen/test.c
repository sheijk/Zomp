
#include <stdio.h>

void f_no_arg(void);
void f_no_arg2(void);

void f_void_int(int i);
int f_int_int(int i);
void f_void_float(float f);
void f_void_int_int(int a, int b);

void f_void_int_anom(int);

// void f_int_with_default(int i = 10);

int v_int = 10;

struct FooStruct
{
    int a, baz;
    float b;
};

void f_void_foostruct(struct FooStruct s);

typedef unsigned int uint;

void f_void_uint(uint ui);

enum BarEnum { BE_a, BE_b, BE_c = 30, BE_d };

void v_void_BarEnum(enum BarEnum e);

int main(void)
{
    struct FooStruct f;
    f.baz = f_int_int(0);
}

