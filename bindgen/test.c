
#include <stdio.h>

void f_no_arg(void);
void f_no_arg2(void);

void f_void_int(int i);
int f_int_int(int i);
void f_void_float(float f);
void f_void_int_int(int a, int b);

void* f_voidptr(void);
int* f_intptr(void);
void f_void_intptr(int* pi);
void f_void_floatptrptr(float** ppf);

void f_void_int_anom(int);

// void f_int_with_default(int i = 10);

int v_int = 10;

struct FooStruct
{
    int a, baz;
    float b;
};

void f_void_foostruct(struct FooStruct s);
void f_void_foostructptr(struct FooStruct* ps);

typedef unsigned int typedefed_uint;

void f_void_uint(typedefed_uint ui);
void f_void_uintptr(typedefed_uint* pui);

enum BarEnum { BE_a, BE_b, BE_c = 30, BE_d };

void v_void_barenum(enum BarEnum e);
void v_void_barenumptr(enum BarEnum* pe);

int main(void)
{
    struct FooStruct f;
    f.baz = f_int_int(0);
}

