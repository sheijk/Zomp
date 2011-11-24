
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

enum BarEnum;
struct FooStruct;

/* void f_int_with_default(int i = 10); */

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

int v_int = 10;
struct FooStruct v_foostruct = { 1, 2, 3.0 };
enum BarEnum v_barenum = BE_d;

void multi_forward_decl();
void multi_forward_decl();

void f_cstring( const char* msg );

void f_variadic( const char* format, ... );

typedef struct {
    int mem_a;
    char* mem_b;
} UglyStruct;

typedef enum { ANON_A, ANON_B, ANON_C } AnonEnum;

typedef int (*int_of_float_int_t)(float, int);

struct TenInts
{
    int nums[10];
};

struct VTable
{
    int_of_float_int_t method_iof;
    float (*parse_char)(char*);
};

/* A little test for clang-lookup */
int foo(int abc)
{
    printf( "foobar baz\n" );

    {
        int abc = 0;
        f_void_int( abc );
    }

    return f_int_int( abc );
}

