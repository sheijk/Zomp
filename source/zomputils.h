#ifndef ZOMPUTILS_H_2009_09_27_INCLUDED
#define ZOMPUTILS_H_2009_09_27_INCLUDED

#include <stdio.h>
#include <stdlib.h>

#ifdef _MSC_VER
#define ZOMP_WINDOWS
#pragma warning(disable: 4996)
#else
#define ZOMP_OSX
#endif

#define ZMP_ASSERT(x,onFail)                                            \
    if( !(x) ) {                                                        \
        printf("%s:%d: error: assertion failed: %s\n", __FILE__, __LINE__, #x); \
        onFail;                                                         \
        fflush(stdout);                                                 \
        fflush(stderr);                                                 \
        abort();                                                        \
    }

typedef __int8_t i8;
typedef __uint8_t u8;
typedef __int16_t i16;
typedef __uint16_t u16;
typedef __int32_t i32;
typedef __uint32_t u32;
typedef __int64_t i64;
typedef __uint64_t u64;

#ifndef __cplusplus
typedef int bool;
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Stuff defined in runtime.c
int ptrToInt(void* ptr);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
// end of ZOMPUTILS_H_2009_09_27_INCLUDED
