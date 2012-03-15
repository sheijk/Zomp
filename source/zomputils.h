#ifndef ZOMPUTILS_H_2009_09_27_INCLUDED
#define ZOMPUTILS_H_2009_09_27_INCLUDED

#include <stdio.h>

#ifdef _MSC_VER
#define ZOMP_WINDOWS
#else
#define ZOMP_OSX
#endif

#define ZMP_ASSERT(x,onFail)                                            \
    if( !(x) ) {                                                        \
        printf("[" __FILE__ ":%d]: assertion failed: %s\n", __LINE__, #x); \
        onFail;                                                         \
        fflush(stdout);                                                 \
        fflush(stderr);                                                 \
        abort();                                                        \
    }

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
