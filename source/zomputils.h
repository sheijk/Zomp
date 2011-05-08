#ifndef ZOMPUTILS_H_2009_09_27_INCLUDED
#define ZOMPUTILS_H_2009_09_27_INCLUDED

#define ZMP_ASSERT(x,onFail)                                            \
    if( !(x) ) {                                                        \
        printf("[" __FILE__ ":%d]: assertion failed: %s\n", __LINE__, #x); \
        onFail;                                                         \
        fflush(stdout);                                                 \
        fflush(stderr);                                                 \
        abort();                                                        \
    }

inline int ptrToInt(void* ptr) {
    int int32_handle = (int)(intptr_t)ptr;
    ZMP_ASSERT((intptr_t)int32_handle == (intptr_t)ptr,
               printf("32-bit: %p, 64-bit: %p\n",
                      (void*)(intptr_t)int32_handle,
                      ptr);
               );
    return int32_handle;
}

#endif
// end of ZOMPUTILS_H_2009_09_27_INCLUDED
