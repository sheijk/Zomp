
#include "zomputils.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#if !defined(ZOMP_WINDOWS)
#include <utime.h>
#endif
#include <sys/stat.h>

#if !defined(ZOMP_WINDOWS)
#include <dlfcn.h>
#endif

#include "zomputils.h"

//------------------------------------------------------------------------------
//- printing to stdout

void printInt(int i) {
  printf( "%d", i );
}

void printString(char* str) {
  printf("%s", str);
}

void printFloat(float f) {
  printf("%f", f);
}

void printDouble(double d) {
  printf("%f", d);
}

void printChar(int8_t c) {
  printf("%c", c);
}

void printPtr(void* ptr) {
  printf("%p", ptr);
}

void printNewline() {
  printf("\n");
  fflush(stdout);
}

void flushStdout() {
  fflush(stdout);
}

//------------------------------------------------------------------------------
//- reading from files

float parseFloat(const char* str) {
    float f;
    sscanf( str, "%f", &f );
    return f;
}

int parseInt(const char* str) {
    int i;
    sscanf( str, "%i", &i );
    return i;
}

char* parseFloats(char* str, int count, float* dest) {
    char* pos = str;
    int i;
    for( i = 0; i < count; ++i ) {
        *dest = parseFloat(pos);
        ++dest;

        while( *pos != ' ' && *pos != '\0' )
            ++pos;
        while( *pos == ' ' )
            ++pos;
    }

    return pos;
}

//------------------------------------------------------------------------------
//- writing to files

int zomp_closeFile(void* file) {
    return fclose((FILE*)file);
}

int zomp_writeInt(void* file, int i) {
    return fprintf((FILE*)file, "%d", i);
}

int zomp_writeCString(void* file, char* val) {
    return fprintf((FILE*)file, "%s", val);
}

int zomp_writeFloat(void* file, float val) {
    return fprintf((FILE*)file, "%f", val);
}

int zomp_writeDouble(void* file, double val) {
    return fprintf((FILE*)file, "%f", val);
}

int zomp_writeChar(void* file, char val) {
    return fprintf((FILE*)file, "%c", val);
}

int zomp_writePtr(void* file, void* val) {
    return fprintf((FILE*)file, "%p", val);
}

//------------------------------------------------------------------------------
//- loading from files

int zomp_readInt(void* file, int* val) {
    return fscanf((FILE*)file, "%d", val);
}

int zomp_readFloat(void* file, float* val) {
    return fscanf((FILE*)file, "%f", val);
}

int zomp_readDouble(void* file, double* val) {
    float tmp = 0.0;
    int result = fscanf((FILE*)file, "%f", &tmp);
    *val = tmp;
    return result;
}

int zomp_readChar(void* file, char* val) {
    return fscanf((FILE*)file, "%c", val);
}

//------------------------------------------------------------------------------
//- conversions

char* copyString(char* str) {
    size_t charCount = strlen( str );
    char* result = (char*) malloc( sizeof(char) * (charCount + 1) );
    strcpy( result, str );
    return result;
}

static char buffer[1000];

char* int2cstring(int i) {
  sprintf( buffer, "%d", i );
  return copyString(buffer);
}

char* float2cstring(float f) {
    sprintf( buffer, "%f", f );
    return copyString(buffer);
}

char* double2cstring(double d) {
    sprintf( buffer, "%f", d );
    return copyString(buffer);
}

char* char2cstring(char c) {
    char* str = (char*)malloc(sizeof(char) * 2);
    str[0] = c;
    str[1] = '\0';
    return str;
}

void stdlibHello() {
  printf( "hello, stdlib\n" );
}

#if defined(ZOMP_WINDOWS)

typedef int bool;

int zompLoadLib(const char* name) {
  // TODO
  return 0;
}

bool zompCheckNativeSymbol(const char* name) {
  // TODO
  return 0;
}

#else

int zompLoadLib(const char* name) {
  void* handle = dlopen( name, RTLD_LAZY );

  if( handle == NULL ) {
    printf( "Could not load dll '%s': %s\n", name, dlerror() );
    fflush( stdout );
  }

  return ptrToInt(handle);
}

bool zompCheckNativeSymbol(const char* name) {
  return dlsym( NULL, name ) != NULL;
}

#endif

// ripped from http://www.anyexample.com/programming/c/how_to_load_file_into_memory_using_plain_ansi_c_language.xml
int zompLoadFileToMemory(const char *filename, char **result)
{
    size_t size = 0;
    FILE *f = fopen(filename, "rb");
    if (f == NULL) 
    {
        *result = NULL;
        return -1; // -1 means file opening fail
    }
    fseek(f, 0, SEEK_END);
    size = ftell(f);
    fseek(f, 0, SEEK_SET);
    *result = (char *)malloc(size+1);
    if (size != fread(*result, sizeof(char), size, f))
    {
        free(*result);
        return -2; // -2 means file reading fail
    }
    fclose(f);
    (*result)[size] = 0;
    return size;
}

int zompFileModificationTimestamp(const char* filename)
{
    struct stat s;
    stat(filename, &s);
    return (int)s.st_mtime;
}

/* NSBundle* webKitBundle; */
/*     webKitBundle = [NSBundle bundleWithPath:@"/System/Library/Frameworks/WebKit.framework"]; */
/*     if (webKitBundle) { */
/*         _webkitAvailable = [webKitBundle load]; */
/*     } */

//------------------------------------------------------------------------------
//- from zomputils.h

int ptrToInt(void* ptr)
{
    int int32_handle = (int)(intptr_t)ptr;
    ZMP_ASSERT((intptr_t)int32_handle == (intptr_t)ptr,
        printf("32-bit: %p, 64-bit: %p\n",
            (void*)(intptr_t)int32_handle,
            ptr);
        );
    return int32_handle;
}


