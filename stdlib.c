
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <dlfcn.h>

#include "zomputils.h"

void printInt( int i ) {
  printf( "%d", i );
}

void printString( char* str ) {
  printf( "%s", str );
}

void printFloat( float f ) {
  printf( "%f", f );
}

void printDouble( double d ) {
  printf( "%f", d );
}

void printChar( int8_t c ) {
  printf( "%c", c );
}

void printPtr( void* ptr ) {
  printf( "%p", ptr );
}

void printNewline() {
  printf( "\n" );
}

void flushStdout() {
  fflush(stdout);
}

/* int* nullptr() { */
/*   return NULL; */
/* } */

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

int zompLoadLib(const char* name) {
  void* handle = dlopen( name, RTLD_LAZY );

  if( handle == NULL ) {
    printf( "Could not load dll '%s': %s\n", name, dlerror() );
    fflush( stdout );
  }

  return ptrToInt(handle);
}

/* typedef int bool; */

bool zompCheckNativeSymbol(const char* name) {
  return dlsym( NULL, name ) != NULL;
}

// ripped from http://www.anyexample.com/programming/c/how_to_load_file_into_memory_using_plain_ansi_c_language.xml
int zompLoadFileToMemory(const char *filename, char **result)
{
    int size = 0;
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

/* NSBundle* webKitBundle; */
/*     webKitBundle = [NSBundle bundleWithPath:@"/System/Library/Frameworks/WebKit.framework"]; */
/*     if (webKitBundle) { */
/*         _webkitAvailable = [webKitBundle load]; */
/*     } */

