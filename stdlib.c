
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <dlfcn.h>


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

  return (int)handle;
}

bool zompCheckNativeSymbol(const char* name) {
  return dlsym( NULL, name ) != NULL ? true : false;
}

/* NSBundle* webKitBundle; */
/*     webKitBundle = [NSBundle bundleWithPath:@"/System/Library/Frameworks/WebKit.framework"]; */
/*     if (webKitBundle) { */
/*         _webkitAvailable = [webKitBundle load]; */
/*     } */

