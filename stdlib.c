
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

char* int2cstring(int i) {
  char buffer[1000];

  sprintf( buffer, "%d", i );

  size_t charCount = strlen( buffer );
  char* result = (char*) malloc( sizeof(char) * (charCount + 1) );
  strcpy( result, buffer );
  return result;
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

