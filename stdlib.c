
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

void printString( char* str ) {
    printf( "%s", str );
}

void printInt( int i ) {
    printf( "%d", i );
}

void printFloat( float f ) {
    printf( "%f", f );
}

void printChar( int8_t c ) {
  printf( "%c", c );
}

void printNewline() {
  printf( "\n" );
}

int* nullptr() {
  return NULL;
}

void stdlibHello() {
  printf( "hello, stdlib\n" );
}

char* int2cstring(int i) {
  char buffer[1000];

  sprintf( buffer, "%d", i );

  size_t charCount = strlen( buffer );
  char* result = (char*) malloc( sizeof(char) * (charCount + 1) );
  strcpy( result, buffer );
  return result;
}


