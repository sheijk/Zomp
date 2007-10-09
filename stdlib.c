
#include <stdio.h>
#include <stdint.h>

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

