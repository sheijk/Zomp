#include <stdio.h>

extern "C" {
#include "zompvm.h"

void zompInit() {
  printf( "Initializing ZompVM\n" );
}

void zompShutdown() {
  printf( "Shutting down ZompVM\n" );
}
}


