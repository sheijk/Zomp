

#include <stdio.h>
#include <dlfcn.h>

int main() {
  void* handle = dlopen( "dlltest.dylib", RTLD_LAZY );
  if( handle == NULL ) {
    printf( "error: %s\n", dlerror() );
  }
  else {
    printf( "success\n" );
  }

  return 0;
}

