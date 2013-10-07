#ifndef ZOMPVM_CAML_H_2012_03_15_INCLUDED
#define ZOMPVM_CAML_H_2012_03_15_INCLUDED

extern "C"
{

void zompInitCamlCallbacks();
bool zompCamlCallbacksValid();

bool isBound(char* name);
int zompLookup(char* name);
void* zompParse(char* source);

int zompGetCamlCounterValue(int id);

} // extern "C"

#endif
// end of ZOMPVM_CAML_H_2012_03_15_INCLUDED
