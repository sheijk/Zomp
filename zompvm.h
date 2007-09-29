#ifndef ZOMPVM_H_20070929_INCLUDED
#define ZOMPVM_H_20070929_INCLUDED

#ifndef __cplusplus
typedef int bool;
#endif

bool zompInit();
void zompShutdown();
bool zompSendCode(const char* code);
bool zompLoadFile(const char* filename);
void zompRunFunction(const char* name);
void zompPrintModuleCode();

#endif

