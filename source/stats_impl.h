#ifndef STATS_H_2013_10_06_INCLUDED
#define STATS_H_2013_10_06_INCLUDED

#include "zomputils.h"

#ifdef __cplusplus
extern "C" {
#endif

struct Counter;
struct Section;

typedef i64 (*CounterQueryFunction)(Counter*, void*);
    
Section* statsMainSection();
Section* statsCreateSection(Section* parent, const char* name);
// void statsDeleteSection(Section*);
Counter* statsCreateCounter(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunction query);
Counter* statsCreateCounterForValue(Section* parent, const char* name, u32 fractionalDigits, int* ptr);
void statsCreateNamedSection(const char* sectionName);
void statsCreateCamlCounter(const char* sectionName, const char* name, int id, int fractionalDigits);
// void statsDeleteCounter(Counter*);

Section* statsFirstChildSection(Section*);
Section* statsNextSection(Section*);
Counter* statsFirstCounter(Section*);
Counter* statsNextCounter(Counter*);

const char* statsCounterName(Counter*);
i64 statsCounterQuery(Counter*);
i32 statsCounterFractionalDigits(Counter*);

void statsPrintReport(int indent);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
// ond of STATS_H_2013_10_06_INCLUDED

