#ifndef STATS_H_2013_10_06_INCLUDED
#define STATS_H_2013_10_06_INCLUDED

#include "zomputils.h"

#ifdef __cplusplus
extern "C" {
#endif

/// A counter
struct Counter;

/// Sections form the counter hierarchy. They can contain other sections and
/// counters and can have a name.
struct Section;

/// Function type used to query the value of a counter.
typedef i64 (*CounterQueryFunction)(Counter*, void*);

/// Function type used to query the value of a float counter.
typedef float (*CounterQueryFunctionFloat)(Counter*, void*);

/// The root section. Has no name and does not contain any counters.
Section* statsMainSection();

/// Adds a new child section. Does not check if another one of the same name
/// already exists.
Section* statsCreateSection(Section* parent, const char* name);

/// Add a counter to the given section. The query function will be called when
/// @statsPrintReport gets called. userData will be passed to it.
Counter* statsCreateCounter(Section* parent, const char* name, u32 fractionalBits, void* userData, CounterQueryFunction query);

/// Same as @statsCreateCounter but for floats.
Counter* statsCreateCounterFloat(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunctionFloat query);

/// Creates a counter whose value is read by de-referencing ptr.
Counter* statsCreateCounterForValue(Section* parent, const char* name, u32 fractionalDigits, int* ptr);

/// Will add a section with the given name to the root section. Will not add
/// a second one of one already exists with the given name.
void statsCreateNamedSection(const char* sectionName);

/// Will create a counter whose value is read by passing the given id to the
/// OCaml function denoted by the named value getCounterValue.
void statsCreateCamlCounter(const char* sectionName, const char* name, int fractionalDigits, int id);

/// @see statsCreateCamlCounter
void statsCreateCamlCounterFloat(const char* sectionName, const char* name, int fractionalDigits, int id);

// TODO: add functions to freeze counters/sections (so their query functions)
// won't get called, again.
// void statsDeleteSection(Section*);
// void statsDeleteCounter(Counter*);

/// Pointer to first sub section. NULL if section does not have sub sections.
Section* statsFirstChildSection(Section*);
/// The child following the given section in it's parent or NULL if it is the last.
Section* statsNextSection(Section*);

/// Pointer to first counter. NULL if section does not have any counters;
Counter* statsFirstCounter(Section*);
/// The counter following the given counter in it's section or NULL if it is the last.
Counter* statsNextCounter(Counter*);

/// A zero terminated string. Will never be NULL.
const char* statsCounterName(Counter*);
/// The number of fractional digits used for displaying.
i32 statsCounterFractionalDigits(Counter*);

/// Will print the values of all counters to stdout.
void statsPrintReport(int indent);
void statsPrintReportToStream(FILE* out, int indent);
bool statsPrintReportToFile(const char* fileName, int indent);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
// ond of STATS_H_2013_10_06_INCLUDED

