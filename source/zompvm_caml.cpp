///
/// Callbacks into OCaml for ZompVM
///

#include "zomputils.h"

extern "C"
{
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
}
#undef flush

/**
 * These are callbacks to the parts of the zomp compiler/runtime implemented
 * in OCaml. You can find the OCaml counter parts in zompvm.ml.
 */

static value* isBoundCB = NULL;
static value* lookupCB = NULL;
static value* parseCB = NULL;
static value* getCounterValueInt = NULL;
static value* getCounterValueFloat = NULL;

extern "C" {
    void zompInitCamlCallbacks() {
        isBoundCB = caml_named_value("isBound");
        lookupCB = caml_named_value("lookup");
        parseCB = caml_named_value("parse");
        getCounterValueInt = caml_named_value("zompCommonGetCounterValueInt");
        getCounterValueFloat = caml_named_value("zompCommonGetCounterValueFloat");
    }

    void zompShutdownCamlCallbacks() {
        isBoundCB = NULL;
        lookupCB = NULL;
        parseCB = NULL;
        getCounterValueInt = NULL;
        getCounterValueFloat = NULL;
    }

    bool zompCamlCallbacksValid() {
        return
            isBoundCB != NULL &&
            lookupCB != NULL;
        // parseCB != NULL;
    }

    bool isBound(char* name) {
        ZMP_ASSERT(isBoundCB != NULL,);

        value result = caml_callback(*isBoundCB, caml_copy_string(name));
        return Bool_val(result);
    }

    enum { // also defined in zompvm.ml
        ZOMP_SYMBOL_UNDEFINED = 0,
        ZOMP_SYMBOL_VAR = 1,
        ZOMP_SYMBOL_FUNC = 2,
        ZOMP_SYMBOL_MACRO = 3,
        ZOMP_SYMBOL_TYPEDEF = 4,
        ZOMP_SYMBOL_LABEL = 5
    };

    int zompLookup(char* name) {
        ZMP_ASSERT(lookupCB != NULL,);
        value result = caml_callback(*lookupCB, caml_copy_string(name));
        return Int_val(result);
    }

    void* zompParse(char* source) {
        ZMP_ASSERT(parseCB != NULL,);
        value result = caml_callback(*parseCB, caml_copy_string(source));
        return (void*)(result);
    }

    int zompGetCamlCounterValueInt(int id)
    {
        ZMP_ASSERT(getCounterValueInt != NULL,);
        value result = caml_callback(*getCounterValueInt, Val_int(id));
        return Int_val(result);
    }

    float zompGetCamlCounterValueFloat(int id)
    {
        ZMP_ASSERT(getCounterValueFloat != NULL,);
        value result = caml_callback(*getCounterValueFloat, Val_int(id));
        return (float)Double_val(result);
    }

} // extern "C"


