///
/// A dummy implementation of zompvm_caml
///

extern "C"
{
    void zompInitCamlCallbacks() {}
    void zompShutdownCamlCallbacks() {}
    bool zompCamlCallbacksValid() { return true; }

    bool isBound(char*) { return false; }
    int zompLookup(char*) { return 0; }
} // extern "C"

