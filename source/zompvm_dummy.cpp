///
/// A dummy library to link against when building zomp exes
///

extern "C"
{
    bool zompRequestedPause() { return false; }
    void zompSetRequestPause(bool request) {}
}

