
#include <stdint.h>
#include <assimp.h>

void forceInclusionOfAssimpSymbols() {
    aiImportFile("", 0);
}

