#include "vm_protocol.h"

#include <string.h>

#define WRITE_FIELD(type, name)     \
    bytes = sizeof(msg->name);      \
    memcpy(out, &msg->name, bytes); \
    out += bytes;                   \
    total_bytes += bytes;

#define VMMSG_DEFINE_WRITER(name)               \
    VMMSG_SIGNATURE_WRITER(name)                \
    {                                           \
        int bytes = 0;                          \
        char* out = target;                     \
        int total_bytes = 0;                    \
        VMMSG_FOR_##name(WRITE_FIELD)           \
        return total_bytes;                     \
    }

#define READ_FIELD(type, name)                  \
    bytes = sizeof(msg->name);                  \
    memcpy(&msg->name, in, bytes);              \
    in += bytes;                                \
    total_bytes += bytes;

#define VMMSG_DEFINE_READER(name)               \
    VMMSG_SIGNATURE_READER(name)                \
    {                                           \
        int bytes = 0;                          \
        int total_bytes = 0;                    \
        const char* in = source;                \
        bool error = false;                     \
        VMMSG_FOR_##name(READ_FIELD);           \
        msg->valid = !error;                    \
        return total_bytes;                     \
    }

#define VMMSG_DEFINE_MESSAGE_IMPL(name, _) \
    VMMSG_DEFINE_WRITER(name)              \
    VMMSG_DEFINE_READER(name)

VMMSG_FOREACH_MESSAGE(VMMSG_DEFINE_MESSAGE_IMPL)

