#ifndef VM_PROTOCOL_H_2012_03_02_INCLUDED
#define VM_PROTOCOL_H_2012_03_02_INCLUDED

#include <stdint.h>

typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

#define VMMSG_FOREACH_MESSAGE(msg)    \
    msg(Confirm, 1)                   \
    msg(Init, 2)                      \
    msg(Shutdown, 3)                  \
    msg(Print, 4)

#define VMMSG_FOR_Init(f) \
    f(uint32, version)

#define VMMSG_FOR_Shutdown(f)

#define VMMSG_FOR_Confirm(f)

#define VMMSG_FOR_Print(f)

/// Use MessageInfo<MessageT>::id() to get from message type to it's enum
template<typename Message>
struct MessageInfo
{};

// -----------------------------------------------------------------------------

#define VMMSG_DEFINE_ENUM(name, value) MessageId##name = value,

#define VMMSG_DEFINE_STRUCT_FIELD(type, name) int version;

#define VMMSG_DEFINE_MSG_STRUCT(msgName)                 \
    struct Message ## msgName                            \
    {                                                    \
        bool valid;                                      \
        VMMSG_FOR_ ## msgName(VMMSG_DEFINE_STRUCT_FIELD) \
    };

#define VMMSG_SIGNATURE_WRITER(name) int write( char* target, Message##name* msg )
#define VMMSG_SIGNATURE_READER(name) int read( Message##name* msg, const char* source )

#define VMMSG_DECLARE_WRITER(name) VMMSG_SIGNATURE_WRITER(name);
#define VMMSG_DECLARE_READER(name) VMMSG_SIGNATURE_READER(name);

#define VMMSG_DEFINE_MESSAGE(name, _)           \
    VMMSG_DEFINE_MSG_STRUCT(name)               \
    VMMSG_DECLARE_WRITER(name)                  \
    VMMSG_DECLARE_READER(name)

VMMSG_FOREACH_MESSAGE(VMMSG_DEFINE_MESSAGE)

enum MessageId
{
    VMMSG_FOREACH_MESSAGE(VMMSG_DEFINE_ENUM)
};

#define VMMSG_DEFINE_MESSAGE_T_TO_ID(name, _)              \
    template<>                                             \
    struct MessageInfo<Message##name>                      \
    {                                                      \
        static MessageId id() { return MessageId##name; }  \
    };

VMMSG_FOREACH_MESSAGE(VMMSG_DEFINE_MESSAGE_T_TO_ID)

#endif
// end of VM_PROTOCOL_H_2012_03_02_INCLUDED
