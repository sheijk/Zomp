#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <netinet/in.h>

#include <vector>
#include <algorithm>

#include "vm_protocol.h"

enum
{
    ERROR_CODE_NONE = 0,
    ERROR_CODE_GENERIC = 1,
    ERROR_CODE_ADDRESS_IN_USE = 2,
};

enum { logNetwork = 0 };

namespace vmserver
{
class Server
{
    enum { INVALID_FD = -1 };
    int portNumber_;
    int listenFD_;
    int clientFD_;
    bool shutdown_;

    enum { BUFFER_LEN = 1024 };
    char buffer[BUFFER_LEN];
    size_t buffer_first_valid_byte;
    size_t buffer_valid_size;

public:
    Server()
    {
        portNumber_ = listenFD_ = clientFD_ = INVALID_FD;
        shutdown_ = false;
        buffer_first_valid_byte = buffer_valid_size = 0;
    }

    ~Server()
    {
        disconnectClient();
        disconnectServer();
    }

    int run(int portNumber)
    {
        waitForClientConnection(portNumber);

        while( !shutdown_ )
        {
            handleClientRequest();
        }

        return ERROR_CODE_NONE;
    }

private:
    int waitForClientConnection(int portNumber)
    {
        portNumber_ = portNumber;

        int result = 0;
        sockaddr_in serverAddress;
        listenFD_ = socket(AF_INET, SOCK_STREAM, 0);
        if (listenFD_ < 0)
        {
            warning("ERROR opening socket");
            return ERROR_CODE_GENERIC;
        }
        memset(&serverAddress, 0, sizeof(serverAddress));
        serverAddress.sin_family = AF_INET;
        serverAddress.sin_addr.s_addr = INADDR_ANY;
        serverAddress.sin_port = htons(portNumber);
        result = bind(listenFD_, (sockaddr*) &serverAddress, sizeof(serverAddress));

        if( result < 0 )
        {
            if( errno == EADDRINUSE )
            {
                printf("error: address already in use\n");
                return ERROR_CODE_ADDRESS_IN_USE;
            }
            else
            {
                warning("ERROR on binding");
                return ERROR_CODE_GENERIC;
            }
        }
        listen(listenFD_,5);

        sockaddr_in clientAddress;
        socklen_t clientLength = sizeof(clientAddress);
        clientFD_ = accept(listenFD_, (sockaddr*) &clientAddress, &clientLength);
        if (clientFD_ < 0)
        {
            warning("error: failed to connect to client");
            return ERROR_CODE_GENERIC;
        }

        return 0;
    }

    void normalizeBuffer()
    {
        if( buffer_first_valid_byte >= BUFFER_LEN )
        {
            buffer_first_valid_byte = 0;
            buffer_valid_size = 0;
        }

        if( buffer_valid_size > 0 && buffer_valid_size < 128 )
        {
            memcpy(buffer, buffer + buffer_first_valid_byte, buffer_valid_size);
            buffer_first_valid_byte = 0;
        }
    }

    void printBytes( int count, char* data )
    {
        for( int i = 0; i < count; ++i )
        {
            printf("%d ", data[i]);
        }
        printf("\n");
        fflush(stdout);
    }

    bool receiveData()
    {
        normalizeBuffer();

        int read_bytes = 0;
        do
        {
            int write_index = buffer_first_valid_byte + buffer_valid_size;
            read_bytes = read(
                clientFD_,
                &buffer[write_index],
                BUFFER_LEN - write_index );
        }
        while( read_bytes == 0 );

        if (read_bytes > 0)
        {
            if( logNetwork )
            {
                printf("received ");
                printBytes(
                    read_bytes,
                    buffer + buffer_first_valid_byte + buffer_valid_size );
            }

            buffer_valid_size += read_bytes;
            return true;
        }
        else
        {
            warning("failed to read from socket");
            shutdown_ = true;
            return false;
        }
    }

    bool consumeBytes( char* target, size_t bytes )
    {
        if( bytes > buffer_valid_size )
        {
            return false;
        }

        if ( target )
        {
            memcpy(target, &buffer[buffer_first_valid_byte], bytes);
        }

        buffer_first_valid_byte += bytes;
        buffer_valid_size -= bytes;
        return true;
    }

    void
    readBytes( char* target, size_t size )
    {
        size_t remaining_bytes = size;

        while( remaining_bytes > 0 )
        {
            if( buffer_valid_size < remaining_bytes )
            {
                receiveData();
            }

            size_t bytes_to_read = std::min(buffer_valid_size, remaining_bytes);
            consumeBytes( target, bytes_to_read );
            target += bytes_to_read;
            remaining_bytes -= bytes_to_read;
        }
    }

    template<typename T>
    T readValue()
    {
        if( buffer_valid_size < sizeof(T) )
        {
            receiveData();
        }

        T value = T();
        if( !consumeBytes((char*)&value, sizeof(T)) )
        {
            warning("failed to read bytes");
        }
        return value;
    }

    void handle( MessageConfirm& msg )
    {
        printf( "server: Received confirmation from client\n" );
    }

    void handle( MessageInit& msg )
    {
        printf( "server: client registered, version 0x%x\n", msg.version );
    }

    void handle( MessageShutdown& msg )
    {
        printf( "server: Received shutdown request\n" );
        shutdown_ = true;
    }

    void handle( MessagePrint& msg )
    {
        printf( "server: Received print message\n" );
    }

    void handleUnknown( uint32 id )
    {
        printf( "server: Received unknown message %d\n", id );
        shutdown_ = true;
    }

#define VMMSG_DISPATCH_ON_ID(name, value)        \
    case value:                                  \
    {                                            \
        Message##name msg;                       \
        int bytes_read = read( &msg, buffer + buffer_first_valid_byte ); \
        readBytes( 0, bytes_read );              \
        handle( msg );                           \
        break;                                   \
    }

#define VMMSG_DISPATCH_MESSAGE(msgId) \
    switch(msgId)                               \
    {                                           \
        VMMSG_FOREACH_MESSAGE(VMMSG_DISPATCH_ON_ID) \
    default:                                    \
        handleUnknown( msgId );                 \
        break;                                  \
    }

    void handleClientRequest()
    {
        char msgid;
        readBytes( &msgid, 1 );

        if( msgid != MessageIdPrint )
        {
            VMMSG_DISPATCH_MESSAGE(msgid);
            return;
        }

        char header[3];
        readBytes(header, 3);
        size_t length = ((size_t)header[0] << 16) | ((size_t)header[1] << 8) | header[2];
        if( length == 0 )
        {
            return;
        }

        std::vector<char> message;
        message.resize( length + 1 );
        readBytes( &message[0], length );
        message.back() = '\0';

        printf( "server: print '%s'\n", &message[0] );
        fflush(stdout);

        sendToClient("I got your message\n");
    }

    void sendToClient(const char* msg)
    {
        int written_bytes = write(clientFD_,msg,strlen(msg));
        if (written_bytes < 0)
        {
            warning("ERROR writing to socket");
            shutdown_ = true;
        }
    }

    void disconnectClient()
    {
        if( clientFD_ == 0 )
            return;

        int result = close(clientFD_);
        clientFD_ = 0;

        if ( result != 0 ) {
            warning("closing connection socket failed");
        }
    }

    void disconnectServer()
    {
        if( listenFD_ == 0 )
        {
            return;
        }

        int result = close(listenFD_);
        listenFD_ = 0;
        if ( result != 0 )
        {
            warning("closing server socket failed");
        }
    }

    void warning(const char* msg)
    {
        printf("warning: %s\n", msg);
    }
};
}

static void printArgs(int argc, char *argv[])
{
    printf("called ");
    for( int i = 0; i < argc; ++i )
    {
        printf("%s ", argv[i]);
    }
    printf("\n");
}

int main(int argc, char *argv[])
{
    printArgs(argc, argv);

    if(argc < 2) {
        printf("error: please call with port number\n");
        exit( ERROR_CODE_GENERIC );
    }

    vmserver::Server server;
    return server.run( atoi(argv[1]) );
}

