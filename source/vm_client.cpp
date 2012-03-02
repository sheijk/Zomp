#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <netdb.h>

static void warning(const char* msg)
{
    perror(msg);
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

class Client
{
    enum { INVALID_FD = -1 };
    int socketFD;

public:
    Client()
    {
        socketFD = INVALID_FD;
    }

    ~Client()
    {
        disconnect();
    }

    bool connectToServer(const char* host, int portno)
    {
        socketFD = socket(AF_INET, SOCK_STREAM, 0);
        if (socketFD < 0)
        {
            warning("error: could not open socket");
            return false;
        }

        hostent* server = gethostbyname(host);
        if (server == NULL)
        {
            fprintf(stderr,"error: no such host\n");
            return false;
        }

        sockaddr_in serverAddress;
        memset(&serverAddress, 0, sizeof(serverAddress));
        serverAddress.sin_family = AF_INET;
        memcpy(
            (char *)server->h_addr, 
            (char *)&serverAddress.sin_addr.s_addr,
            server->h_length);

        serverAddress.sin_port = htons(portno);

        if( connect(socketFD, (sockaddr *)&serverAddress, sizeof(serverAddress)) < 0)
        {
            warning("error: could not connect");
            return false;
        }

        return true;
    }

    void disconnect()
    {
        if( socketFD != INVALID_FD )
        {
            close(socketFD);
            socketFD = INVALID_FD;
        }
    }

    bool sendMessage(const char* msg)
    {
        int length = strlen(msg);

        int bytes_written_1 = write( socketFD, &length, 4 );
        int bytes_written = write(socketFD, msg, length);
        if ( bytes_written_1 != 4 || bytes_written < 0 )
        {
            warning("error: could not write to socket");
            return false;
        }
        else
        {
            return true;
        }
    }

    void printReceivedData()
    {
        char buffer[256];
        bzero(buffer,256);
        int bytes_read = read(socketFD,buffer,255);
        if (bytes_read < 0)
        {
            warning("ERROR reading from socket");
        }
        printf("client received: %s\n",buffer);
    }
};

int main(int argc, char *argv[])
{
    printArgs(argc, argv);

    if (argc < 3) {
        fprintf(stderr,"usage %s hostname port\n", argv[0]);
        return 1;
    }

    const char* host =argv[1];
    int portno = atoi(argv[2]);

    Client client;

    if( !client.connectToServer(host, portno) )
    {
        fprintf(stderr, "error: failed to connect to server\n");
        return 1;
    }

    client.sendMessage( "hello server" );
    client.sendMessage( "shutdown" );

    client.printReceivedData();

    client.disconnect();

    return 0;
}

