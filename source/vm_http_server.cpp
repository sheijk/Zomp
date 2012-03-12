#include "zomputils.h"

#include <stdio.h>
#include <string.h>
#include <map>
#include <string>

#include "mongoose.h"

#pragma comment(lib, "ws2_32.lib")

static const char* htmlHeader =
    "HTTP/1.1 200 OK\r\n"
    "Content-Type: text/html\r\n\r\n";

class HttpVMServer
{
public:

    void run()
    {
        struct mg_context *ctx;
        const char *options[] = {"listening_ports", "8080", NULL};

        ctx = mg_start(&callback, this, options);
        getchar();  // Wait until user hits "enter"
        mg_stop(ctx);
    }

private:

    void pageIndex(mg_connection* conn, const mg_request_info* requestInfo)
    {
        mg_printf(conn, htmlHeader);

        mg_printf(conn,
            "<html>\n"
            "  <head><title>ZompVM server</title></head>\n"
            "  <body>\n"
            "    <h1>Status</h1>\n"
            "    <p>running</p>\n"
            "  </body>\n"
            "</html>\n");
    }

    void pageUnknown(mg_connection* conn, const mg_request_info* requestInfo)
    {
        mg_printf(conn, htmlHeader);

        mg_printf(conn,
            "<html>\n"
            "  <head>\n"
            "    <title>ZompVM - error</title>\n"
            "  </head>\n"
            "  <body>\n"
            "    <p>404</p>\n"
            "  </body>\n"
            "</html>\n");
    }

    void* onEvent(mg_event eventType, mg_connection *conn, const mg_request_info *requestInfo)
    {
        if(eventType == MG_NEW_REQUEST)
        {
            std::string uri(requestInfo->uri + 1);

            if(uri == "index.html")
            {
                pageIndex(conn, requestInfo);
            }
            else
            {
                pageUnknown(conn, requestInfo);
            }

            return "";  // Mark as processed
        }
        else
        {
            return NULL;
        }
    }

    static void* callback(mg_event eventType, mg_connection *conn, const mg_request_info *requestInfo)
    {
        HttpVMServer* server = (HttpVMServer*)requestInfo->user_data;
        if( server )
        {
            return server->onEvent( eventType, conn, requestInfo );
        }
        else
        {
            return NULL;
        }
    }
};

int main(int argc, char *argv[])
{
    HttpVMServer server;
    server.run();

    return 0;
}

