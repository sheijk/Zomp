#include "zomputils.h"
#include "zompvm_impl.h"

#include <stdio.h>
#include <string.h>
#include <map>
#include <string>

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"

#include "mongoose.h"

#pragma comment(lib, "ws2_32.lib")

static const char* htmlHeader =
    "HTTP/1.1 200 OK\r\n"
    "Content-Type: text/html\r\n\r\n";

class HttpVMServer
{
    enum State {
        StateNotInitialized,
        StateRunning,
        StateFailedToInitialize
    };

    State state_;
    std::string stateDescr_;

public:

    HttpVMServer() {
        state_ = StateNotInitialized;
    }

    void run()
    {
        if( !zompInit() ) {
            state_ = StateFailedToInitialize;
            stateDescr_ = "Failed to initialize Zomp VM";
            return;
        }
        state_ = StateRunning;

        const char *options[] = {"listening_ports", "8080", NULL};
        mg_context* ctx = mg_start(&callback, this, options);
        getchar(); // Wait until user hits "enter"
        mg_stop(ctx);

        zompShutdown();
        state_ = StateNotInitialized;
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
            "    <h2>LLVM</h2>\n");

        llvm::LLVMContext* context = zompLLVMContext();
        llvm::Module* module = zompLLVMModule();
        std::string target = module->getTargetTriple();

        mg_printf(conn, "  <p>Module identifier %s</p>\n", module->getModuleIdentifier().c_str());
        mg_printf(conn, "  <p>Target %s</p>\n", target.c_str());
        mg_printf(conn, "  <p>Data Layout %s</p>\n", module->getDataLayout().c_str());

        mg_printf(conn, "  <h3>Functions</h3>\n");
        llvm::Module::iterator function = module->begin();
        llvm::Module::iterator functionEnd = module->end();
        mg_printf(conn, "  <ul>\n");
        for( ; function != functionEnd; ++function ) {
            mg_printf( conn, "<li>%s</li>\n", function->getName().str().c_str() );
        }
        mg_printf(conn, "  </ul>\n");

        mg_printf(conn,
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

