#include "zomputils.h"
#include "zompvm_impl.h"

#include <stdio.h>
#include <string.h>
#include <map>
#include <string>

#pragma warning(push, 0)
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#pragma warning(pop)

#include "mongoose.h"

#pragma comment(lib, "ws2_32.lib")


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

        const char* port = "8080";
        const char* options[] = { "listening_ports", port, NULL };
        mg_context* ctx = mg_start(&callback, this, options);

        printf("Listening on port %s\n", port);
        do {
            printf("Press 'q' to exit\n");
            fflush(stdout);
        }
        while( getchar() != 'q' );

        mg_stop(ctx);

        zompShutdown();
        state_ = StateNotInitialized;
    }

private:

    void printHead(mg_connection* conn, const char* title)
    {
        mg_printf(conn,
            "HTTP/1.1 200 OK\r\n"
            "Content-Type: text/html\r\n\r\n"

            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            "<!DOCTYPE html>\n"
            "<html>\n"
            "  <head><title>ZompVM - %s</title></head>\n"
            "  <body>\n",
            title);
    }

    void printFoot(mg_connection* conn)
    {
        mg_printf(conn,
            " </body>\n"
            "</html>\n");
    }

    void pageIndex(mg_connection* conn, const mg_request_info* requestInfo)
    {
        printHead(conn, "main");

        mg_printf(conn,
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

        mg_printf(conn, "  <a href=\"test/debug?foo=bar?blah=blubber\">Html debug page</a>");

        mg_printf(conn, "<h1>Enter code</h1>\n");
        mg_printf(conn, "<p>Enter some code to be sent</p>\n");
        mg_printf(conn,
            "<form action=\"sendcode\" method=\"get\">\n"
            "<textarea name=\"code\" cols=\"100\" rows=\"20\"></textarea>\n"
            "<input type=\"submit\" value=\"Send\">\n"
            "</form>\n");

        mg_printf(conn,
            "<form action=\"test/debug\" method=\"post\" enctype=\"multipart/form-data\">\n"
            "<input name=\"code\" type=\"file\" size=\"50\" accept=\"text/*\"></input>\n"
            "<input type=\"submit\" value=\"Upload\">\n"
            "</form>\n");

        mg_printf(conn,
            "<form action=\"runfunction\" method=\"get\">\n"
            "<input name=\"function\" size=rows=\"50\"></input>\n"
            "<input type=\"submit\" value=\"Run\">\n"
            "</form>\n");

        printFoot(conn);
    }

    void tr2(mg_connection* conn, const char* a, const char* b)
    {
        mg_printf(conn, "<tr> <th>%s</th> <th>%s</th> </tr>\n", a, b);
    }

    void tr2(mg_connection* conn, const char* a, int b)
    {
        mg_printf(conn, "<tr> <th>%s</th> <th>%d</th> </tr>\n", a, b);
    }

    void pageDebug(mg_connection* conn, const mg_request_info* requestInfo)
    {
        printHead(conn, "debug");
        mg_printf(conn, "  <table>\n");
        tr2(conn, "query string", requestInfo->query_string);
        tr2(conn, "uri", requestInfo->uri);
        tr2(conn, "user", requestInfo->remote_user);
        tr2(conn, "ssl", requestInfo->is_ssl ? "yes" : "no");
        tr2(conn, "request method", requestInfo->request_method);
        tr2(conn, "status code", requestInfo->status_code);
        mg_printf(conn, "  </table>\n");
        printFoot(conn);
    }

    void pageRunfunction(mg_connection* conn, const mg_request_info* requestInfo)
    {
        printHead(conn, "run");

        char* ptr = requestInfo->query_string;
        while(true)
        {
            const char* key = ptr;

            while(true) {
                if(*ptr == '=' || *ptr == '\0')
                    break;
                ++ptr;
            }

            if(*ptr == '\0')
                break;

            ++ptr;
            const char* value = ptr;

            while(true) {
                if(*ptr == '&' || *ptr == '\0')
                    break;
                ++ptr;
            }

            // handle key and value
            if(strncmp(key, "function=", 9) == 0) {
                char endChar = *ptr;
                *ptr = '\0';

                // decode
                std::vector<char> decodedText;
                decodedText.resize(ptr - value + 1);
                
                char* write = &decodedText[0];
                for(const char* read = value; read < ptr; ++read, ++write) {
                    if(*read == '%') {
                        char c0 = hexDigitValue(read[1]);
                        char c1 = hexDigitValue(read[2]);
                        *write = c0 * 16 + c1;
                        read += 2;
                    }
                    else if(*read == '+') {
                        *write = ' ';
                    }
                    else {
                        *write = *read;
                    }
                }
                *write = '\0';

                // evaluate code
                printf("=== Running LLVM function: %s\n", &decodedText[0]);
                fflush(stdout);
                zompRunFunction(&decodedText[0]);
                printf("===\n");
                fflush(stdout);
                mg_printf(conn, "<p>Ok</p>\n");
                *ptr = endChar;
            }

            if(*ptr == '\0')
                break;

            ++ptr;
        }

        printFoot(conn);
    }

    void pageUnknown(mg_connection* conn, const mg_request_info* requestInfo)
    {
        printHead(conn, "error");
        mg_printf(conn, "    <p>404</p>\n");
        printFoot(conn);
    }

    static int hexDigitValue(char chr) {
        if(chr >= '0' && chr <= '9') {
            return chr - '0';
        }
        else if(chr >= 'a' && chr <= 'f') {
            return chr - 'a' + 10;
        }
        else if(chr >= 'A' && chr <= 'F') {
            return chr - 'A' + 10;
        }
        else {
            ZMP_ASSERT(false,);
            return 0;
        }
    }

    void pageSendcode(mg_connection* conn, const mg_request_info* requestInfo)
    {
        printHead(conn, "send code");

        char* ptr = requestInfo->query_string;
        while(true)
        {
            const char* key = ptr;

            while(true) {
                if(*ptr == '=' || *ptr == '\0')
                    break;
                ++ptr;
            }

            if(*ptr == '\0')
                break;

            ++ptr;
            const char* value = ptr;

            while(true) {
                if(*ptr == '&' || *ptr == '\0')
                    break;
                ++ptr;
            }

            // handle key and value
            if(strncmp(key, "code=", 5) == 0) {
                char endChar = *ptr;
                *ptr = '\0';

                // decode
                std::vector<char> decodedText;
                decodedText.resize(ptr - value + 1);
                
                char* write = &decodedText[0];
                for(const char* read = value; read < ptr; ++read, ++write) {
                    if(*read == '%') {
                        char c0 = hexDigitValue(read[1]);
                        char c1 = hexDigitValue(read[2]);
                        *write = c0 * 16 + c1;
                        read += 2;
                    }
                    else if(*read == '+') {
                        *write = ' ';
                    }
                    else {
                        *write = *read;
                    }
                }
                *write = '\0';

                // evaluate code
                printf("=== Evaluating LLVM code ====\n%s\n===\n", &decodedText[0]);
                fflush(stdout);
                bool success = zompSendCode(&decodedText[0], "runtime");
                if(success) {
                    mg_printf(conn, "<p>Ok</p>\n");
                }
                else {
                    mg_printf(conn, "<p>Error</p>\n");
                }
                *ptr = endChar;
            }

            if(*ptr == '\0')
                break;

            ++ptr;
        }

        printFoot(conn);
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
            else if(uri == "sendcode")
            {
                pageSendcode(conn, requestInfo);
            }
            else if(uri == "runfunction")
            {
                pageRunfunction(conn, requestInfo);
            }
            else if(uri == "test/debug")
            {
                pageDebug(conn, requestInfo);
            }
            else
            {
                pageUnknown(conn, requestInfo);
            }

            return (void*)"";  // Mark as processed
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

