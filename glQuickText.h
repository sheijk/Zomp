#ifndef _glQuickText_H_

    #define _glQuickText_H_

    /*
     *   Written 2004 by <mgix@mgix.com>
     *   This code is in the public domain
     *   See http://www.mgix.com/snippets/?GLQuickText for details
     */

    class glQuickText
    {
    public:

        static void stringBox(
            double      *box,
            double      scale,
            const char  *format,
            ...
        );

        static void printfAt(
            double      xPos,
            double      yPos,
            double      zPos,
            double      scale,
            const char  *format,
            ...
        );

        static double getFontHeight(
            double scale = 1.0
        );
    };

extern "C" {
    void glqtStringBox(float* box, float scale, const char* text);
    void glqtPrintfAt(float xpos, float ypos, float zpos, float scale, const char* text);
    float glqtGetFontHeight(float scale = 1.0);
}

#endif // _glQuickText_H_

