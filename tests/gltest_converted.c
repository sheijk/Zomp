/* Provide Declarations */
#include <stdarg.h>
#include <setjmp.h>
/* get a declaration for alloca */
#if defined(__CYGWIN__) || defined(__MINGW32__)
#define  alloca(x) __builtin_alloca((x))
#define _alloca(x) __builtin_alloca((x))
#elif defined(__APPLE__)
extern void *__builtin_alloca(unsigned long);
#define alloca(x) __builtin_alloca(x)
#define longjmp _longjmp
#define setjmp _setjmp
#elif defined(__sun__)
#if defined(__sparcv9)
extern void *__builtin_alloca(unsigned long);
#else
extern void *__builtin_alloca(unsigned int);
#endif
#define alloca(x) __builtin_alloca(x)
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
#define alloca(x) __builtin_alloca(x)
#elif defined(_MSC_VER)
#define inline _inline
#define alloca(x) _alloca(x)
#else
#include <alloca.h>
#endif

#ifndef __GNUC__  /* Can only support "linkonce" vars with GCC */
#define __attribute__(X)
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __EXTERNAL_WEAK__ __attribute__((weak_import))
#elif defined(__GNUC__)
#define __EXTERNAL_WEAK__ __attribute__((weak))
#else
#define __EXTERNAL_WEAK__
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __ATTRIBUTE_WEAK__
#elif defined(__GNUC__)
#define __ATTRIBUTE_WEAK__ __attribute__((weak))
#else
#define __ATTRIBUTE_WEAK__
#endif

#if defined(__GNUC__)
#define __HIDDEN__ __attribute__((visibility("hidden")))
#endif

#ifdef __GNUC__
#define LLVM_NAN(NanStr)   __builtin_nan(NanStr)   /* Double */
#define LLVM_NANF(NanStr)  __builtin_nanf(NanStr)  /* Float */
#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */
#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */
#define LLVM_INF           __builtin_inf()         /* Double */
#define LLVM_INFF          __builtin_inff()        /* Float */
#define LLVM_PREFETCH(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define __ATTRIBUTE_CTOR__ __attribute__((constructor))
#define __ATTRIBUTE_DTOR__ __attribute__((destructor))
#define LLVM_ASM           __asm__
#else
#define LLVM_NAN(NanStr)   ((double)0.0)           /* Double */
#define LLVM_NANF(NanStr)  0.0F                    /* Float */
#define LLVM_NANS(NanStr)  ((double)0.0)           /* Double */
#define LLVM_NANSF(NanStr) 0.0F                    /* Float */
#define LLVM_INF           ((double)0.0)           /* Double */
#define LLVM_INFF          0.0F                    /* Float */
#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */
#define __ATTRIBUTE_CTOR__
#define __ATTRIBUTE_DTOR__
#define LLVM_ASM(X)
#endif

#if __GNUC__ < 4 /* Old GCC's, or compilers not GCC */ 
#define __builtin_stack_save() 0   /* not implemented */
#define __builtin_stack_restore(X) /* noop */
#endif

#define CODE_FOR_MAIN() /* Any target-specific code for main()*/

#ifndef __cplusplus
typedef unsigned char bool;
#endif

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <GL/glew.h>
#include <OpenGL/gl.h>
#include <GL/glfw.h>

/* Support for floating point constants */
typedef unsigned long long ConstantDoubleTy;
typedef unsigned int        ConstantFloatTy;


/* Global Declarations */
/* Helper union for bitcasts */
typedef union {
  unsigned int Int32;
  unsigned long long Int64;
  float Float;
  double Double;
} llvmBitCastUnion;
/* Structure forward decls */
struct l_ast;
struct l_framebuffer;
struct l_string;

/* Typedefs */
typedef struct l_ast l_ast;
typedef struct l_framebuffer l_framebuffer;
typedef struct l_string l_string;

/* Structure contents */
struct l_ast {
  unsigned char *field0;
  unsigned int field1;
  struct l_ast **field2;
};

struct l_framebuffer {
  unsigned int field0;
  unsigned int field1;
  unsigned int field2;
  unsigned int field3;
  unsigned int field4;
  unsigned int field5;
  unsigned int field6;
};

struct l_string {
  unsigned int field0;
  unsigned char *field1;
};


/* External Global Variable Declarations */
extern unsigned char *tempvar_1;
extern unsigned char *tempvar_2;
extern unsigned char *tempvar_3;
extern unsigned char *tempvar_4;
extern unsigned char *tempvar_5;
extern unsigned char *tempvar_6;
extern unsigned char *tempvar_7;
extern unsigned char *tempvar_8;
extern unsigned char *tempvar_9;
extern unsigned char *tempvar_10;
extern unsigned char *tempvar_11;
extern unsigned char *tempvar_12;
extern unsigned char *tempvar_13;
extern unsigned char *tempvar_14;
extern unsigned char *tempvar_15;
extern unsigned int lastid;
extern unsigned char *tempvar_16;
extern unsigned char *tempvar_17;
extern unsigned char *tempvar_385;
extern unsigned char *tempvar_386;
extern unsigned char *tempvar_387;
extern unsigned char *tempvar_388;
extern unsigned char *tempvar_389;
extern unsigned char *tempvar_390;
extern unsigned char *tempvar_540;
extern unsigned char *tempvar_3745;
extern unsigned char *tempvar_3746;
extern unsigned char *tempvar_3747;
extern unsigned char *tempvar_3748;
extern unsigned char *tempvar_3749;
extern unsigned char *tempvar_3750;
extern unsigned char *tempvar_3781;
extern unsigned char *tempvar_3813;
extern unsigned char *tempvar_3822;
extern unsigned int windowWidth;
extern unsigned int windowHeight;
extern bool initialized;
extern unsigned char *tempvar_4461;
extern unsigned char *tempvar_4462;
extern unsigned char *tempvar_4504;
extern unsigned char *tempvar_4505;
extern unsigned char *tempvar_4506;
extern unsigned char *tempvar_4507;
extern unsigned char *tempvar_4508;
extern unsigned char *tempvar_4509;
extern unsigned char *tempvar_4510;
extern unsigned char *tempvar_4511;
extern unsigned char *tempvar_4512;
extern unsigned char *tempvar_4513;
extern unsigned char *tempvar_4514;
extern unsigned char *tempvar_4515;
extern unsigned char *tempvar_4516;
extern double autoTimeout;
extern unsigned char *tempvar_4579;
extern unsigned char *tempvar_4580;
extern unsigned char *tempvar_4908;

/* Function Declarations */
/* double fmod(double, double); */
/* float fmodf(float, float); */
void printString(unsigned char *llvm_cbe_str);
/* unsigned int printf(unsigned char *, ...); */
void printInt(unsigned int llvm_cbe_i);
void printFloat(float llvm_cbe_f);
void printDouble(double llvm_cbe_d);
void printChar(signed char llvm_cbe_c);
/* unsigned int putchar(unsigned int ); */
void printNewline(void);
unsigned int *nullptr(void);
void stdlibHello(void);
/* unsigned int puts(unsigned char *); */
unsigned char *int2cstring(unsigned int llvm_cbe_i);
/* unsigned int sprintf(unsigned char *, unsigned char *, ...); */
/* unsigned int strlen(unsigned char *); */
/* unsigned char *malloc(unsigned int ); */
/* unsigned char *strcpy(unsigned char *, unsigned char *); */
void zompHello(void);
/* void exit(unsigned int ); */
/* double sin(double ); */
/* double cos(double ); */
/* float sinf(float ); */
/* float cosf(float ); */
/* float atanf(float ); */
/* float asinf(float ); */
/* float acosf(float ); */
unsigned int zompLoadLib(unsigned char *);
bool bool_2E_not(bool llvm_cbe_b);
void printBool(bool llvm_cbe_b);
void println(void);
void printlnInt(unsigned int llvm_cbe_v);
void printlnString(unsigned char *llvm_cbe_v);
unsigned int length(unsigned char *llvm_cbe_s);
struct l_string *makestring(unsigned char *llvm_cbe_cstr);
struct l_string *newString(unsigned int llvm_cbe_length, signed char llvm_cbe_init);
struct l_string *append(struct l_string *llvm_cbe_l, struct l_string *llvm_cbe_r);
void printIndent(unsigned int llvm_cbe_count);
/* unsigned int strcmp(unsigned char *, unsigned char *); */
bool cstring_2E_equal(unsigned char *llvm_cbe_l, unsigned char *llvm_cbe_r);
struct l_ast *simpleAst(unsigned char *llvm_cbe_name);
struct l_ast *astFromString(unsigned char *llvm_cbe_name);
struct l_ast *seqAst(void);
struct l_ast *astFromInt(unsigned int llvm_cbe_i);
void printAst(struct l_ast *llvm_cbe_a, unsigned int llvm_cbe_indent);
void addChild(struct l_ast *llvm_cbe_tree, struct l_ast *llvm_cbe_child);
struct l_ast *astChild(struct l_ast *llvm_cbe_parent, unsigned int llvm_cbe_index);
unsigned char *macroAstId(unsigned int llvm_cbe_macroCurrentAst);
unsigned int macroAstChildCount(unsigned int llvm_cbe_macroCurrentAst);
unsigned int macroAstChild(unsigned int llvm_cbe_treeaddr, unsigned int llvm_cbe_num);
struct l_ast *testMacro2(struct l_ast *llvm_cbe_lastArg);
unsigned int macroExec(void);
struct l_ast *macroTest(void);
unsigned int newUniqueId(void);
unsigned char *cstrings_2E_append(unsigned char *llvm_cbe_l, unsigned char *llvm_cbe_r);
unsigned char *newUniqueName(void);
unsigned char *newUniqueNameFor(unsigned char *llvm_cbe_purpose);
void printHLine(signed char llvm_cbe_chr, unsigned int llvm_cbe_count);
void ast_2E_setChild(struct l_ast *llvm_cbe_parent, unsigned int llvm_cbe_index, struct l_ast *llvm_cbe_newChild);
void ast_2E_replace(struct l_ast *llvm_cbe_tree, unsigned char *llvm_cbe_placeholder, struct l_ast *llvm_cbe_replacement);
int main(void);
void runMain(void);
void test(void);
/* void gluPerspective(double , double , double , double ); */
/* void gluLookAt(double , double , double , double , double , double , double , double , double ); */
void printGLError(void);
void printGlfwVersion(void);
bool checkAndReportShaderError(unsigned int llvm_cbe_shader, unsigned char *llvm_cbe_shaderName);
unsigned int createShader(unsigned char *llvm_cbe_vertexSource, unsigned char *llvm_cbe_fragmentSource);
void renderStripes(float llvm_cbe_count, float llvm_cbe_size, float llvm_cbe_alpha);
void drawPausedOverlay(void);
void renderQuad(float llvm_cbe_size);
void drawCoordSys(float llvm_cbe_length);
void drawOrientationGrid(void);
void sendCircle(void);
void autoSetupViewport(void);
unsigned int makeFramebuffer(void);
unsigned int makeTexture(void);
unsigned int makeRenderbuffer(void);
void glBindTexture2D(unsigned int llvm_cbe_texture);
struct l_framebuffer *fbo_make(unsigned int llvm_cbe_width, unsigned int llvm_cbe_height);
void fbo_attachColorTexture(struct l_framebuffer *llvm_cbe_fbo);
void fbo_create(struct l_framebuffer *llvm_cbe_fbo);
void fbo_bind(struct l_framebuffer *llvm_cbe_fbo);
void fbo_unbind(struct l_framebuffer *llvm_cbe_fbo);
void fbo_makeColorTexture(struct l_framebuffer *llvm_cbe_fbo);
bool isPressed(unsigned int llvm_cbe_key);
bool init(void);
unsigned int makeBillboardGShader(void);
unsigned int makeBillboard(unsigned int llvm_cbe_width, unsigned int llvm_cbe_height);
unsigned int makeMainShader(void);
/* unsigned int rand(void); */
unsigned int random(unsigned int llvm_cbe_max);
void long_(void);
void short_(void);
/* void free(unsigned char *); */
void abort(void);


/* Global Variable Declarations */
/* static unsigned char _2E_str[3]; */
/* static unsigned char _2E_str1[3]; */
/* static unsigned char _2E_str2[3]; */
/* static unsigned char _2E_str3[14]; */
/* static unsigned char temp113[4]; */
/* extern unsigned char *tempvar_1; */
/* static unsigned char temp119[3]; */
/* extern unsigned char *tempvar_2; */
/* static unsigned char temp120[9]; */
/* extern unsigned char *tempvar_3; */
/* static unsigned char temp195[7]; */
/* extern unsigned char *tempvar_4; */
/* static unsigned char temp196[7]; */
/* extern unsigned char *tempvar_5; */
/* static unsigned char temp197[5]; */
/* extern unsigned char *tempvar_6; */
/* static unsigned char temp198[5]; */
/* extern unsigned char *tempvar_7; */
/* static unsigned char temp199[5]; */
/* extern unsigned char *tempvar_8; */
/* static unsigned char temp200[2]; */
/* extern unsigned char *tempvar_9; */
/* static unsigned char temp201[2]; */
/* extern unsigned char *tempvar_10; */
/* static unsigned char temp202[2]; */
/* extern unsigned char *tempvar_11; */
/* static unsigned char temp239[4]; */
/* extern unsigned char *tempvar_12; */
/* static unsigned char temp240[4]; */
/* extern unsigned char *tempvar_13; */
/* static unsigned char temp252[6]; */
/* extern unsigned char *tempvar_14; */
/* static unsigned char temp253[5]; */
/* extern unsigned char *tempvar_15; */
/* extern unsigned int lastid; */
/* static unsigned char temp272[6]; */
/* extern unsigned char *tempvar_16; */
/* static unsigned char temp279[2]; */
/* extern unsigned char *tempvar_17; */
/* static unsigned char temp2407[17]; */
/* extern unsigned char *tempvar_385; */
/* static unsigned char temp2408[10]; */
/* extern unsigned char *tempvar_386; */
/* static unsigned char temp2409[6]; */
/* extern unsigned char *tempvar_387; */
/* static unsigned char temp2410[9]; */
/* extern unsigned char *tempvar_388; */
/* static unsigned char temp2411[7]; */
/* extern unsigned char *tempvar_389; */
/* static unsigned char temp2412[11]; */
/* extern unsigned char *tempvar_390; */
/* static unsigned char temp3271[17]; */
/* extern unsigned char *tempvar_540; */
/* extern unsigned int GL_ACCUM; */
/* extern unsigned int GL_LOAD; */
/* extern unsigned int GL_RETURN; */
/* extern unsigned int GL_MULT; */
/* extern unsigned int GL_ADD; */
/* extern unsigned int GL_NEVER; */
/* extern unsigned int GL_LESS; */
/* extern unsigned int GL_EQUAL; */
/* extern unsigned int GL_LEQUAL; */
/* extern unsigned int GL_GREATER; */
/* extern unsigned int GL_NOTEQUAL; */
/* extern unsigned int GL_GEQUAL; */
/* extern unsigned int GL_ALWAYS; */
/* extern unsigned int GL_CURRENT_BIT; */
/* extern unsigned int GL_POINT_BIT; */
/* extern unsigned int GL_LINE_BIT; */
/* extern unsigned int GL_POLYGON_BIT; */
/* extern unsigned int GL_POLYGON_STIPPLE_BIT; */
/* extern unsigned int GL_PIXEL_MODE_BIT; */
/* extern unsigned int GL_LIGHTING_BIT; */
/* extern unsigned int GL_FOG_BIT; */
/* extern unsigned int GL_DEPTH_BUFFER_BIT; */
/* extern unsigned int GL_ACCUM_BUFFER_BIT; */
/* extern unsigned int GL_STENCIL_BUFFER_BIT; */
/* extern unsigned int GL_VIEWPORT_BIT; */
/* extern unsigned int GL_TRANSFORM_BIT; */
/* extern unsigned int GL_ENABLE_BIT; */
/* extern unsigned int GL_COLOR_BUFFER_BIT; */
/* extern unsigned int GL_HINT_BIT; */
/* extern unsigned int GL_EVAL_BIT; */
/* extern unsigned int GL_LIST_BIT; */
/* extern unsigned int GL_TEXTURE_BIT; */
/* extern unsigned int GL_SCISSOR_BIT; */
/* extern unsigned int GL_ALL_ATTRIB_BITS; */
/* extern unsigned int GL_POINTS; */
/* extern unsigned int GL_LINES; */
/* extern unsigned int GL_LINE_LOOP; */
/* extern unsigned int GL_LINE_STRIP; */
/* extern unsigned int GL_TRIANGLES; */
/* extern unsigned int GL_TRIANGLE_STRIP; */
/* extern unsigned int GL_TRIANGLE_FAN; */
/* extern unsigned int GL_QUADS; */
/* extern unsigned int GL_QUAD_STRIP; */
/* extern unsigned int GL_POLYGON; */
/* extern unsigned int GL_ZERO; */
/* extern unsigned int GL_ONE; */
/* extern unsigned int GL_SRC_COLOR; */
/* extern unsigned int GL_ONE_MINUS_SRC_COLOR; */
/* extern unsigned int GL_SRC_ALPHA; */
/* extern unsigned int GL_ONE_MINUS_SRC_ALPHA; */
/* extern unsigned int GL_DST_ALPHA; */
/* extern unsigned int GL_ONE_MINUS_DST_ALPHA; */
/* extern unsigned int GL_DST_COLOR; */
/* extern unsigned int GL_ONE_MINUS_DST_COLOR; */
/* extern unsigned int GL_SRC_ALPHA_SATURATE; */
/* extern unsigned int GL_TRUE; */
/* extern unsigned int GL_FALSE; */
/* extern unsigned int GL_CLIP_PLANE0; */
/* extern unsigned int GL_CLIP_PLANE1; */
/* extern unsigned int GL_CLIP_PLANE2; */
/* extern unsigned int GL_CLIP_PLANE3; */
/* extern unsigned int GL_CLIP_PLANE4; */
/* extern unsigned int GL_CLIP_PLANE5; */
/* extern unsigned int GL_BYTE; */
/* extern unsigned int GL_UNSIGNED_BYTE; */
/* extern unsigned int GL_SHORT; */
/* extern unsigned int GL_UNSIGNED_SHORT; */
/* extern unsigned int GL_INT; */
/* extern unsigned int GL_UNSIGNED_INT; */
/* extern unsigned int GL_FLOAT; */
/* extern unsigned int GL_2_BYTES; */
/* extern unsigned int GL_3_BYTES; */
/* extern unsigned int GL_4_BYTES; */
/* extern unsigned int GL_DOUBLE; */
/* extern unsigned int GL_NONE; */
/* extern unsigned int GL_FRONT_LEFT; */
/* extern unsigned int GL_FRONT_RIGHT; */
/* extern unsigned int GL_BACK_LEFT; */
/* extern unsigned int GL_BACK_RIGHT; */
/* extern unsigned int GL_FRONT; */
/* extern unsigned int GL_BACK; */
/* extern unsigned int GL_LEFT; */
/* extern unsigned int GL_RIGHT; */
/* extern unsigned int GL_FRONT_AND_BACK; */
/* extern unsigned int GL_AUX0; */
/* extern unsigned int GL_AUX1; */
/* extern unsigned int GL_AUX2; */
/* extern unsigned int GL_AUX3; */
/* extern unsigned int GL_NO_ERROR; */
/* extern unsigned int GL_INVALID_ENUM; */
/* extern unsigned int GL_INVALID_VALUE; */
/* extern unsigned int GL_INVALID_OPERATION; */
/* extern unsigned int GL_STACK_OVERFLOW; */
/* extern unsigned int GL_STACK_UNDERFLOW; */
/* extern unsigned int GL_OUT_OF_MEMORY; */
/* extern unsigned int GL_2D; */
/* extern unsigned int GL_3D; */
/* extern unsigned int GL_3D_COLOR; */
/* extern unsigned int GL_3D_COLOR_TEXTURE; */
/* extern unsigned int GL_4D_COLOR_TEXTURE; */
/* extern unsigned int GL_PASS_THROUGH_TOKEN; */
/* extern unsigned int GL_POINT_TOKEN; */
/* extern unsigned int GL_LINE_TOKEN; */
/* extern unsigned int GL_POLYGON_TOKEN; */
/* extern unsigned int GL_BITMAP_TOKEN; */
/* extern unsigned int GL_DRAW_PIXEL_TOKEN; */
/* extern unsigned int GL_COPY_PIXEL_TOKEN; */
/* extern unsigned int GL_LINE_RESET_TOKEN; */
/* extern unsigned int GL_EXP; */
/* extern unsigned int GL_EXP2; */
/* extern unsigned int GL_CW; */
/* extern unsigned int GL_CCW; */
/* extern unsigned int GL_COEFF; */
/* extern unsigned int GL_ORDER; */
/* extern unsigned int GL_DOMAIN; */
/* extern unsigned int GL_CURRENT_COLOR; */
/* extern unsigned int GL_CURRENT_INDEX; */
/* extern unsigned int GL_CURRENT_NORMAL; */
/* extern unsigned int GL_CURRENT_TEXTURE_COORDS; */
/* extern unsigned int GL_CURRENT_RASTER_COLOR; */
/* extern unsigned int GL_CURRENT_RASTER_INDEX; */
/* extern unsigned int GL_CURRENT_RASTER_TEXTURE_COORDS; */
/* extern unsigned int GL_CURRENT_RASTER_POSITION; */
/* extern unsigned int GL_CURRENT_RASTER_POSITION_VALID; */
/* extern unsigned int GL_CURRENT_RASTER_DISTANCE; */
/* extern unsigned int GL_POINT_SMOOTH; */
/* extern unsigned int GL_POINT_SIZE; */
/* extern unsigned int GL_POINT_SIZE_RANGE; */
/* extern unsigned int GL_POINT_SIZE_GRANULARITY; */
/* extern unsigned int GL_LINE_SMOOTH; */
/* extern unsigned int GL_LINE_WIDTH; */
/* extern unsigned int GL_LINE_WIDTH_RANGE; */
/* extern unsigned int GL_LINE_WIDTH_GRANULARITY; */
/* extern unsigned int GL_LINE_STIPPLE; */
/* extern unsigned int GL_LINE_STIPPLE_PATTERN; */
/* extern unsigned int GL_LINE_STIPPLE_REPEAT; */
/* extern unsigned int GL_LIST_MODE; */
/* extern unsigned int GL_MAX_LIST_NESTING; */
/* extern unsigned int GL_LIST_BASE; */
/* extern unsigned int GL_LIST_INDEX; */
/* extern unsigned int GL_POLYGON_MODE; */
/* extern unsigned int GL_POLYGON_SMOOTH; */
/* extern unsigned int GL_POLYGON_STIPPLE; */
/* extern unsigned int GL_EDGE_FLAG; */
/* extern unsigned int GL_CULL_FACE; */
/* extern unsigned int GL_CULL_FACE_MODE; */
/* extern unsigned int GL_FRONT_FACE; */
/* extern unsigned int GL_LIGHTING; */
/* extern unsigned int GL_LIGHT_MODEL_LOCAL_VIEWER; */
/* extern unsigned int GL_LIGHT_MODEL_TWO_SIDE; */
/* extern unsigned int GL_LIGHT_MODEL_AMBIENT; */
/* extern unsigned int GL_SHADE_MODEL; */
/* extern unsigned int GL_COLOR_MATERIAL_FACE; */
/* extern unsigned int GL_COLOR_MATERIAL_PARAMETER; */
/* extern unsigned int GL_COLOR_MATERIAL; */
/* extern unsigned int GL_FOG; */
/* extern unsigned int GL_FOG_INDEX; */
/* extern unsigned int GL_FOG_DENSITY; */
/* extern unsigned int GL_FOG_START; */
/* extern unsigned int GL_FOG_END; */
/* extern unsigned int GL_FOG_MODE; */
/* extern unsigned int GL_FOG_COLOR; */
/* extern unsigned int GL_DEPTH_RANGE; */
/* extern unsigned int GL_DEPTH_TEST; */
/* extern unsigned int GL_DEPTH_WRITEMASK; */
/* extern unsigned int GL_DEPTH_CLEAR_VALUE; */
/* extern unsigned int GL_DEPTH_FUNC; */
/* extern unsigned int GL_ACCUM_CLEAR_VALUE; */
/* extern unsigned int GL_STENCIL_TEST; */
/* extern unsigned int GL_STENCIL_CLEAR_VALUE; */
/* extern unsigned int GL_STENCIL_FUNC; */
/* extern unsigned int GL_STENCIL_VALUE_MASK; */
/* extern unsigned int GL_STENCIL_FAIL; */
/* extern unsigned int GL_STENCIL_PASS_DEPTH_FAIL; */
/* extern unsigned int GL_STENCIL_PASS_DEPTH_PASS; */
/* extern unsigned int GL_STENCIL_REF; */
/* extern unsigned int GL_STENCIL_WRITEMASK; */
/* extern unsigned int GL_MATRIX_MODE; */
/* extern unsigned int GL_NORMALIZE; */
/* extern unsigned int GL_VIEWPORT; */
/* extern unsigned int GL_MODELVIEW_STACK_DEPTH; */
/* extern unsigned int GL_PROJECTION_STACK_DEPTH; */
/* extern unsigned int GL_TEXTURE_STACK_DEPTH; */
/* extern unsigned int GL_MODELVIEW_MATRIX; */
/* extern unsigned int GL_PROJECTION_MATRIX; */
/* extern unsigned int GL_TEXTURE_MATRIX; */
/* extern unsigned int GL_ATTRIB_STACK_DEPTH; */
/* extern unsigned int GL_CLIENT_ATTRIB_STACK_DEPTH; */
/* extern unsigned int GL_ALPHA_TEST; */
/* extern unsigned int GL_ALPHA_TEST_FUNC; */
/* extern unsigned int GL_ALPHA_TEST_REF; */
/* extern unsigned int GL_DITHER; */
/* extern unsigned int GL_BLEND_DST; */
/* extern unsigned int GL_BLEND_SRC; */
/* extern unsigned int GL_BLEND; */
/* extern unsigned int GL_LOGIC_OP_MODE; */
/* extern unsigned int GL_INDEX_LOGIC_OP; */
/* extern unsigned int GL_COLOR_LOGIC_OP; */
/* extern unsigned int GL_AUX_BUFFERS; */
/* extern unsigned int GL_DRAW_BUFFER; */
/* extern unsigned int GL_READ_BUFFER; */
/* extern unsigned int GL_SCISSOR_BOX; */
/* extern unsigned int GL_SCISSOR_TEST; */
/* extern unsigned int GL_INDEX_CLEAR_VALUE; */
/* extern unsigned int GL_INDEX_WRITEMASK; */
/* extern unsigned int GL_COLOR_CLEAR_VALUE; */
/* extern unsigned int GL_COLOR_WRITEMASK; */
/* extern unsigned int GL_INDEX_MODE; */
/* extern unsigned int GL_RGBA_MODE; */
/* extern unsigned int GL_DOUBLEBUFFER; */
/* extern unsigned int GL_STEREO; */
/* extern unsigned int GL_RENDER_MODE; */
/* extern unsigned int GL_PERSPECTIVE_CORRECTION_HINT; */
/* extern unsigned int GL_POINT_SMOOTH_HINT; */
/* extern unsigned int GL_LINE_SMOOTH_HINT; */
/* extern unsigned int GL_POLYGON_SMOOTH_HINT; */
/* extern unsigned int GL_FOG_HINT; */
/* extern unsigned int GL_TEXTURE_GEN_S; */
/* extern unsigned int GL_TEXTURE_GEN_T; */
/* extern unsigned int GL_TEXTURE_GEN_R; */
/* extern unsigned int GL_TEXTURE_GEN_Q; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_I; */
/* extern unsigned int GL_PIXEL_MAP_S_TO_S; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_R; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_G; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_B; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_A; */
/* extern unsigned int GL_PIXEL_MAP_R_TO_R; */
/* extern unsigned int GL_PIXEL_MAP_G_TO_G; */
/* extern unsigned int GL_PIXEL_MAP_B_TO_B; */
/* extern unsigned int GL_PIXEL_MAP_A_TO_A; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_I_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_S_TO_S_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_R_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_G_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_B_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_I_TO_A_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_R_TO_R_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_G_TO_G_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_B_TO_B_SIZE; */
/* extern unsigned int GL_PIXEL_MAP_A_TO_A_SIZE; */
/* extern unsigned int GL_UNPACK_SWAP_BYTES; */
/* extern unsigned int GL_UNPACK_LSB_FIRST; */
/* extern unsigned int GL_UNPACK_ROW_LENGTH; */
/* extern unsigned int GL_UNPACK_SKIP_ROWS; */
/* extern unsigned int GL_UNPACK_SKIP_PIXELS; */
/* extern unsigned int GL_UNPACK_ALIGNMENT; */
/* extern unsigned int GL_PACK_SWAP_BYTES; */
/* extern unsigned int GL_PACK_LSB_FIRST; */
/* extern unsigned int GL_PACK_ROW_LENGTH; */
/* extern unsigned int GL_PACK_SKIP_ROWS; */
/* extern unsigned int GL_PACK_SKIP_PIXELS; */
/* extern unsigned int GL_PACK_ALIGNMENT; */
/* extern unsigned int GL_MAP_COLOR; */
/* extern unsigned int GL_MAP_STENCIL; */
/* extern unsigned int GL_INDEX_SHIFT; */
/* extern unsigned int GL_INDEX_OFFSET; */
/* extern unsigned int GL_RED_SCALE; */
/* extern unsigned int GL_RED_BIAS; */
/* extern unsigned int GL_ZOOM_X; */
/* extern unsigned int GL_ZOOM_Y; */
/* extern unsigned int GL_GREEN_SCALE; */
/* extern unsigned int GL_GREEN_BIAS; */
/* extern unsigned int GL_BLUE_SCALE; */
/* extern unsigned int GL_BLUE_BIAS; */
/* extern unsigned int GL_ALPHA_SCALE; */
/* extern unsigned int GL_ALPHA_BIAS; */
/* extern unsigned int GL_DEPTH_SCALE; */
/* extern unsigned int GL_DEPTH_BIAS; */
/* extern unsigned int GL_MAX_EVAL_ORDER; */
/* extern unsigned int GL_MAX_LIGHTS; */
/* extern unsigned int GL_MAX_CLIP_PLANES; */
/* extern unsigned int GL_MAX_TEXTURE_SIZE; */
/* extern unsigned int GL_MAX_PIXEL_MAP_TABLE; */
/* extern unsigned int GL_MAX_ATTRIB_STACK_DEPTH; */
/* extern unsigned int GL_MAX_MODELVIEW_STACK_DEPTH; */
/* extern unsigned int GL_MAX_NAME_STACK_DEPTH; */
/* extern unsigned int GL_MAX_PROJECTION_STACK_DEPTH; */
/* extern unsigned int GL_MAX_TEXTURE_STACK_DEPTH; */
/* extern unsigned int GL_MAX_VIEWPORT_DIMS; */
/* extern unsigned int GL_MAX_CLIENT_ATTRIB_STACK_DEPTH; */
/* extern unsigned int GL_SUBPIXEL_BITS; */
/* extern unsigned int GL_INDEX_BITS; */
/* extern unsigned int GL_RED_BITS; */
/* extern unsigned int GL_GREEN_BITS; */
/* extern unsigned int GL_BLUE_BITS; */
/* extern unsigned int GL_ALPHA_BITS; */
/* extern unsigned int GL_DEPTH_BITS; */
/* extern unsigned int GL_STENCIL_BITS; */
/* extern unsigned int GL_ACCUM_RED_BITS; */
/* extern unsigned int GL_ACCUM_GREEN_BITS; */
/* extern unsigned int GL_ACCUM_BLUE_BITS; */
/* extern unsigned int GL_ACCUM_ALPHA_BITS; */
/* extern unsigned int GL_NAME_STACK_DEPTH; */
/* extern unsigned int GL_AUTO_NORMAL; */
/* extern unsigned int GL_MAP1_COLOR_4; */
/* extern unsigned int GL_MAP1_INDEX; */
/* extern unsigned int GL_MAP1_NORMAL; */
/* extern unsigned int GL_MAP1_TEXTURE_COORD_1; */
/* extern unsigned int GL_MAP1_TEXTURE_COORD_2; */
/* extern unsigned int GL_MAP1_TEXTURE_COORD_3; */
/* extern unsigned int GL_MAP1_TEXTURE_COORD_4; */
/* extern unsigned int GL_MAP1_VERTEX_3; */
/* extern unsigned int GL_MAP1_VERTEX_4; */
/* extern unsigned int GL_MAP2_COLOR_4; */
/* extern unsigned int GL_MAP2_INDEX; */
/* extern unsigned int GL_MAP2_NORMAL; */
/* extern unsigned int GL_MAP2_TEXTURE_COORD_1; */
/* extern unsigned int GL_MAP2_TEXTURE_COORD_2; */
/* extern unsigned int GL_MAP2_TEXTURE_COORD_3; */
/* extern unsigned int GL_MAP2_TEXTURE_COORD_4; */
/* extern unsigned int GL_MAP2_VERTEX_3; */
/* extern unsigned int GL_MAP2_VERTEX_4; */
/* extern unsigned int GL_MAP1_GRID_DOMAIN; */
/* extern unsigned int GL_MAP1_GRID_SEGMENTS; */
/* extern unsigned int GL_MAP2_GRID_DOMAIN; */
/* extern unsigned int GL_MAP2_GRID_SEGMENTS; */
/* extern unsigned int GL_TEXTURE_1D; */
/* extern unsigned int GL_TEXTURE_2D; */
/* extern unsigned int GL_FEEDBACK_BUFFER_POINTER; */
/* extern unsigned int GL_FEEDBACK_BUFFER_SIZE; */
/* extern unsigned int GL_FEEDBACK_BUFFER_TYPE; */
/* extern unsigned int GL_SELECTION_BUFFER_POINTER; */
/* extern unsigned int GL_SELECTION_BUFFER_SIZE; */
/* extern unsigned int GL_TEXTURE_WIDTH; */
/* extern unsigned int GL_TEXTURE_HEIGHT; */
/* extern unsigned int GL_TEXTURE_INTERNAL_FORMAT; */
/* extern unsigned int GL_TEXTURE_BORDER_COLOR; */
/* extern unsigned int GL_TEXTURE_BORDER; */
/* extern unsigned int GL_DONT_CARE; */
/* extern unsigned int GL_FASTEST; */
/* extern unsigned int GL_NICEST; */
/* extern unsigned int GL_LIGHT0; */
/* extern unsigned int GL_LIGHT1; */
/* extern unsigned int GL_LIGHT2; */
/* extern unsigned int GL_LIGHT3; */
/* extern unsigned int GL_LIGHT4; */
/* extern unsigned int GL_LIGHT5; */
/* extern unsigned int GL_LIGHT6; */
/* extern unsigned int GL_LIGHT7; */
/* extern unsigned int GL_AMBIENT; */
/* extern unsigned int GL_DIFFUSE; */
/* extern unsigned int GL_SPECULAR; */
/* extern unsigned int GL_POSITION; */
/* extern unsigned int GL_SPOT_DIRECTION; */
/* extern unsigned int GL_SPOT_EXPONENT; */
/* extern unsigned int GL_SPOT_CUTOFF; */
/* extern unsigned int GL_CONSTANT_ATTENUATION; */
/* extern unsigned int GL_LINEAR_ATTENUATION; */
/* extern unsigned int GL_QUADRATIC_ATTENUATION; */
/* extern unsigned int GL_COMPILE; */
/* extern unsigned int GL_COMPILE_AND_EXECUTE; */
/* extern unsigned int GL_CLEAR; */
/* extern unsigned int GL_AND; */
/* extern unsigned int GL_AND_REVERSE; */
/* extern unsigned int GL_COPY; */
/* extern unsigned int GL_AND_INVERTED; */
/* extern unsigned int GL_NOOP; */
/* extern unsigned int GL_XOR; */
/* extern unsigned int GL_OR; */
/* extern unsigned int GL_NOR; */
/* extern unsigned int GL_EQUIV; */
/* extern unsigned int GL_INVERT; */
/* extern unsigned int GL_OR_REVERSE; */
/* extern unsigned int GL_COPY_INVERTED; */
/* extern unsigned int GL_OR_INVERTED; */
/* extern unsigned int GL_NAND; */
/* extern unsigned int GL_SET; */
/* extern unsigned int GL_EMISSION; */
/* extern unsigned int GL_SHININESS; */
/* extern unsigned int GL_AMBIENT_AND_DIFFUSE; */
/* extern unsigned int GL_COLOR_INDEXES; */
/* extern unsigned int GL_MODELVIEW; */
/* extern unsigned int GL_PROJECTION; */
/* extern unsigned int GL_TEXTURE; */
/* extern unsigned int GL_COLOR; */
/* extern unsigned int GL_DEPTH; */
/* extern unsigned int GL_STENCIL; */
/* extern unsigned int GL_COLOR_INDEX; */
/* extern unsigned int GL_STENCIL_INDEX; */
/* extern unsigned int GL_DEPTH_COMPONENT; */
/* extern unsigned int GL_RED; */
/* extern unsigned int GL_GREEN; */
/* extern unsigned int GL_BLUE; */
/* extern unsigned int GL_ALPHA; */
/* extern unsigned int GL_RGB; */
/* extern unsigned int GL_RGBA; */
/* extern unsigned int GL_LUMINANCE; */
/* extern unsigned int GL_LUMINANCE_ALPHA; */
/* extern unsigned int GL_BITMAP; */
/* extern unsigned int GL_POINT; */
/* extern unsigned int GL_LINE; */
/* extern unsigned int GL_FILL; */
/* extern unsigned int GL_RENDER; */
/* extern unsigned int GL_FEEDBACK; */
/* extern unsigned int GL_SELECT; */
/* extern unsigned int GL_FLAT; */
/* extern unsigned int GL_SMOOTH; */
/* extern unsigned int GL_KEEP; */
/* extern unsigned int GL_REPLACE; */
/* extern unsigned int GL_INCR; */
/* extern unsigned int GL_DECR; */
/* extern unsigned int GL_VENDOR; */
/* extern unsigned int GL_RENDERER; */
/* extern unsigned int GL_VERSION; */
/* extern unsigned int GL_S; */
/* extern unsigned int GL_T; */
/* extern unsigned int GL_R; */
/* extern unsigned int GL_Q; */
/* extern unsigned int GL_MODULATE; */
/* extern unsigned int GL_DECAL; */
/* extern unsigned int GL_TEXTURE_ENV_MODE; */
/* extern unsigned int GL_TEXTURE_ENV_COLOR; */
/* extern unsigned int GL_TEXTURE_ENV; */
/* extern unsigned int GL_EYE_LINEAR; */
/* extern unsigned int GL_OBJECT_LINEAR; */
/* extern unsigned int GL_SPHERE_MAP; */
/* extern unsigned int GL_TEXTURE_GEN_MODE; */
/* extern unsigned int GL_OBJECT_PLANE; */
/* extern unsigned int GL_EYE_PLANE; */
/* extern unsigned int GL_NEAREST; */
/* extern unsigned int GL_LINEAR; */
/* extern unsigned int GL_NEAREST_MIPMAP_NEAREST; */
/* extern unsigned int GL_LINEAR_MIPMAP_NEAREST; */
/* extern unsigned int GL_NEAREST_MIPMAP_LINEAR; */
/* extern unsigned int GL_LINEAR_MIPMAP_LINEAR; */
/* extern unsigned int GL_TEXTURE_MAG_FILTER; */
/* extern unsigned int GL_TEXTURE_MIN_FILTER; */
/* extern unsigned int GL_TEXTURE_WRAP_S; */
/* extern unsigned int GL_TEXTURE_WRAP_T; */
/* extern unsigned int GL_CLAMP; */
/* extern unsigned int GL_REPEAT; */
/* extern unsigned int GL_CLIENT_PIXEL_STORE_BIT; */
/* extern unsigned int GL_CLIENT_VERTEX_ARRAY_BIT; */
/* extern unsigned int GL_CLIENT_ALL_ATTRIB_BITS; */
/* extern unsigned int GL_POLYGON_OFFSET_FACTOR; */
/* extern unsigned int GL_POLYGON_OFFSET_UNITS; */
/* extern unsigned int GL_POLYGON_OFFSET_POINT; */
/* extern unsigned int GL_POLYGON_OFFSET_LINE; */
/* extern unsigned int GL_POLYGON_OFFSET_FILL; */
/* extern unsigned int GL_ALPHA4; */
/* extern unsigned int GL_ALPHA8; */
/* extern unsigned int GL_ALPHA12; */
/* extern unsigned int GL_ALPHA16; */
/* extern unsigned int GL_LUMINANCE4; */
/* extern unsigned int GL_LUMINANCE8; */
/* extern unsigned int GL_LUMINANCE12; */
/* extern unsigned int GL_LUMINANCE16; */
/* extern unsigned int GL_LUMINANCE4_ALPHA4; */
/* extern unsigned int GL_LUMINANCE6_ALPHA2; */
/* extern unsigned int GL_LUMINANCE8_ALPHA8; */
/* extern unsigned int GL_LUMINANCE12_ALPHA4; */
/* extern unsigned int GL_LUMINANCE12_ALPHA12; */
/* extern unsigned int GL_LUMINANCE16_ALPHA16; */
/* extern unsigned int GL_INTENSITY; */
/* extern unsigned int GL_INTENSITY4; */
/* extern unsigned int GL_INTENSITY8; */
/* extern unsigned int GL_INTENSITY12; */
/* extern unsigned int GL_INTENSITY16; */
/* extern unsigned int GL_R3_G3_B2; */
/* extern unsigned int GL_RGB4; */
/* extern unsigned int GL_RGB5; */
/* extern unsigned int GL_RGB8; */
/* extern unsigned int GL_RGB10; */
/* extern unsigned int GL_RGB12; */
/* extern unsigned int GL_RGB16; */
/* extern unsigned int GL_RGBA2; */
/* extern unsigned int GL_RGBA4; */
/* extern unsigned int GL_RGB5_A1; */
/* extern unsigned int GL_RGBA8; */
/* extern unsigned int GL_RGB10_A2; */
/* extern unsigned int GL_RGBA12; */
/* extern unsigned int GL_RGBA16; */
/* extern unsigned int GL_TEXTURE_RED_SIZE; */
/* extern unsigned int GL_TEXTURE_GREEN_SIZE; */
/* extern unsigned int GL_TEXTURE_BLUE_SIZE; */
/* extern unsigned int GL_TEXTURE_ALPHA_SIZE; */
/* extern unsigned int GL_TEXTURE_LUMINANCE_SIZE; */
/* extern unsigned int GL_TEXTURE_INTENSITY_SIZE; */
/* extern unsigned int GL_PROXY_TEXTURE_1D; */
/* extern unsigned int GL_PROXY_TEXTURE_2D; */
/* extern unsigned int GL_TEXTURE_PRIORITY; */
/* extern unsigned int GL_TEXTURE_RESIDENT; */
/* extern unsigned int GL_TEXTURE_BINDING_1D; */
/* extern unsigned int GL_TEXTURE_BINDING_2D; */
/* extern unsigned int GL_VERTEX_ARRAY; */
/* extern unsigned int GL_NORMAL_ARRAY; */
/* extern unsigned int GL_COLOR_ARRAY; */
/* extern unsigned int GL_INDEX_ARRAY; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY; */
/* extern unsigned int GL_EDGE_FLAG_ARRAY; */
/* extern unsigned int GL_VERTEX_ARRAY_SIZE; */
/* extern unsigned int GL_VERTEX_ARRAY_TYPE; */
/* extern unsigned int GL_VERTEX_ARRAY_STRIDE; */
/* extern unsigned int GL_NORMAL_ARRAY_TYPE; */
/* extern unsigned int GL_NORMAL_ARRAY_STRIDE; */
/* extern unsigned int GL_COLOR_ARRAY_SIZE; */
/* extern unsigned int GL_COLOR_ARRAY_TYPE; */
/* extern unsigned int GL_COLOR_ARRAY_STRIDE; */
/* extern unsigned int GL_INDEX_ARRAY_TYPE; */
/* extern unsigned int GL_INDEX_ARRAY_STRIDE; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY_SIZE; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY_TYPE; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY_STRIDE; */
/* extern unsigned int GL_EDGE_FLAG_ARRAY_STRIDE; */
/* extern unsigned int GL_VERTEX_ARRAY_POINTER; */
/* extern unsigned int GL_NORMAL_ARRAY_POINTER; */
/* extern unsigned int GL_COLOR_ARRAY_POINTER; */
/* extern unsigned int GL_INDEX_ARRAY_POINTER; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY_POINTER; */
/* extern unsigned int GL_EDGE_FLAG_ARRAY_POINTER; */
/* extern unsigned int GL_V2F; */
/* extern unsigned int GL_V3F; */
/* extern unsigned int GL_C4UB_V2F; */
/* extern unsigned int GL_C4UB_V3F; */
/* extern unsigned int GL_C3F_V3F; */
/* extern unsigned int GL_N3F_V3F; */
/* extern unsigned int GL_C4F_N3F_V3F; */
/* extern unsigned int GL_T2F_V3F; */
/* extern unsigned int GL_T4F_V4F; */
/* extern unsigned int GL_T2F_C4UB_V3F; */
/* extern unsigned int GL_T2F_C3F_V3F; */
/* extern unsigned int GL_T2F_N3F_V3F; */
/* extern unsigned int GL_T2F_C4F_N3F_V3F; */
/* extern unsigned int GL_T4F_C4F_N3F_V4F; */
/* extern unsigned int GL_TEXTURE_COMPONENTS; */
/* extern unsigned int GL_UNSIGNED_BYTE_3_3_2; */
/* extern unsigned int GL_UNSIGNED_SHORT_4_4_4_4; */
/* extern unsigned int GL_UNSIGNED_SHORT_5_5_5_1; */
/* extern unsigned int GL_UNSIGNED_INT_8_8_8_8; */
/* extern unsigned int GL_UNSIGNED_INT_10_10_10_2; */
/* extern unsigned int GL_RESCALE_NORMAL; */
/* extern unsigned int GL_UNSIGNED_BYTE_2_3_3_REV; */
/* extern unsigned int GL_UNSIGNED_SHORT_5_6_5; */
/* extern unsigned int GL_UNSIGNED_SHORT_5_6_5_REV; */
/* extern unsigned int GL_UNSIGNED_SHORT_4_4_4_4_REV; */
/* extern unsigned int GL_UNSIGNED_SHORT_1_5_5_5_REV; */
/* extern unsigned int GL_UNSIGNED_INT_8_8_8_8_REV; */
/* extern unsigned int GL_UNSIGNED_INT_2_10_10_10_REV; */
/* extern unsigned int GL_BGR; */
/* extern unsigned int GL_BGRA; */
/* extern unsigned int GL_MAX_ELEMENTS_VERTICES; */
/* extern unsigned int GL_MAX_ELEMENTS_INDICES; */
/* extern unsigned int GL_CLAMP_TO_EDGE; */
/* extern unsigned int GL_TEXTURE_MIN_LOD; */
/* extern unsigned int GL_TEXTURE_MAX_LOD; */
/* extern unsigned int GL_TEXTURE_BASE_LEVEL; */
/* extern unsigned int GL_TEXTURE_MAX_LEVEL; */
/* extern unsigned int GL_LIGHT_MODEL_COLOR_CONTROL; */
/* extern unsigned int GL_SINGLE_COLOR; */
/* extern unsigned int GL_SEPARATE_SPECULAR_COLOR; */
/* extern unsigned int GL_SMOOTH_POINT_SIZE_RANGE; */
/* extern unsigned int GL_SMOOTH_POINT_SIZE_GRANULARITY; */
/* extern unsigned int GL_SMOOTH_LINE_WIDTH_RANGE; */
/* extern unsigned int GL_SMOOTH_LINE_WIDTH_GRANULARITY; */
/* extern unsigned int GL_ALIASED_POINT_SIZE_RANGE; */
/* extern unsigned int GL_ALIASED_LINE_WIDTH_RANGE; */
/* extern unsigned int GL_PACK_SKIP_IMAGES; */
/* extern unsigned int GL_PACK_IMAGE_HEIGHT; */
/* extern unsigned int GL_UNPACK_SKIP_IMAGES; */
/* extern unsigned int GL_UNPACK_IMAGE_HEIGHT; */
/* extern unsigned int GL_TEXTURE_3D; */
/* extern unsigned int GL_PROXY_TEXTURE_3D; */
/* extern unsigned int GL_TEXTURE_DEPTH; */
/* extern unsigned int GL_TEXTURE_WRAP_R; */
/* extern unsigned int GL_MAX_3D_TEXTURE_SIZE; */
/* extern unsigned int GL_TEXTURE_BINDING_3D; */
/* extern unsigned int GL_TEXTURE0; */
/* extern unsigned int GL_TEXTURE1; */
/* extern unsigned int GL_TEXTURE2; */
/* extern unsigned int GL_TEXTURE3; */
/* extern unsigned int GL_TEXTURE4; */
/* extern unsigned int GL_TEXTURE5; */
/* extern unsigned int GL_TEXTURE6; */
/* extern unsigned int GL_TEXTURE7; */
/* extern unsigned int GL_TEXTURE8; */
/* extern unsigned int GL_TEXTURE9; */
/* extern unsigned int GL_TEXTURE10; */
/* extern unsigned int GL_TEXTURE11; */
/* extern unsigned int GL_TEXTURE12; */
/* extern unsigned int GL_TEXTURE13; */
/* extern unsigned int GL_TEXTURE14; */
/* extern unsigned int GL_TEXTURE15; */
/* extern unsigned int GL_TEXTURE16; */
/* extern unsigned int GL_TEXTURE17; */
/* extern unsigned int GL_TEXTURE18; */
/* extern unsigned int GL_TEXTURE19; */
/* extern unsigned int GL_TEXTURE20; */
/* extern unsigned int GL_TEXTURE21; */
/* extern unsigned int GL_TEXTURE22; */
/* extern unsigned int GL_TEXTURE23; */
/* extern unsigned int GL_TEXTURE24; */
/* extern unsigned int GL_TEXTURE25; */
/* extern unsigned int GL_TEXTURE26; */
/* extern unsigned int GL_TEXTURE27; */
/* extern unsigned int GL_TEXTURE28; */
/* extern unsigned int GL_TEXTURE29; */
/* extern unsigned int GL_TEXTURE30; */
/* extern unsigned int GL_TEXTURE31; */
/* extern unsigned int GL_ACTIVE_TEXTURE; */
/* extern unsigned int GL_CLIENT_ACTIVE_TEXTURE; */
/* extern unsigned int GL_MAX_TEXTURE_UNITS; */
/* extern unsigned int GL_NORMAL_MAP; */
/* extern unsigned int GL_REFLECTION_MAP; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP; */
/* extern unsigned int GL_TEXTURE_BINDING_CUBE_MAP; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_POSITIVE_X; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_NEGATIVE_X; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_POSITIVE_Y; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_NEGATIVE_Y; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_POSITIVE_Z; */
/* extern unsigned int GL_TEXTURE_CUBE_MAP_NEGATIVE_Z; */
/* extern unsigned int GL_PROXY_TEXTURE_CUBE_MAP; */
/* extern unsigned int GL_MAX_CUBE_MAP_TEXTURE_SIZE; */
/* extern unsigned int GL_COMPRESSED_ALPHA; */
/* extern unsigned int GL_COMPRESSED_LUMINANCE; */
/* extern unsigned int GL_COMPRESSED_LUMINANCE_ALPHA; */
/* extern unsigned int GL_COMPRESSED_INTENSITY; */
/* extern unsigned int GL_COMPRESSED_RGB; */
/* extern unsigned int GL_COMPRESSED_RGBA; */
/* extern unsigned int GL_TEXTURE_COMPRESSION_HINT; */
/* extern unsigned int GL_TEXTURE_COMPRESSED_IMAGE_SIZE; */
/* extern unsigned int GL_TEXTURE_COMPRESSED; */
/* extern unsigned int GL_NUM_COMPRESSED_TEXTURE_FORMATS; */
/* extern unsigned int GL_COMPRESSED_TEXTURE_FORMATS; */
/* extern unsigned int GL_MULTISAMPLE; */
/* extern unsigned int GL_SAMPLE_ALPHA_TO_COVERAGE; */
/* extern unsigned int GL_SAMPLE_ALPHA_TO_ONE; */
/* extern unsigned int GL_SAMPLE_COVERAGE; */
/* extern unsigned int GL_SAMPLE_BUFFERS; */
/* extern unsigned int GL_SAMPLES; */
/* extern unsigned int GL_SAMPLE_COVERAGE_VALUE; */
/* extern unsigned int GL_SAMPLE_COVERAGE_INVERT; */
/* extern unsigned int GL_MULTISAMPLE_BIT; */
/* extern unsigned int GL_TRANSPOSE_MODELVIEW_MATRIX; */
/* extern unsigned int GL_TRANSPOSE_PROJECTION_MATRIX; */
/* extern unsigned int GL_TRANSPOSE_TEXTURE_MATRIX; */
/* extern unsigned int GL_TRANSPOSE_COLOR_MATRIX; */
/* extern unsigned int GL_COMBINE; */
/* extern unsigned int GL_COMBINE_RGB; */
/* extern unsigned int GL_COMBINE_ALPHA; */
/* extern unsigned int GL_SOURCE0_RGB; */
/* extern unsigned int GL_SOURCE1_RGB; */
/* extern unsigned int GL_SOURCE2_RGB; */
/* extern unsigned int GL_SOURCE0_ALPHA; */
/* extern unsigned int GL_SOURCE1_ALPHA; */
/* extern unsigned int GL_SOURCE2_ALPHA; */
/* extern unsigned int GL_OPERAND0_RGB; */
/* extern unsigned int GL_OPERAND1_RGB; */
/* extern unsigned int GL_OPERAND2_RGB; */
/* extern unsigned int GL_OPERAND0_ALPHA; */
/* extern unsigned int GL_OPERAND1_ALPHA; */
/* extern unsigned int GL_OPERAND2_ALPHA; */
/* extern unsigned int GL_RGB_SCALE; */
/* extern unsigned int GL_ADD_SIGNED; */
/* extern unsigned int GL_INTERPOLATE; */
/* extern unsigned int GL_SUBTRACT; */
/* extern unsigned int GL_CONSTANT; */
/* extern unsigned int GL_PRIMARY_COLOR; */
/* extern unsigned int GL_PREVIOUS; */
/* extern unsigned int GL_DOT3_RGB; */
/* extern unsigned int GL_DOT3_RGBA; */
/* extern unsigned int GL_CLAMP_TO_BORDER; */
/* extern unsigned int GL_GENERATE_MIPMAP; */
/* extern unsigned int GL_GENERATE_MIPMAP_HINT; */
/* extern unsigned int GL_DEPTH_COMPONENT16; */
/* extern unsigned int GL_DEPTH_COMPONENT24; */
/* extern unsigned int GL_DEPTH_COMPONENT32; */
/* extern unsigned int GL_TEXTURE_DEPTH_SIZE; */
/* extern unsigned int GL_DEPTH_TEXTURE_MODE; */
/* extern unsigned int GL_TEXTURE_COMPARE_MODE; */
/* extern unsigned int GL_TEXTURE_COMPARE_FUNC; */
/* extern unsigned int GL_COMPARE_R_TO_TEXTURE; */
/* extern unsigned int GL_FOG_COORDINATE_SOURCE; */
/* extern unsigned int GL_FOG_COORDINATE; */
/* extern unsigned int GL_FRAGMENT_DEPTH; */
/* extern unsigned int GL_CURRENT_FOG_COORDINATE; */
/* extern unsigned int GL_FOG_COORDINATE_ARRAY_TYPE; */
/* extern unsigned int GL_FOG_COORDINATE_ARRAY_STRIDE; */
/* extern unsigned int GL_FOG_COORDINATE_ARRAY_POINTER; */
/* extern unsigned int GL_FOG_COORDINATE_ARRAY; */
/* extern unsigned int GL_POINT_SIZE_MIN; */
/* extern unsigned int GL_POINT_SIZE_MAX; */
/* extern unsigned int GL_POINT_FADE_THRESHOLD_SIZE; */
/* extern unsigned int GL_POINT_DISTANCE_ATTENUATION; */
/* extern unsigned int GL_COLOR_SUM; */
/* extern unsigned int GL_CURRENT_SECONDARY_COLOR; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY_SIZE; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY_TYPE; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY_STRIDE; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY_POINTER; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY; */
/* extern unsigned int GL_BLEND_DST_RGB; */
/* extern unsigned int GL_BLEND_SRC_RGB; */
/* extern unsigned int GL_BLEND_DST_ALPHA; */
/* extern unsigned int GL_BLEND_SRC_ALPHA; */
/* extern unsigned int GL_INCR_WRAP; */
/* extern unsigned int GL_DECR_WRAP; */
/* extern unsigned int GL_TEXTURE_FILTER_CONTROL; */
/* extern unsigned int GL_TEXTURE_LOD_BIAS; */
/* extern unsigned int GL_MAX_TEXTURE_LOD_BIAS; */
/* extern unsigned int GL_MIRRORED_REPEAT; */
/* extern unsigned int GL_BUFFER_SIZE; */
/* extern unsigned int GL_BUFFER_USAGE; */
/* extern unsigned int GL_QUERY_COUNTER_BITS; */
/* extern unsigned int GL_CURRENT_QUERY; */
/* extern unsigned int GL_QUERY_RESULT; */
/* extern unsigned int GL_QUERY_RESULT_AVAILABLE; */
/* extern unsigned int GL_ARRAY_BUFFER; */
/* extern unsigned int GL_ELEMENT_ARRAY_BUFFER; */
/* extern unsigned int GL_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_ELEMENT_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_VERTEX_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_NORMAL_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_COLOR_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_INDEX_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_EDGE_FLAG_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_WEIGHT_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_READ_ONLY; */
/* extern unsigned int GL_WRITE_ONLY; */
/* extern unsigned int GL_READ_WRITE; */
/* extern unsigned int GL_BUFFER_ACCESS; */
/* extern unsigned int GL_BUFFER_MAPPED; */
/* extern unsigned int GL_BUFFER_MAP_POINTER; */
/* extern unsigned int GL_STREAM_DRAW; */
/* extern unsigned int GL_STREAM_READ; */
/* extern unsigned int GL_STREAM_COPY; */
/* extern unsigned int GL_STATIC_DRAW; */
/* extern unsigned int GL_STATIC_READ; */
/* extern unsigned int GL_STATIC_COPY; */
/* extern unsigned int GL_DYNAMIC_DRAW; */
/* extern unsigned int GL_DYNAMIC_READ; */
/* extern unsigned int GL_DYNAMIC_COPY; */
/* extern unsigned int GL_SAMPLES_PASSED; */
/* extern unsigned int GL_FOG_COORD_SRC; */
/* extern unsigned int GL_FOG_COORD; */
/* extern unsigned int GL_CURRENT_FOG_COORD; */
/* extern unsigned int GL_FOG_COORD_ARRAY_TYPE; */
/* extern unsigned int GL_FOG_COORD_ARRAY_STRIDE; */
/* extern unsigned int GL_FOG_COORD_ARRAY_POINTER; */
/* extern unsigned int GL_FOG_COORD_ARRAY; */
/* extern unsigned int GL_FOG_COORD_ARRAY_BUFFER_BINDING; */
/* extern unsigned int GL_SRC0_RGB; */
/* extern unsigned int GL_SRC1_RGB; */
/* extern unsigned int GL_SRC2_RGB; */
/* extern unsigned int GL_SRC0_ALPHA; */
/* extern unsigned int GL_SRC1_ALPHA; */
/* extern unsigned int GL_SRC2_ALPHA; */
/* extern unsigned int GL_BLEND_EQUATION; */
/* extern unsigned int GL_BLEND_EQUATION_RGB; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_ENABLED; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_SIZE; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_STRIDE; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_TYPE; */
/* extern unsigned int GL_CURRENT_VERTEX_ATTRIB; */
/* extern unsigned int GL_VERTEX_PROGRAM_POINT_SIZE; */
/* extern unsigned int GL_VERTEX_PROGRAM_TWO_SIDE; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_POINTER; */
/* extern unsigned int GL_STENCIL_BACK_FUNC; */
/* extern unsigned int GL_STENCIL_BACK_FAIL; */
/* extern unsigned int GL_STENCIL_BACK_PASS_DEPTH_FAIL; */
/* extern unsigned int GL_STENCIL_BACK_PASS_DEPTH_PASS; */
/* extern unsigned int GL_MAX_DRAW_BUFFERS; */
/* extern unsigned int GL_DRAW_BUFFER0; */
/* extern unsigned int GL_DRAW_BUFFER1; */
/* extern unsigned int GL_DRAW_BUFFER2; */
/* extern unsigned int GL_DRAW_BUFFER3; */
/* extern unsigned int GL_DRAW_BUFFER4; */
/* extern unsigned int GL_DRAW_BUFFER5; */
/* extern unsigned int GL_DRAW_BUFFER6; */
/* extern unsigned int GL_DRAW_BUFFER7; */
/* extern unsigned int GL_DRAW_BUFFER8; */
/* extern unsigned int GL_DRAW_BUFFER9; */
/* extern unsigned int GL_DRAW_BUFFER10; */
/* extern unsigned int GL_DRAW_BUFFER11; */
/* extern unsigned int GL_DRAW_BUFFER12; */
/* extern unsigned int GL_DRAW_BUFFER13; */
/* extern unsigned int GL_DRAW_BUFFER14; */
/* extern unsigned int GL_DRAW_BUFFER15; */
/* extern unsigned int GL_BLEND_EQUATION_ALPHA; */
/* extern unsigned int GL_POINT_SPRITE; */
/* extern unsigned int GL_COORD_REPLACE; */
/* extern unsigned int GL_MAX_VERTEX_ATTRIBS; */
/* extern unsigned int GL_VERTEX_ATTRIB_ARRAY_NORMALIZED; */
/* extern unsigned int GL_MAX_TEXTURE_COORDS; */
/* extern unsigned int GL_MAX_TEXTURE_IMAGE_UNITS; */
/* extern unsigned int GL_FRAGMENT_SHADER; */
/* extern unsigned int GL_VERTEX_SHADER; */
/* extern unsigned int GL_MAX_FRAGMENT_UNIFORM_COMPONENTS; */
/* extern unsigned int GL_MAX_VERTEX_UNIFORM_COMPONENTS; */
/* extern unsigned int GL_MAX_VARYING_FLOATS; */
/* extern unsigned int GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS; */
/* extern unsigned int GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS; */
/* extern unsigned int GL_SHADER_TYPE; */
/* extern unsigned int GL_FLOAT_VEC2; */
/* extern unsigned int GL_FLOAT_VEC3; */
/* extern unsigned int GL_FLOAT_VEC4; */
/* extern unsigned int GL_INT_VEC2; */
/* extern unsigned int GL_INT_VEC3; */
/* extern unsigned int GL_INT_VEC4; */
/* extern unsigned int GL_BOOL; */
/* extern unsigned int GL_BOOL_VEC2; */
/* extern unsigned int GL_BOOL_VEC3; */
/* extern unsigned int GL_BOOL_VEC4; */
/* extern unsigned int GL_FLOAT_MAT2; */
/* extern unsigned int GL_FLOAT_MAT3; */
/* extern unsigned int GL_FLOAT_MAT4; */
/* extern unsigned int GL_SAMPLER_1D; */
/* extern unsigned int GL_SAMPLER_2D; */
/* extern unsigned int GL_SAMPLER_3D; */
/* extern unsigned int GL_SAMPLER_CUBE; */
/* extern unsigned int GL_SAMPLER_1D_SHADOW; */
/* extern unsigned int GL_SAMPLER_2D_SHADOW; */
/* extern unsigned int GL_DELETE_STATUS; */
/* extern unsigned int GL_COMPILE_STATUS; */
/* extern unsigned int GL_LINK_STATUS; */
/* extern unsigned int GL_VALIDATE_STATUS; */
/* extern unsigned int GL_INFO_LOG_LENGTH; */
/* extern unsigned int GL_ATTACHED_SHADERS; */
/* extern unsigned int GL_ACTIVE_UNIFORMS; */
/* extern unsigned int GL_ACTIVE_UNIFORM_MAX_LENGTH; */
/* extern unsigned int GL_SHADER_SOURCE_LENGTH; */
/* extern unsigned int GL_ACTIVE_ATTRIBUTES; */
/* extern unsigned int GL_ACTIVE_ATTRIBUTE_MAX_LENGTH; */
/* extern unsigned int GL_FRAGMENT_SHADER_DERIVATIVE_HINT; */
/* extern unsigned int GL_SHADING_LANGUAGE_VERSION; */
/* extern unsigned int GL_CURRENT_PROGRAM; */
/* extern unsigned int GL_POINT_SPRITE_COORD_ORIGIN; */
/* extern unsigned int GL_LOWER_LEFT; */
/* extern unsigned int GL_UPPER_LEFT; */
/* extern unsigned int GL_STENCIL_BACK_REF; */
/* extern unsigned int GL_STENCIL_BACK_VALUE_MASK; */
/* extern unsigned int GL_STENCIL_BACK_WRITEMASK; */
/* extern unsigned int GL_FRAMEBUFFER_EXT; */
/* extern unsigned int GL_RENDERBUFFER_EXT; */
/* extern unsigned int GL_STENCIL_INDEX1_EXT; */
/* extern unsigned int GL_STENCIL_INDEX4_EXT; */
/* extern unsigned int GL_STENCIL_INDEX8_EXT; */
/* extern unsigned int GL_STENCIL_INDEX16_EXT; */
/* extern unsigned int GL_RENDERBUFFER_WIDTH_EXT; */
/* extern unsigned int GL_RENDERBUFFER_HEIGHT_EXT; */
/* extern unsigned int GL_RENDERBUFFER_INTERNAL_FORMAT_EXT; */
/* extern unsigned int GL_RENDERBUFFER_RED_SIZE_EXT; */
/* extern unsigned int GL_RENDERBUFFER_GREEN_SIZE_EXT; */
/* extern unsigned int GL_RENDERBUFFER_BLUE_SIZE_EXT; */
/* extern unsigned int GL_RENDERBUFFER_ALPHA_SIZE_EXT; */
/* extern unsigned int GL_RENDERBUFFER_DEPTH_SIZE_EXT; */
/* extern unsigned int GL_RENDERBUFFER_STENCIL_SIZE_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT0_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT1_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT2_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT3_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT4_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT5_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT6_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT7_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT8_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT9_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT10_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT11_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT12_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT13_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT14_EXT; */
/* extern unsigned int GL_COLOR_ATTACHMENT15_EXT; */
/* extern unsigned int GL_DEPTH_ATTACHMENT_EXT; */
/* extern unsigned int GL_STENCIL_ATTACHMENT_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_COMPLETE_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_UNSUPPORTED_EXT; */
/* extern unsigned int GL_FRAMEBUFFER_BINDING_EXT; */
/* extern unsigned int GL_RENDERBUFFER_BINDING_EXT; */
/* extern unsigned int GL_MAX_COLOR_ATTACHMENTS_EXT; */
/* extern unsigned int GL_MAX_RENDERBUFFER_SIZE_EXT; */
/* extern unsigned int GL_INVALID_FRAMEBUFFER_OPERATION_EXT; */
/* extern unsigned int GLFW_VERSION_MAJOR; */
/* extern unsigned int GLFW_VERSION_MINOR; */
/* extern unsigned int GLFW_VERSION_REVISION; */
/* extern unsigned int GLFW_RELEASE; */
/* extern unsigned int GLFW_PRESS; */
/* extern unsigned int GLFW_KEY_SPACE; */
/* extern unsigned int GLFW_KEY_SPECIAL; */
/* extern unsigned int GLFW_KEY_ESC; */
/* extern unsigned int GLFW_KEY_F1; */
/* extern unsigned int GLFW_KEY_F2; */
/* extern unsigned int GLFW_KEY_F3; */
/* extern unsigned int GLFW_KEY_F4; */
/* extern unsigned int GLFW_KEY_F5; */
/* extern unsigned int GLFW_KEY_F6; */
/* extern unsigned int GLFW_KEY_F7; */
/* extern unsigned int GLFW_KEY_F8; */
/* extern unsigned int GLFW_KEY_F9; */
/* extern unsigned int GLFW_KEY_F10; */
/* extern unsigned int GLFW_KEY_F11; */
/* extern unsigned int GLFW_KEY_F12; */
/* extern unsigned int GLFW_KEY_F13; */
/* extern unsigned int GLFW_KEY_F14; */
/* extern unsigned int GLFW_KEY_F15; */
/* extern unsigned int GLFW_KEY_F16; */
/* extern unsigned int GLFW_KEY_F17; */
/* extern unsigned int GLFW_KEY_F18; */
/* extern unsigned int GLFW_KEY_F19; */
/* extern unsigned int GLFW_KEY_F20; */
/* extern unsigned int GLFW_KEY_F21; */
/* extern unsigned int GLFW_KEY_F22; */
/* extern unsigned int GLFW_KEY_F23; */
/* extern unsigned int GLFW_KEY_F24; */
/* extern unsigned int GLFW_KEY_F25; */
/* extern unsigned int GLFW_KEY_UP; */
/* extern unsigned int GLFW_KEY_DOWN; */
/* extern unsigned int GLFW_KEY_LEFT; */
/* extern unsigned int GLFW_KEY_RIGHT; */
/* extern unsigned int GLFW_KEY_LSHIFT; */
/* extern unsigned int GLFW_KEY_RSHIFT; */
/* extern unsigned int GLFW_KEY_LCTRL; */
/* extern unsigned int GLFW_KEY_RCTRL; */
/* extern unsigned int GLFW_KEY_LALT; */
/* extern unsigned int GLFW_KEY_RALT; */
/* extern unsigned int GLFW_KEY_TAB; */
/* extern unsigned int GLFW_KEY_ENTER; */
/* extern unsigned int GLFW_KEY_BACKSPACE; */
/* extern unsigned int GLFW_KEY_INSERT; */
/* extern unsigned int GLFW_KEY_DEL; */
/* extern unsigned int GLFW_KEY_PAGEUP; */
/* extern unsigned int GLFW_KEY_PAGEDOWN; */
/* extern unsigned int GLFW_KEY_HOME; */
/* extern unsigned int GLFW_KEY_END; */
/* extern unsigned int GLFW_KEY_KP_0; */
/* extern unsigned int GLFW_KEY_KP_1; */
/* extern unsigned int GLFW_KEY_KP_2; */
/* extern unsigned int GLFW_KEY_KP_3; */
/* extern unsigned int GLFW_KEY_KP_4; */
/* extern unsigned int GLFW_KEY_KP_5; */
/* extern unsigned int GLFW_KEY_KP_6; */
/* extern unsigned int GLFW_KEY_KP_7; */
/* extern unsigned int GLFW_KEY_KP_8; */
/* extern unsigned int GLFW_KEY_KP_9; */
/* extern unsigned int GLFW_KEY_KP_DIVIDE; */
/* extern unsigned int GLFW_KEY_KP_MULTIPLY; */
/* extern unsigned int GLFW_KEY_KP_SUBTRACT; */
/* extern unsigned int GLFW_KEY_KP_ADD; */
/* extern unsigned int GLFW_KEY_KP_DECIMAL; */
/* extern unsigned int GLFW_KEY_KP_EQUAL; */
/* extern unsigned int GLFW_KEY_KP_ENTER; */
/* extern unsigned int GLFW_KEY_LAST; */
/* extern unsigned int GLFW_KEY_A; */
/* extern unsigned int GLFW_KEY_B; */
/* extern unsigned int GLFW_KEY_C; */
/* extern unsigned int GLFW_KEY_D; */
/* extern unsigned int GLFW_KEY_E; */
/* extern unsigned int GLFW_KEY_F; */
/* extern unsigned int GLFW_KEY_G; */
/* extern unsigned int GLFW_KEY_H; */
/* extern unsigned int GLFW_KEY_I; */
/* extern unsigned int GLFW_KEY_J; */
/* extern unsigned int GLFW_KEY_K; */
/* extern unsigned int GLFW_KEY_L; */
/* extern unsigned int GLFW_KEY_M; */
/* extern unsigned int GLFW_KEY_N; */
/* extern unsigned int GLFW_KEY_O; */
/* extern unsigned int GLFW_KEY_P; */
/* extern unsigned int GLFW_KEY_Q; */
/* extern unsigned int GLFW_KEY_R; */
/* extern unsigned int GLFW_KEY_S; */
/* extern unsigned int GLFW_KEY_T; */
/* extern unsigned int GLFW_KEY_U; */
/* extern unsigned int GLFW_KEY_V; */
/* extern unsigned int GLFW_KEY_W; */
/* extern unsigned int GLFW_KEY_X; */
/* extern unsigned int GLFW_KEY_Y; */
/* extern unsigned int GLFW_KEY_Z; */
/* extern unsigned int GLFW_KEY_0; */
/* extern unsigned int GLFW_KEY_1; */
/* extern unsigned int GLFW_KEY_2; */
/* extern unsigned int GLFW_KEY_3; */
/* extern unsigned int GLFW_KEY_4; */
/* extern unsigned int GLFW_KEY_5; */
/* extern unsigned int GLFW_KEY_6; */
/* extern unsigned int GLFW_KEY_7; */
/* extern unsigned int GLFW_KEY_8; */
/* extern unsigned int GLFW_KEY_9; */
/* extern unsigned int GLFW_MOUSE_BUTTON_1; */
/* extern unsigned int GLFW_MOUSE_BUTTON_2; */
/* extern unsigned int GLFW_MOUSE_BUTTON_3; */
/* extern unsigned int GLFW_MOUSE_BUTTON_4; */
/* extern unsigned int GLFW_MOUSE_BUTTON_5; */
/* extern unsigned int GLFW_MOUSE_BUTTON_6; */
/* extern unsigned int GLFW_MOUSE_BUTTON_7; */
/* extern unsigned int GLFW_MOUSE_BUTTON_8; */
/* extern unsigned int GLFW_MOUSE_BUTTON_LAST; */
/* extern unsigned int GLFW_MOUSE_BUTTON_LEFT; */
/* extern unsigned int GLFW_MOUSE_BUTTON_RIGHT; */
/* extern unsigned int GLFW_MOUSE_BUTTON_MIDDLE; */
/* extern unsigned int GLFW_JOYSTICK_1; */
/* extern unsigned int GLFW_JOYSTICK_2; */
/* extern unsigned int GLFW_JOYSTICK_3; */
/* extern unsigned int GLFW_JOYSTICK_4; */
/* extern unsigned int GLFW_JOYSTICK_5; */
/* extern unsigned int GLFW_JOYSTICK_6; */
/* extern unsigned int GLFW_JOYSTICK_7; */
/* extern unsigned int GLFW_JOYSTICK_8; */
/* extern unsigned int GLFW_JOYSTICK_9; */
/* extern unsigned int GLFW_JOYSTICK_10; */
/* extern unsigned int GLFW_JOYSTICK_11; */
/* extern unsigned int GLFW_JOYSTICK_12; */
/* extern unsigned int GLFW_JOYSTICK_13; */
/* extern unsigned int GLFW_JOYSTICK_14; */
/* extern unsigned int GLFW_JOYSTICK_15; */
/* extern unsigned int GLFW_JOYSTICK_16; */
/* extern unsigned int GLFW_JOYSTICK_LAST; */
/* extern unsigned int GLFW_WINDOW; */
/* extern unsigned int GLFW_FULLSCREEN; */
/* extern unsigned int GLFW_OPENED; */
/* extern unsigned int GLFW_ACTIVE; */
/* extern unsigned int GLFW_ICONIFIED; */
/* extern unsigned int GLFW_ACCELERATED; */
/* extern unsigned int GLFW_RED_BITS; */
/* extern unsigned int GLFW_GREEN_BITS; */
/* extern unsigned int GLFW_BLUE_BITS; */
/* extern unsigned int GLFW_ALPHA_BITS; */
/* extern unsigned int GLFW_DEPTH_BITS; */
/* extern unsigned int GLFW_STENCIL_BITS; */
/* extern unsigned int GLFW_WINDOW_NO_RESIZE; */
/* extern unsigned int GLFW_FSAA_SAMPLES; */
/* extern unsigned int GLFW_REFRESH_RATE; */
/* extern unsigned int GLFW_ACCUM_RED_BITS; */
/* extern unsigned int GLFW_ACCUM_GREEN_BITS; */
/* extern unsigned int GLFW_ACCUM_BLUE_BITS; */
/* extern unsigned int GLFW_ACCUM_ALPHA_BITS; */
/* extern unsigned int GLFW_AUX_BUFFERS; */
/* extern unsigned int GLFW_STEREO; */
/* extern unsigned int GLFW_MOUSE_CURSOR; */
/* extern unsigned int GLFW_STICKY_KEYS; */
/* extern unsigned int GLFW_STICKY_MOUSE_BUTTONS; */
/* extern unsigned int GLFW_SYSTEM_KEYS; */
/* extern unsigned int GLFW_KEY_REPEAT; */
/* extern unsigned int GLFW_AUTO_POLL_EVENTS; */
/* extern unsigned int GLFW_WAIT; */
/* extern unsigned int GLFW_NOWAIT; */
/* extern unsigned int GLFW_PRESENT; */
/* extern unsigned int GLFW_AXES; */
/* extern unsigned int GLFW_BUTTONS; */
/* extern unsigned int GLFW_NO_RESCALE_BIT; */
/* extern unsigned int GLFW_ORIGIN_UL_BIT; */
/* extern unsigned int GLFW_BUILD_MIPMAPS_BIT; */
/* extern unsigned int GLFW_ALPHA_MAP_BIT; */
/* static unsigned char temp22587[14]; */
/* extern unsigned char *tempvar_3745; */
/* static unsigned char temp22588[3]; */
/* extern unsigned char *tempvar_3746; */
/* static unsigned char temp22589[15]; */
/* extern unsigned char *tempvar_3747; */
/* static unsigned char temp22590[9]; */
/* extern unsigned char *tempvar_3748; */
/* static unsigned char temp22591[11]; */
/* extern unsigned char *tempvar_3749; */
/* static unsigned char temp22592[9]; */
/* extern unsigned char *tempvar_3750; */
/* static unsigned char temp22593[24]; */
/* extern unsigned char *tempvar_3781; */
/* static unsigned char temp22594[7]; */
/* extern unsigned char *tempvar_3813; */
/* static unsigned char temp22595[9]; */
/* extern unsigned char *tempvar_3822; */
/* extern unsigned int windowWidth; */
/* extern unsigned int windowHeight; */
/* extern bool initialized; */
/* static unsigned char temp22596[12]; */
/* extern unsigned char *tempvar_4461; */
/* static unsigned char temp22597[9]; */
/* extern unsigned char *tempvar_4462; */
/* static unsigned char temp22598[12]; */
/* extern unsigned char *tempvar_4504; */
/* static unsigned char temp22599[15]; */
/* extern unsigned char *tempvar_4505; */
/* static unsigned char temp22600[12]; */
/* extern unsigned char *tempvar_4506; */
/* static unsigned char temp22601[13]; */
/* extern unsigned char *tempvar_4507; */
/* static unsigned char temp22602[2]; */
/* extern unsigned char *tempvar_4508; */
/* static unsigned char temp22603[2]; */
/* extern unsigned char *tempvar_4509; */
/* static unsigned char temp22604[2]; */
/* extern unsigned char *tempvar_4510; */
/* static unsigned char temp22605[2]; */
/* extern unsigned char *tempvar_4511; */
/* static unsigned char temp22606[3]; */
/* extern unsigned char *tempvar_4512; */
/* static unsigned char temp22607[2]; */
/* extern unsigned char *tempvar_4513; */
/* static unsigned char temp22608[12]; */
/* extern unsigned char *tempvar_4514; */
/* static unsigned char temp22609[114]; */
/* extern unsigned char *tempvar_4515; */
/* static unsigned char temp22610[82]; */
/* extern unsigned char *tempvar_4516; */
/* extern double autoTimeout; */
/* static unsigned char temp22611[110]; */
/* extern unsigned char *tempvar_4579; */
/* static unsigned char temp22612[375]; */
/* extern unsigned char *tempvar_4580; */
/* static unsigned char temp22613[9]; */
/* extern unsigned char *tempvar_4908; */


/* Global Variable Definitions and Initialization */
static unsigned char _2E_str[3] = "%s";
static unsigned char _2E_str1[3] = "%d";
static unsigned char _2E_str2[3] = "%f";
static unsigned char _2E_str3[14] = "hello, stdlib";
static unsigned char temp113[4] = "seq";
unsigned char *tempvar_1 = (&(temp113[((unsigned int )0)]));
static unsigned char temp119[3] = " [";
unsigned char *tempvar_2 = (&(temp119[((unsigned int )0)]));
static unsigned char temp120[9] = " childs]";
unsigned char *tempvar_3 = (&(temp120[((unsigned int )0)]));
static unsigned char temp195[7] = "native";
unsigned char *tempvar_4 = (&(temp195[((unsigned int )0)]));
static unsigned char temp196[7] = "nested";
unsigned char *tempvar_5 = (&(temp196[((unsigned int )0)]));
static unsigned char temp197[5] = "tree";
unsigned char *tempvar_6 = (&(temp197[((unsigned int )0)]));
static unsigned char temp198[5] = "with";
unsigned char *tempvar_7 = (&(temp198[((unsigned int )0)]));
static unsigned char temp199[5] = "args";
unsigned char *tempvar_8 = (&(temp199[((unsigned int )0)]));
static unsigned char temp200[2] = "a";
unsigned char *tempvar_9 = (&(temp200[((unsigned int )0)]));
static unsigned char temp201[2] = "b";
unsigned char *tempvar_10 = (&(temp201[((unsigned int )0)]));
static unsigned char temp202[2] = "c";
unsigned char *tempvar_11 = (&(temp202[((unsigned int )0)]));
static unsigned char temp239[4] = "foo";
unsigned char *tempvar_12 = (&(temp239[((unsigned int )0)]));
static unsigned char temp240[4] = "bar";
unsigned char *tempvar_13 = (&(temp240[((unsigned int )0)]));
static unsigned char temp252[6] = "macro";
unsigned char *tempvar_14 = (&(temp252[((unsigned int )0)]));
static unsigned char temp253[5] = "test";
unsigned char *tempvar_15 = (&(temp253[((unsigned int )0)]));
unsigned int lastid;
static unsigned char temp272[6] = "_tmp_";
unsigned char *tempvar_16 = (&(temp272[((unsigned int )0)]));
static unsigned char temp279[2] = "_";
unsigned char *tempvar_17 = (&(temp279[((unsigned int )0)]));
static unsigned char temp2407[17] = "Assertion failed";
unsigned char *tempvar_385 = (&(temp2407[((unsigned int )0)]));
static unsigned char temp2408[10] = "int.uless";
unsigned char *tempvar_386 = (&(temp2408[((unsigned int )0)]));
static unsigned char temp2409[6] = "index";
unsigned char *tempvar_387 = (&(temp2409[((unsigned int )0)]));
static unsigned char temp2410[9] = "getField";
unsigned char *tempvar_388 = (&(temp2410[((unsigned int )0)]));
static unsigned char temp2411[7] = "parent";
unsigned char *tempvar_389 = (&(temp2411[((unsigned int )0)]));
static unsigned char temp2412[11] = "childCount";
unsigned char *tempvar_390 = (&(temp2412[((unsigned int )0)]));
static unsigned char temp3271[17] = "main() returned ";
unsigned char *tempvar_540 = (&(temp3271[((unsigned int )0)]));
static unsigned char temp22587[14] = "OpenGL error ";
unsigned char *tempvar_3745 = (&(temp22587[((unsigned int )0)]));
static unsigned char temp22588[3] = ": ";
unsigned char *tempvar_3746 = (&(temp22588[((unsigned int )0)]));
static unsigned char temp22589[15] = "Glfw version: ";
unsigned char *tempvar_3747 = (&(temp22589[((unsigned int )0)]));
static unsigned char temp22590[9] = "major = ";
unsigned char *tempvar_3748 = (&(temp22590[((unsigned int )0)]));
static unsigned char temp22591[11] = ", minor = ";
unsigned char *tempvar_3749 = (&(temp22591[((unsigned int )0)]));
static unsigned char temp22592[9] = ", rev = ";
unsigned char *tempvar_3750 = (&(temp22592[((unsigned int )0)]));
static unsigned char temp22593[24] = "Error compiling shader ";
unsigned char *tempvar_3781 = (&(temp22593[((unsigned int )0)]));
static unsigned char temp22594[7] = "vertex";
unsigned char *tempvar_3813 = (&(temp22594[((unsigned int )0)]));
static unsigned char temp22595[9] = "fragment";
unsigned char *tempvar_3822 = (&(temp22595[((unsigned int )0)]));
unsigned int windowWidth = ((unsigned int )400);
unsigned int windowHeight = ((unsigned int )300);
bool initialized;
static unsigned char temp22596[12] = "Init failed";
unsigned char *tempvar_4461 = (&(temp22596[((unsigned int )0)]));
static unsigned char temp22597[9] = "glfwInit";
unsigned char *tempvar_4462 = (&(temp22597[((unsigned int )0)]));
static unsigned char temp22598[12] = "Init failed";
unsigned char *tempvar_4504 = (&(temp22598[((unsigned int )0)]));
static unsigned char temp22599[15] = "glfwOpenWindow";
unsigned char *tempvar_4505 = (&(temp22599[((unsigned int )0)]));
static unsigned char temp22600[12] = "windowWidth";
unsigned char *tempvar_4506 = (&(temp22600[((unsigned int )0)]));
static unsigned char temp22601[13] = "windowHeight";
unsigned char *tempvar_4507 = (&(temp22601[((unsigned int )0)]));
static unsigned char temp22602[2] = "8";
unsigned char *tempvar_4508 = (&(temp22602[((unsigned int )0)]));
static unsigned char temp22603[2] = "8";
unsigned char *tempvar_4509 = (&(temp22603[((unsigned int )0)]));
static unsigned char temp22604[2] = "8";
unsigned char *tempvar_4510 = (&(temp22604[((unsigned int )0)]));
static unsigned char temp22605[2] = "8";
unsigned char *tempvar_4511 = (&(temp22605[((unsigned int )0)]));
static unsigned char temp22606[3] = "16";
unsigned char *tempvar_4512 = (&(temp22606[((unsigned int )0)]));
static unsigned char temp22607[2] = "0";
unsigned char *tempvar_4513 = (&(temp22607[((unsigned int )0)]));
static unsigned char temp22608[12] = "GLFW_WINDOW";
unsigned char *tempvar_4514 = (&(temp22608[((unsigned int )0)]));
static unsigned char temp22609[114] = "\nvarying vec3 normal;\nvoid main(void) {\n  gl_Position = ftransform();\n  normal = normalize( gl_Position.xyz );\n}\n";
unsigned char *tempvar_4515 = (&(temp22609[((unsigned int )0)]));
static unsigned char temp22610[82] = "\nvarying vec3 normal;\n\nvoid main(void) {\n  gl_FragColor = vec4( normal, 1.0 );\n}\n";
unsigned char *tempvar_4516 = (&(temp22610[((unsigned int )0)]));
double autoTimeout = 0x1.f4p+9;
static unsigned char temp22611[110] = "\nvarying vec2 texCoord;\n\nvoid main(void) {\n  gl_Position = ftransform();\n  texCoord = gl_MultiTexCoord0.xy;\n}";
unsigned char *tempvar_4579 = (&(temp22611[((unsigned int )0)]));
static unsigned char temp22612[375] = "\nuniform vec4 lightPos;\nuniform sampler2D texture;\nvarying vec2 texCoord;\n\nvoid main(void) {\n  vec4 texval = texture2D( texture, texCoord );\n  float alpha = texval.a;\n  vec3 normal = texval.xyz;\n\n  vec3 lightDir = normalize( lightPos.xyz );\n  float intensity = dot( lightDir, normal );\n  intensity += 0.3;\n\n  gl_FragColor = vec4( intensity, intensity, intensity, alpha );\n}\n";
unsigned char *tempvar_4580 = (&(temp22612[((unsigned int )0)]));
static unsigned char temp22613[9] = "lightPos";
unsigned char *tempvar_4908 = (&(temp22613[((unsigned int )0)]));


/* Function Bodies */
static inline int llvm_fcmp_ord(double X, double Y) { return X == X && Y == Y; }
static inline int llvm_fcmp_uno(double X, double Y) { return X != X || Y != Y; }
static inline int llvm_fcmp_ueq(double X, double Y) { return X == Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_une(double X, double Y) { return X != Y; }
static inline int llvm_fcmp_ult(double X, double Y) { return X <  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ugt(double X, double Y) { return X >  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ule(double X, double Y) { return X <= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_uge(double X, double Y) { return X >= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_oeq(double X, double Y) { return X == Y ; }
static inline int llvm_fcmp_one(double X, double Y) { return X != Y && llvm_fcmp_ord(X, Y); }
static inline int llvm_fcmp_olt(double X, double Y) { return X <  Y ; }
static inline int llvm_fcmp_ogt(double X, double Y) { return X >  Y ; }
static inline int llvm_fcmp_ole(double X, double Y) { return X <= Y ; }
static inline int llvm_fcmp_oge(double X, double Y) { return X >= Y ; }

void printString(unsigned char *llvm_cbe_str) {
  unsigned char *llvm_cbe_str_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  unsigned char *llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_str_addr) = llvm_cbe_str;
  llvm_cbe_tmp1 = *(&llvm_cbe_str_addr);
  llvm_cbe_tmp2 = printf((char*)(&_2E_str[((unsigned int )0)]), llvm_cbe_tmp1);
  return;
}


void printInt(unsigned int llvm_cbe_i) {
  unsigned int llvm_cbe_i_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_i_addr) = llvm_cbe_i;
  llvm_cbe_tmp1 = *(&llvm_cbe_i_addr);
  llvm_cbe_tmp2 = printf((char*)(&_2E_str1[((unsigned int )0)]), llvm_cbe_tmp1);
  return;
}


void printFloat(float llvm_cbe_f) {
  float llvm_cbe_f_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  float llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp3;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_f_addr) = llvm_cbe_f;
  llvm_cbe_tmp = *(&llvm_cbe_f_addr);
  llvm_cbe_tmp3 = printf((char*)(&_2E_str2[((unsigned int )0)]), (((double )llvm_cbe_tmp)));
  return;
}


void printDouble(double llvm_cbe_d) {
  double llvm_cbe_d_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  double llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_d_addr) = llvm_cbe_d;
  llvm_cbe_tmp1 = *(&llvm_cbe_d_addr);
  llvm_cbe_tmp2 = printf((char*)(&_2E_str2[((unsigned int )0)]), llvm_cbe_tmp1);
  return;
}


void printChar(signed char llvm_cbe_c) {
  unsigned char llvm_cbe_c_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  unsigned char llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_c_addr) = llvm_cbe_c;
  llvm_cbe_tmp = *(&llvm_cbe_c_addr);
  llvm_cbe_tmp2 = putchar((((signed int )(signed char )llvm_cbe_tmp)));
  return;
}


void printNewline(void) {
  unsigned int llvm_cbe_tmp;

  llvm_cbe_tmp = putchar(((unsigned int )10));
  return;
}


unsigned int *nullptr(void) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  unsigned int *llvm_cbe_tmp1;
  unsigned int *llvm_cbe_retval2;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_tmp) = ((unsigned int *)/*NULL*/0);
  llvm_cbe_tmp1 = *(&llvm_cbe_tmp);
  *(&llvm_cbe_retval) = llvm_cbe_tmp1;
  llvm_cbe_retval2 = *(&llvm_cbe_retval);
  return llvm_cbe_retval2;
}


void stdlibHello(void) {
  unsigned int llvm_cbe_tmp1;

  llvm_cbe_tmp1 = puts((char*)(&_2E_str3[((unsigned int )0)]));
  return;
}


unsigned char *int2cstring(unsigned int llvm_cbe_i) {
  unsigned int llvm_cbe_i_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  unsigned char *llvm_cbe_tmp;    /* Address-exposed local */
  unsigned char llvm_cbe_buffer[1000];    /* Address-exposed local */
  unsigned int llvm_cbe_charCount;    /* Address-exposed local */
  unsigned char *llvm_cbe_result;    /* Address-exposed local */
  unsigned int llvm_cbe_alloca_point;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp6;
  unsigned int llvm_cbe_tmp7;
  unsigned char *llvm_cbe_tmp9;
  unsigned char *llvm_cbe_tmp10;
  unsigned char *llvm_cbe_tmp12;
  unsigned char *llvm_cbe_tmp13;
  unsigned char *llvm_cbe_tmp14;
  unsigned char *llvm_cbe_retval15;

  llvm_cbe_alloca_point = ((unsigned int )((unsigned int )0));
  *(&llvm_cbe_i_addr) = llvm_cbe_i;
  llvm_cbe_tmp3 = *(&llvm_cbe_i_addr);
  llvm_cbe_tmp4 = sprintf((((char *)(&llvm_cbe_buffer[0]))), ((const char*)&_2E_str1[((unsigned int )0)]), llvm_cbe_tmp3);
  llvm_cbe_tmp6 = strlen((((char *)(&llvm_cbe_buffer))));
  *(&llvm_cbe_charCount) = llvm_cbe_tmp6;
  llvm_cbe_tmp7 = *(&llvm_cbe_charCount);
  llvm_cbe_tmp9 = (unsigned char*) malloc((llvm_cbe_tmp7 + ((unsigned int )1)));
  *(&llvm_cbe_result) = llvm_cbe_tmp9;
  llvm_cbe_tmp10 = *(&llvm_cbe_result);
  llvm_cbe_tmp12 = (unsigned char*)strcpy((char*)llvm_cbe_tmp10, (((const char *)(&llvm_cbe_buffer))));
  llvm_cbe_tmp13 = *(&llvm_cbe_result);
  *(&llvm_cbe_tmp) = llvm_cbe_tmp13;
  llvm_cbe_tmp14 = *(&llvm_cbe_tmp);
  *(&llvm_cbe_retval) = llvm_cbe_tmp14;
  llvm_cbe_retval15 = *(&llvm_cbe_retval);
  return llvm_cbe_retval15;
}


bool bool_2E_not(bool llvm_cbe_b) {
  return (1 ^ llvm_cbe_b);
}


void printBool(bool llvm_cbe_b) {
  if (llvm_cbe_b) {
    goto llvm_cbe_true;
  } else {
    goto llvm_cbe_false;
  }

llvm_cbe_true:
  printChar(((unsigned char )116));
  printNewline();
  return;
llvm_cbe_false:
  printChar(((unsigned char )102));
  printNewline();
  return;
}


void println(void) {
  printNewline();
  return;
}


void printlnInt(unsigned int llvm_cbe_v) {
  printInt(llvm_cbe_v);
  printNewline();
  return;
}


void printlnString(unsigned char *llvm_cbe_v) {
  printString(llvm_cbe_v);
  printNewline();
  return;
}


unsigned int length(unsigned char *llvm_cbe_s) {
  unsigned char *llvm_cbe_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_count;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp11;
  unsigned char llvm_cbe_temp12;
  unsigned char *llvm_cbe_c;
  unsigned char *llvm_cbe_temp14;
  unsigned int llvm_cbe_temp15;
  unsigned char llvm_cbe_temp17;
  bool *llvm_cbe_b;
  bool llvm_cbe_temp19;
  unsigned int llvm_cbe_temp20;

  *(&llvm_cbe_ptr) = llvm_cbe_s;
  *(&llvm_cbe_count) = ((unsigned int )-1);
  goto llvm_cbe_start;

  do {     /* Syntactic loop 'start' to make GCC happy */
llvm_cbe_start:
  llvm_cbe_temp11 = *(&llvm_cbe_ptr);
  llvm_cbe_temp12 = *llvm_cbe_temp11;
  llvm_cbe_c = (unsigned char *) alloca(sizeof(unsigned char ));
  *llvm_cbe_c = llvm_cbe_temp12;
  llvm_cbe_temp14 = *(&llvm_cbe_ptr);
  *(&llvm_cbe_ptr) = (&llvm_cbe_temp14[((unsigned int )1)]);
  llvm_cbe_temp15 = *(&llvm_cbe_count);
  *(&llvm_cbe_count) = (llvm_cbe_temp15 + ((unsigned int )1));
  llvm_cbe_temp17 = *llvm_cbe_c;
  llvm_cbe_b = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_b = (((llvm_cbe_temp17 == ((unsigned char )0))) & 1);
  llvm_cbe_temp19 = *llvm_cbe_b;
  if (llvm_cbe_temp19) {
    goto llvm_cbe_end;
  } else {
    goto llvm_cbe_start;
  }

  } while (1); /* end of syntactic loop 'start' */
llvm_cbe_end:
  llvm_cbe_temp20 = *(&llvm_cbe_count);
  return llvm_cbe_temp20;
}


struct l_string *makestring(unsigned char *llvm_cbe_cstr) {
  unsigned char *ltmp_0_1;
  struct l_string *llvm_cbe_pstr;    /* Address-exposed local */
  struct l_string *llvm_cbe_temp23;
  unsigned int llvm_cbe_temp24;
  struct l_string *llvm_cbe_temp26;
  struct l_string *llvm_cbe_temp27;

/*   ltmp_0_1 =  /\*tail*\/ ((unsigned char * (*) ())(void*)malloc)(((unsigned int )8)); */
  ltmp_0_1 = (unsigned char*) malloc(8);
  *(&llvm_cbe_pstr) = (((struct l_string *)ltmp_0_1));
  llvm_cbe_temp23 = *(&llvm_cbe_pstr);
  llvm_cbe_temp24 = length(llvm_cbe_cstr);
  *(&llvm_cbe_temp23->field0) = llvm_cbe_temp24;
  llvm_cbe_temp26 = *(&llvm_cbe_pstr);
  *(&llvm_cbe_temp26->field1) = llvm_cbe_cstr;
  llvm_cbe_temp27 = *(&llvm_cbe_pstr);
  return llvm_cbe_temp27;
}


struct l_string *newString(unsigned int llvm_cbe_length, signed char llvm_cbe_init) {
  unsigned char *ltmp_1_1;
  unsigned char *llvm_cbe_chars;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp31;
  unsigned int llvm_cbe_index;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp33;
  unsigned int llvm_cbe_temp34;
  unsigned int llvm_cbe_temp35;
  unsigned int llvm_cbe_temp37;
  bool *llvm_cbe_reachedEnd;
  bool llvm_cbe_temp39;
  unsigned char *ltmp_2_1;
  struct l_string **llvm_cbe_result;
  struct l_string *llvm_cbe_temp42;
  struct l_string *llvm_cbe_temp44;
  unsigned char *llvm_cbe_temp45;
  struct l_string *llvm_cbe_temp46;

  ltmp_1_1 =  /*tail*/ (unsigned char*) malloc(1 + llvm_cbe_length);
  *(&llvm_cbe_chars) = (((unsigned char *)ltmp_1_1));
  llvm_cbe_temp31 = *(&llvm_cbe_chars);
  *(&llvm_cbe_temp31[llvm_cbe_length]) = ((unsigned char )0);
  *(&llvm_cbe_index) = ((unsigned int )0);
  goto llvm_cbe_start;

  do {     /* Syntactic loop 'start' to make GCC happy */
llvm_cbe_start:
  llvm_cbe_temp33 = *(&llvm_cbe_chars);
  llvm_cbe_temp34 = *(&llvm_cbe_index);
  *(&llvm_cbe_temp33[llvm_cbe_temp34]) = llvm_cbe_init;
  llvm_cbe_temp35 = *(&llvm_cbe_index);
  *(&llvm_cbe_index) = (llvm_cbe_temp35 + ((unsigned int )1));
  llvm_cbe_temp37 = *(&llvm_cbe_index);
  llvm_cbe_reachedEnd = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_reachedEnd = (((((unsigned int )llvm_cbe_temp37) >= ((unsigned int )llvm_cbe_length))) & 1);
  llvm_cbe_temp39 = *llvm_cbe_reachedEnd;
  if (llvm_cbe_temp39) {
    goto llvm_cbe_end;
  } else {
    goto llvm_cbe_start;
  }

  } while (1); /* end of syntactic loop 'start' */
llvm_cbe_end:
  ltmp_2_1 =  /*tail*/ (unsigned char*) malloc(((unsigned int )8));
  llvm_cbe_result = (struct l_string **) alloca(sizeof(struct l_string *));
  *llvm_cbe_result = (((struct l_string *)ltmp_2_1));
  llvm_cbe_temp42 = *llvm_cbe_result;
  *(&llvm_cbe_temp42->field0) = llvm_cbe_length;
  llvm_cbe_temp44 = *llvm_cbe_result;
  llvm_cbe_temp45 = *(&llvm_cbe_chars);
  *(&llvm_cbe_temp44->field1) = llvm_cbe_temp45;
  llvm_cbe_temp46 = *llvm_cbe_result;
  return llvm_cbe_temp46;
}


struct l_string *append(struct l_string *llvm_cbe_l, struct l_string *llvm_cbe_r) {
  unsigned int llvm_cbe_temp48;
  unsigned int llvm_cbe_temp50;
  unsigned int llvm_cbe_length;    /* Address-exposed local */
  unsigned int llvm_cbe_temp52;
  struct l_string *llvm_cbe_temp53;
  struct l_string *llvm_cbe_result;    /* Address-exposed local */
  struct l_string *llvm_cbe_temp55;
  unsigned char *llvm_cbe_temp56;
  unsigned char *llvm_cbe_chars;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp58;
  unsigned char *llvm_cbe_leftChars;    /* Address-exposed local */
  unsigned int llvm_cbe_temp60;
  unsigned int llvm_cbe_leftLength;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp62;
  unsigned char *llvm_cbe_rightChars;    /* Address-exposed local */
  unsigned int llvm_cbe_temp64;
  unsigned int llvm_cbe_rightLength;    /* Address-exposed local */
  unsigned int llvm_cbe_index;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp66;
  unsigned int llvm_cbe_temp67;
  unsigned char *llvm_cbe_temp69;
  unsigned int llvm_cbe_temp70;
  unsigned char llvm_cbe_temp71;
  unsigned int llvm_cbe_temp72;
  unsigned int llvm_cbe_temp74;
  unsigned int llvm_cbe_temp75;
  bool *llvm_cbe_test;
  bool llvm_cbe_temp77;
  unsigned char *llvm_cbe_temp79;
  unsigned int llvm_cbe_temp80;
  unsigned int llvm_cbe_temp81;
  unsigned char *llvm_cbe_temp84;
  unsigned int llvm_cbe_temp85;
  unsigned char llvm_cbe_temp86;
  unsigned int llvm_cbe_temp87;
  unsigned int llvm_cbe_temp89;
  unsigned int llvm_cbe_temp90;
  bool *llvm_cbe_test2;
  bool llvm_cbe_temp92;
  struct l_string *llvm_cbe_temp93;

  llvm_cbe_temp48 = *(&llvm_cbe_l->field0);
  llvm_cbe_temp50 = *(&llvm_cbe_r->field0);
  *(&llvm_cbe_length) = (llvm_cbe_temp48 + llvm_cbe_temp50);
  llvm_cbe_temp52 = *(&llvm_cbe_length);
  llvm_cbe_temp53 = newString(llvm_cbe_temp52, ((unsigned char )120));
  *(&llvm_cbe_result) = llvm_cbe_temp53;
  llvm_cbe_temp55 = *(&llvm_cbe_result);
  llvm_cbe_temp56 = *(&llvm_cbe_temp55->field1);
  *(&llvm_cbe_chars) = llvm_cbe_temp56;
  llvm_cbe_temp58 = *(&llvm_cbe_l->field1);
  *(&llvm_cbe_leftChars) = llvm_cbe_temp58;
  llvm_cbe_temp60 = *(&llvm_cbe_l->field0);
  *(&llvm_cbe_leftLength) = llvm_cbe_temp60;
  llvm_cbe_temp62 = *(&llvm_cbe_r->field1);
  *(&llvm_cbe_rightChars) = llvm_cbe_temp62;
  llvm_cbe_temp64 = *(&llvm_cbe_r->field0);
  *(&llvm_cbe_rightLength) = llvm_cbe_temp64;
  *(&llvm_cbe_index) = ((unsigned int )0);
  goto llvm_cbe_lbegin;

  do {     /* Syntactic loop 'lbegin' to make GCC happy */
llvm_cbe_lbegin:
  llvm_cbe_temp66 = *(&llvm_cbe_chars);
  llvm_cbe_temp67 = *(&llvm_cbe_index);
  llvm_cbe_temp69 = *(&llvm_cbe_leftChars);
  llvm_cbe_temp70 = *(&llvm_cbe_index);
  llvm_cbe_temp71 = *(&llvm_cbe_temp69[llvm_cbe_temp70]);
  *(&llvm_cbe_temp66[llvm_cbe_temp67]) = llvm_cbe_temp71;
  llvm_cbe_temp72 = *(&llvm_cbe_index);
  *(&llvm_cbe_index) = (llvm_cbe_temp72 + ((unsigned int )1));
  llvm_cbe_temp74 = *(&llvm_cbe_index);
  llvm_cbe_temp75 = *(&llvm_cbe_leftLength);
  llvm_cbe_test = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_test = (((((unsigned int )llvm_cbe_temp74) < ((unsigned int )llvm_cbe_temp75))) & 1);
  llvm_cbe_temp77 = *llvm_cbe_test;
  if (llvm_cbe_temp77) {
    goto llvm_cbe_lbegin;
  } else {
    goto llvm_cbe_lend;
  }

  } while (1); /* end of syntactic loop 'lbegin' */
llvm_cbe_lend:
  *(&llvm_cbe_index) = ((unsigned int )0);
  goto llvm_cbe_rbegin;

  do {     /* Syntactic loop 'rbegin' to make GCC happy */
llvm_cbe_rbegin:
  llvm_cbe_temp79 = *(&llvm_cbe_chars);
  llvm_cbe_temp80 = *(&llvm_cbe_leftLength);
  llvm_cbe_temp81 = *(&llvm_cbe_index);
  llvm_cbe_temp84 = *(&llvm_cbe_rightChars);
  llvm_cbe_temp85 = *(&llvm_cbe_index);
  llvm_cbe_temp86 = *(&llvm_cbe_temp84[llvm_cbe_temp85]);
  *(&llvm_cbe_temp79[(llvm_cbe_temp80 + llvm_cbe_temp81)]) = llvm_cbe_temp86;
  llvm_cbe_temp87 = *(&llvm_cbe_index);
  *(&llvm_cbe_index) = (llvm_cbe_temp87 + ((unsigned int )1));
  llvm_cbe_temp89 = *(&llvm_cbe_index);
  llvm_cbe_temp90 = *(&llvm_cbe_rightLength);
  llvm_cbe_test2 = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_test2 = (((((unsigned int )llvm_cbe_temp89) < ((unsigned int )llvm_cbe_temp90))) & 1);
  llvm_cbe_temp92 = *llvm_cbe_test2;
  if (llvm_cbe_temp92) {
    goto llvm_cbe_rbegin;
  } else {
    goto llvm_cbe_rend;
  }

  } while (1); /* end of syntactic loop 'rbegin' */
llvm_cbe_rend:
  llvm_cbe_temp93 = *(&llvm_cbe_result);
  return llvm_cbe_temp93;
}


void printIndent(unsigned int llvm_cbe_count) {
  unsigned int llvm_cbe_counter;    /* Address-exposed local */
  unsigned int llvm_cbe_temp94;
  bool *llvm_cbe_t;
  bool llvm_cbe_temp96;
  unsigned int llvm_cbe_temp99;

  *(&llvm_cbe_counter) = llvm_cbe_count;
  goto llvm_cbe_start;

  do {     /* Syntactic loop 'start' to make GCC happy */
llvm_cbe_start:
  llvm_cbe_temp94 = *(&llvm_cbe_counter);
  llvm_cbe_t = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_t = (((((unsigned int )llvm_cbe_temp94) > ((unsigned int )((unsigned int )0)))) & 1);
  llvm_cbe_temp96 = *llvm_cbe_t;
  if (llvm_cbe_temp96) {
    goto llvm_cbe_goon;
  } else {
    goto llvm_cbe_end;
  }

llvm_cbe_goon:
  printChar(((unsigned char )32));
  printChar(((unsigned char )32));
  llvm_cbe_temp99 = *(&llvm_cbe_counter);
  *(&llvm_cbe_counter) = (llvm_cbe_temp99 - ((unsigned int )1));
  goto llvm_cbe_start;

  } while (1); /* end of syntactic loop 'start' */
llvm_cbe_end:
  return;
}


bool cstring_2E_equal(unsigned char *llvm_cbe_l, unsigned char *llvm_cbe_r) {
  unsigned int llvm_cbe_temp101;

  llvm_cbe_temp101 = strcmp((char*)llvm_cbe_l, (const char*)llvm_cbe_r);
  return (((unsigned int )0) == llvm_cbe_temp101);
}


struct l_ast *simpleAst(unsigned char *llvm_cbe_name) {
  unsigned char *ltmp_3_1;
  struct l_ast *llvm_cbe_a;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp105;
  struct l_ast *llvm_cbe_temp107;
  struct l_ast *llvm_cbe_temp109;
  struct l_ast *llvm_cbe_temp111;

  ltmp_3_1 =  /*tail*/ (unsigned char*) malloc(((unsigned int )12));
  *(&llvm_cbe_a) = (((struct l_ast *)ltmp_3_1));
  llvm_cbe_temp105 = *(&llvm_cbe_a);
  *(&llvm_cbe_temp105->field0) = llvm_cbe_name;
  llvm_cbe_temp107 = *(&llvm_cbe_a);
  *(&llvm_cbe_temp107->field1) = ((unsigned int )0);
  llvm_cbe_temp109 = *(&llvm_cbe_a);
  *(&llvm_cbe_temp109->field2) = (((struct l_ast **)((unsigned char *)/*NULL*/0)));
  llvm_cbe_temp111 = *(&llvm_cbe_a);
  return llvm_cbe_temp111;
}


struct l_ast *astFromString(unsigned char *llvm_cbe_name) {
  struct l_ast *llvm_cbe_temp112;

  llvm_cbe_temp112 = simpleAst(llvm_cbe_name);
  return llvm_cbe_temp112;
}


struct l_ast *seqAst(void) {
  unsigned char *llvm_cbe_temp114;
  struct l_ast *llvm_cbe_temp115;

  llvm_cbe_temp114 = *(&tempvar_1);
  llvm_cbe_temp115 = astFromString(llvm_cbe_temp114);
  return llvm_cbe_temp115;
}


struct l_ast *astFromInt(unsigned int llvm_cbe_i) {
  unsigned char *llvm_cbe_temp116;
  unsigned char *llvm_cbe_asString;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp117;
  struct l_ast *llvm_cbe_temp118;

  llvm_cbe_temp116 = int2cstring(llvm_cbe_i);
  *(&llvm_cbe_asString) = llvm_cbe_temp116;
  llvm_cbe_temp117 = *(&llvm_cbe_asString);
  llvm_cbe_temp118 = simpleAst(llvm_cbe_temp117);
  return llvm_cbe_temp118;
}


void printAst(struct l_ast *llvm_cbe_a, unsigned int llvm_cbe_indent) {
  unsigned char *llvm_cbe_temp123;
  unsigned char *llvm_cbe_temp125;
  unsigned int llvm_cbe_temp128;
  unsigned char *llvm_cbe_temp130;
  unsigned int llvm_cbe_index;    /* Address-exposed local */
  unsigned int llvm_cbe_temp133;
  unsigned int llvm_cbe_temp135;
  bool *llvm_cbe_t;
  bool llvm_cbe_temp137;
  struct l_ast **llvm_cbe_temp140;
  unsigned int llvm_cbe_temp141;
  struct l_ast *llvm_cbe_temp142;
  struct l_ast **llvm_cbe_child;
  struct l_ast *llvm_cbe_temp143;
  unsigned int llvm_cbe_temp146;

  printIndent(llvm_cbe_indent);
  llvm_cbe_temp123 = *(&llvm_cbe_a->field0);
  printString(llvm_cbe_temp123);
  llvm_cbe_temp125 = *(&tempvar_2);
  printString(llvm_cbe_temp125);
  llvm_cbe_temp128 = *(&llvm_cbe_a->field1);
  printInt(llvm_cbe_temp128);
  llvm_cbe_temp130 = *(&tempvar_3);
  printString(llvm_cbe_temp130);
  println();
  *(&llvm_cbe_index) = ((unsigned int )0);
  goto llvm_cbe_start;

  do {     /* Syntactic loop 'start' to make GCC happy */
llvm_cbe_start:
  llvm_cbe_temp133 = *(&llvm_cbe_index);
  llvm_cbe_temp135 = *(&llvm_cbe_a->field1);
  llvm_cbe_t = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_t = (((((unsigned int )llvm_cbe_temp133) < ((unsigned int )llvm_cbe_temp135))) & 1);
  llvm_cbe_temp137 = *llvm_cbe_t;
  if (llvm_cbe_temp137) {
    goto llvm_cbe_goon;
  } else {
    goto llvm_cbe_end;
  }

llvm_cbe_goon:
  llvm_cbe_temp140 = *(&llvm_cbe_a->field2);
  llvm_cbe_temp141 = *(&llvm_cbe_index);
  llvm_cbe_temp142 = *(&llvm_cbe_temp140[llvm_cbe_temp141]);
  llvm_cbe_child = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe_child = llvm_cbe_temp142;
  llvm_cbe_temp143 = *llvm_cbe_child;
  printAst(llvm_cbe_temp143, (llvm_cbe_indent + ((unsigned int )1)));
  llvm_cbe_temp146 = *(&llvm_cbe_index);
  *(&llvm_cbe_index) = (llvm_cbe_temp146 + ((unsigned int )1));
  goto llvm_cbe_start;

  } while (1); /* end of syntactic loop 'start' */
llvm_cbe_end:
  return;
}


void addChild(struct l_ast *llvm_cbe_tree, struct l_ast *llvm_cbe_child) {
  unsigned int llvm_cbe_temp149;
  unsigned int llvm_cbe_oldChildCount;    /* Address-exposed local */
  unsigned int llvm_cbe_temp150;
  unsigned int llvm_cbe_newChildCount;    /* Address-exposed local */
  unsigned int llvm_cbe_temp152;
  unsigned char *ltmp_4_1;
  struct l_ast **llvm_cbe_newChilds;    /* Address-exposed local */
  unsigned int llvm_cbe_index;    /* Address-exposed local */
  unsigned int llvm_cbe_temp154;
  unsigned int llvm_cbe_temp155;
  bool *llvm_cbe_t;
  bool llvm_cbe_temp157;
  struct l_ast **llvm_cbe_temp159;
  unsigned int llvm_cbe_temp160;
  struct l_ast **llvm_cbe_temp163;
  unsigned int llvm_cbe_temp164;
  struct l_ast *llvm_cbe_temp165;
  unsigned int llvm_cbe_temp166;
  unsigned int llvm_cbe_temp170;
  struct l_ast **llvm_cbe_temp172;
  unsigned int llvm_cbe_temp174;

  llvm_cbe_temp149 = *(&llvm_cbe_tree->field1);
  *(&llvm_cbe_oldChildCount) = llvm_cbe_temp149;
  llvm_cbe_temp150 = *(&llvm_cbe_oldChildCount);
  *(&llvm_cbe_newChildCount) = (((unsigned int )1) + llvm_cbe_temp150);
  llvm_cbe_temp152 = *(&llvm_cbe_newChildCount);
  ltmp_4_1 =  /*tail*/ (unsigned char*) malloc((llvm_cbe_temp152 * ((unsigned int )4)));
  *(&llvm_cbe_newChilds) = (((struct l_ast **)ltmp_4_1));
  *(&llvm_cbe_index) = ((unsigned int )0);
  goto llvm_cbe_start;

  do {     /* Syntactic loop 'start' to make GCC happy */
llvm_cbe_start:
  llvm_cbe_temp154 = *(&llvm_cbe_index);
  llvm_cbe_temp155 = *(&llvm_cbe_oldChildCount);
  llvm_cbe_t = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_t = (((((unsigned int )llvm_cbe_temp154) < ((unsigned int )llvm_cbe_temp155))) & 1);
  llvm_cbe_temp157 = *llvm_cbe_t;
  llvm_cbe_temp159 = *(&llvm_cbe_newChilds);
  if (llvm_cbe_temp157) {
    goto llvm_cbe_goon;
  } else {
    goto llvm_cbe_end;
  }

llvm_cbe_goon:
  llvm_cbe_temp160 = *(&llvm_cbe_index);
  llvm_cbe_temp163 = *(&llvm_cbe_tree->field2);
  llvm_cbe_temp164 = *(&llvm_cbe_index);
  llvm_cbe_temp165 = *(&llvm_cbe_temp163[llvm_cbe_temp164]);
  *(&llvm_cbe_temp159[llvm_cbe_temp160]) = llvm_cbe_temp165;
  llvm_cbe_temp166 = *(&llvm_cbe_index);
  *(&llvm_cbe_index) = (llvm_cbe_temp166 + ((unsigned int )1));
  goto llvm_cbe_start;

  } while (1); /* end of syntactic loop 'start' */
llvm_cbe_end:
  llvm_cbe_temp170 = *(&llvm_cbe_oldChildCount);
  *(&llvm_cbe_temp159[llvm_cbe_temp170]) = llvm_cbe_child;
  llvm_cbe_temp172 = *(&llvm_cbe_newChilds);
  *(&llvm_cbe_tree->field2) = llvm_cbe_temp172;
  llvm_cbe_temp174 = *(&llvm_cbe_newChildCount);
  *(&llvm_cbe_tree->field1) = llvm_cbe_temp174;
  return;
}


struct l_ast *astChild(struct l_ast *llvm_cbe_parent, unsigned int llvm_cbe_index) {
  struct l_ast **llvm_cbe_temp177;
  struct l_ast *llvm_cbe_temp178;
  struct l_ast *llvm_cbe_child;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp179;

  llvm_cbe_temp177 = *(&llvm_cbe_parent->field2);
  llvm_cbe_temp178 = *(&llvm_cbe_temp177[llvm_cbe_index]);
  *(&llvm_cbe_child) = llvm_cbe_temp178;
  llvm_cbe_temp179 = *(&llvm_cbe_child);
  return llvm_cbe_temp179;
}


unsigned char *macroAstId(unsigned int llvm_cbe_macroCurrentAst) {
  unsigned char *llvm_cbe_temp182;

  llvm_cbe_temp182 = *(&(((struct l_ast *)(unsigned long)llvm_cbe_macroCurrentAst))->field0);
  return llvm_cbe_temp182;
}


unsigned int macroAstChildCount(unsigned int llvm_cbe_macroCurrentAst) {
  unsigned int llvm_cbe_temp185;

  llvm_cbe_temp185 = *(&(((struct l_ast *)(unsigned long)llvm_cbe_macroCurrentAst))->field1);
  return llvm_cbe_temp185;
}


unsigned int macroAstChild(unsigned int llvm_cbe_treeaddr, unsigned int llvm_cbe_num) {
  struct l_ast *llvm_cbe_tree;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp189;
  struct l_ast **llvm_cbe_temp190;
  struct l_ast *llvm_cbe_temp191;
  struct l_ast *llvm_cbe_child;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp192;
  unsigned int llvm_cbe_i;    /* Address-exposed local */
  unsigned int llvm_cbe_temp194;

  *(&llvm_cbe_tree) = (((struct l_ast *)(unsigned long)llvm_cbe_treeaddr));
  llvm_cbe_temp189 = *(&llvm_cbe_tree);
  llvm_cbe_temp190 = *(&llvm_cbe_temp189->field2);
  llvm_cbe_temp191 = *(&llvm_cbe_temp190[llvm_cbe_num]);
  *(&llvm_cbe_child) = llvm_cbe_temp191;
  llvm_cbe_temp192 = *(&llvm_cbe_child);
  *(&llvm_cbe_i) = (((unsigned int )(unsigned long)llvm_cbe_temp192));
  llvm_cbe_temp194 = *(&llvm_cbe_i);
  return llvm_cbe_temp194;
}


struct l_ast *testMacro2(struct l_ast *llvm_cbe_lastArg) {
  unsigned char *llvm_cbe_temp203;
  struct l_ast *llvm_cbe_temp204;
  struct l_ast *llvm_cbe___temp_1;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp205;
  unsigned char *llvm_cbe_temp206;
  struct l_ast *llvm_cbe_temp207;
  struct l_ast *llvm_cbe___temp_2;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp208;
  unsigned char *llvm_cbe_temp209;
  struct l_ast *llvm_cbe_temp210;
  struct l_ast *llvm_cbe_temp212;
  struct l_ast *llvm_cbe_temp214;
  unsigned char *llvm_cbe_temp215;
  struct l_ast *llvm_cbe_temp216;
  struct l_ast *llvm_cbe___temp_3;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp217;
  unsigned char *llvm_cbe_temp218;
  struct l_ast *llvm_cbe_temp219;
  struct l_ast *llvm_cbe___temp_4;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp220;
  unsigned char *llvm_cbe_temp221;
  struct l_ast *llvm_cbe_temp222;
  struct l_ast *llvm_cbe_temp224;
  unsigned char *llvm_cbe_temp225;
  struct l_ast *llvm_cbe_temp226;
  struct l_ast *llvm_cbe_temp228;
  unsigned char *llvm_cbe_temp229;
  struct l_ast *llvm_cbe_temp230;
  struct l_ast *llvm_cbe_temp232;
  struct l_ast *llvm_cbe_temp234;
  struct l_ast *llvm_cbe_temp236;
  struct l_ast *llvm_cbe_temp238;

  llvm_cbe_temp203 = *(&tempvar_4);
  llvm_cbe_temp204 = simpleAst(llvm_cbe_temp203);
  *(&llvm_cbe___temp_1) = llvm_cbe_temp204;
  llvm_cbe_temp205 = *(&llvm_cbe___temp_1);
  llvm_cbe_temp206 = *(&tempvar_5);
  llvm_cbe_temp207 = simpleAst(llvm_cbe_temp206);
  *(&llvm_cbe___temp_2) = llvm_cbe_temp207;
  llvm_cbe_temp208 = *(&llvm_cbe___temp_2);
  llvm_cbe_temp209 = *(&tempvar_6);
  llvm_cbe_temp210 = simpleAst(llvm_cbe_temp209);
  addChild(llvm_cbe_temp208, llvm_cbe_temp210);
  llvm_cbe_temp212 = *(&llvm_cbe___temp_2);
  addChild(llvm_cbe_temp205, llvm_cbe_temp212);
  llvm_cbe_temp214 = *(&llvm_cbe___temp_1);
  llvm_cbe_temp215 = *(&tempvar_7);
  llvm_cbe_temp216 = simpleAst(llvm_cbe_temp215);
  *(&llvm_cbe___temp_3) = llvm_cbe_temp216;
  llvm_cbe_temp217 = *(&llvm_cbe___temp_3);
  llvm_cbe_temp218 = *(&tempvar_8);
  llvm_cbe_temp219 = simpleAst(llvm_cbe_temp218);
  *(&llvm_cbe___temp_4) = llvm_cbe_temp219;
  llvm_cbe_temp220 = *(&llvm_cbe___temp_4);
  llvm_cbe_temp221 = *(&tempvar_9);
  llvm_cbe_temp222 = simpleAst(llvm_cbe_temp221);
  addChild(llvm_cbe_temp220, llvm_cbe_temp222);
  llvm_cbe_temp224 = *(&llvm_cbe___temp_4);
  llvm_cbe_temp225 = *(&tempvar_10);
  llvm_cbe_temp226 = simpleAst(llvm_cbe_temp225);
  addChild(llvm_cbe_temp224, llvm_cbe_temp226);
  llvm_cbe_temp228 = *(&llvm_cbe___temp_4);
  llvm_cbe_temp229 = *(&tempvar_11);
  llvm_cbe_temp230 = simpleAst(llvm_cbe_temp229);
  addChild(llvm_cbe_temp228, llvm_cbe_temp230);
  llvm_cbe_temp232 = *(&llvm_cbe___temp_4);
  addChild(llvm_cbe_temp232, llvm_cbe_lastArg);
  llvm_cbe_temp234 = *(&llvm_cbe___temp_4);
  addChild(llvm_cbe_temp217, llvm_cbe_temp234);
  llvm_cbe_temp236 = *(&llvm_cbe___temp_3);
  addChild(llvm_cbe_temp214, llvm_cbe_temp236);
  llvm_cbe_temp238 = *(&llvm_cbe___temp_1);
  return llvm_cbe_temp238;
}


unsigned int macroExec(void) {
  unsigned char *llvm_cbe_temp241;
  struct l_ast *llvm_cbe_temp242;
  struct l_ast *llvm_cbe___temp_5;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp243;
  unsigned char *llvm_cbe_temp244;
  struct l_ast *llvm_cbe_temp245;
  struct l_ast *llvm_cbe_temp247;
  struct l_ast *llvm_cbe_temp248;
  struct l_ast *llvm_cbe_tree;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp249;
  unsigned int llvm_cbe_i;    /* Address-exposed local */
  unsigned int llvm_cbe_temp251;

  llvm_cbe_temp241 = *(&tempvar_12);
  llvm_cbe_temp242 = simpleAst(llvm_cbe_temp241);
  *(&llvm_cbe___temp_5) = llvm_cbe_temp242;
  llvm_cbe_temp243 = *(&llvm_cbe___temp_5);
  llvm_cbe_temp244 = *(&tempvar_13);
  llvm_cbe_temp245 = simpleAst(llvm_cbe_temp244);
  addChild(llvm_cbe_temp243, llvm_cbe_temp245);
  llvm_cbe_temp247 = *(&llvm_cbe___temp_5);
  llvm_cbe_temp248 = testMacro2(llvm_cbe_temp247);
  *(&llvm_cbe_tree) = llvm_cbe_temp248;
  llvm_cbe_temp249 = *(&llvm_cbe_tree);
  *(&llvm_cbe_i) = (((unsigned int )(unsigned long)llvm_cbe_temp249));
  llvm_cbe_temp251 = *(&llvm_cbe_i);
  return llvm_cbe_temp251;
}


struct l_ast *macroTest(void) {
  unsigned char *llvm_cbe_temp254;
  struct l_ast *llvm_cbe_temp255;
  struct l_ast *llvm_cbe___temp_6;    /* Address-exposed local */
  struct l_ast *llvm_cbe_temp256;
  unsigned char *llvm_cbe_temp257;
  struct l_ast *llvm_cbe_temp258;
  struct l_ast *llvm_cbe_temp260;

  llvm_cbe_temp254 = *(&tempvar_14);
  llvm_cbe_temp255 = simpleAst(llvm_cbe_temp254);
  *(&llvm_cbe___temp_6) = llvm_cbe_temp255;
  llvm_cbe_temp256 = *(&llvm_cbe___temp_6);
  llvm_cbe_temp257 = *(&tempvar_15);
  llvm_cbe_temp258 = simpleAst(llvm_cbe_temp257);
  addChild(llvm_cbe_temp256, llvm_cbe_temp258);
  llvm_cbe_temp260 = *(&llvm_cbe___temp_6);
  return llvm_cbe_temp260;
}


unsigned int newUniqueId(void) {
  unsigned int llvm_cbe_temp261;
  unsigned int llvm_cbe_temp263;

  llvm_cbe_temp261 = *(&lastid);
  *(&lastid) = (llvm_cbe_temp261 + ((unsigned int )1));
  llvm_cbe_temp263 = *(&lastid);
  return llvm_cbe_temp263;
}


unsigned char *cstrings_2E_append(unsigned char *llvm_cbe_l, unsigned char *llvm_cbe_r) {
  struct l_string *llvm_cbe_temp264;
  struct l_string *llvm_cbe_lstr;    /* Address-exposed local */
  struct l_string *llvm_cbe_temp265;
  struct l_string *llvm_cbe_rstr;    /* Address-exposed local */
  struct l_string *llvm_cbe_temp266;
  struct l_string *llvm_cbe_temp267;
  struct l_string *llvm_cbe_temp268;
  struct l_string *llvm_cbe_appended;    /* Address-exposed local */
  struct l_string *llvm_cbe_temp270;
  unsigned char *llvm_cbe_temp271;

  llvm_cbe_temp264 = makestring(llvm_cbe_l);
  *(&llvm_cbe_lstr) = llvm_cbe_temp264;
  llvm_cbe_temp265 = makestring(llvm_cbe_r);
  *(&llvm_cbe_rstr) = llvm_cbe_temp265;
  llvm_cbe_temp266 = *(&llvm_cbe_lstr);
  llvm_cbe_temp267 = *(&llvm_cbe_rstr);
  llvm_cbe_temp268 = append(llvm_cbe_temp266, llvm_cbe_temp267);
  *(&llvm_cbe_appended) = llvm_cbe_temp268;
  llvm_cbe_temp270 = *(&llvm_cbe_appended);
  llvm_cbe_temp271 = *(&llvm_cbe_temp270->field1);
  return llvm_cbe_temp271;
}


unsigned char *newUniqueName(void) {
  unsigned int llvm_cbe_temp273;
  unsigned int llvm_cbe_id;    /* Address-exposed local */
  unsigned int llvm_cbe_temp274;
  unsigned char *llvm_cbe_temp275;
  unsigned char *llvm_cbe_numstr;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp276;
  unsigned char *llvm_cbe_temp277;
  unsigned char *llvm_cbe_temp278;

  llvm_cbe_temp273 = newUniqueId();
  *(&llvm_cbe_id) = llvm_cbe_temp273;
  llvm_cbe_temp274 = *(&llvm_cbe_id);
  llvm_cbe_temp275 = int2cstring(llvm_cbe_temp274);
  *(&llvm_cbe_numstr) = llvm_cbe_temp275;
  llvm_cbe_temp276 = *(&tempvar_16);
  llvm_cbe_temp277 = *(&llvm_cbe_numstr);
  llvm_cbe_temp278 = cstrings_2E_append(llvm_cbe_temp276, llvm_cbe_temp277);
  return llvm_cbe_temp278;
}


unsigned char *newUniqueNameFor(unsigned char *llvm_cbe_purpose) {
  unsigned char *llvm_cbe_temp280;
  unsigned char *llvm_cbe_name;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp281;
  unsigned char *llvm_cbe_temp282;
  unsigned char *llvm_cbe_temp283;
  unsigned char *llvm_cbe_temp284;

  llvm_cbe_temp280 = newUniqueName();
  *(&llvm_cbe_name) = llvm_cbe_temp280;
  llvm_cbe_temp281 = *(&llvm_cbe_name);
  llvm_cbe_temp282 = *(&tempvar_17);
  llvm_cbe_temp283 = cstrings_2E_append(llvm_cbe_temp281, llvm_cbe_temp282);
  llvm_cbe_temp284 = cstrings_2E_append(llvm_cbe_temp283, llvm_cbe_purpose);
  return llvm_cbe_temp284;
}


void printHLine(signed char llvm_cbe_chr, unsigned int llvm_cbe_count) {
  unsigned int llvm_cbe__tmp_15_timesCounter;    /* Address-exposed local */
  unsigned int llvm_cbe_temp1790;
  bool *llvm_cbe__tmp_18_for_testvar;
  bool llvm_cbe_temp1792;
  unsigned int llvm_cbe_temp1794;

  *(&llvm_cbe__tmp_15_timesCounter) = ((unsigned int )0);
  goto llvm_cbe__tmp_16_for_start;

  do {     /* Syntactic loop '_tmp_16_for_start' to make GCC happy */
llvm_cbe__tmp_16_for_start:
  llvm_cbe_temp1790 = *(&llvm_cbe__tmp_15_timesCounter);
  llvm_cbe__tmp_18_for_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_18_for_testvar = (((((signed int )llvm_cbe_temp1790) < ((signed int )llvm_cbe_count))) & 1);
  llvm_cbe_temp1792 = *llvm_cbe__tmp_18_for_testvar;
  if (llvm_cbe_temp1792) {
    goto llvm_cbe__tmp_19_for_body;
  } else {
    goto llvm_cbe__tmp_17_for_exit;
  }

llvm_cbe__tmp_19_for_body:
  printChar(llvm_cbe_chr);
  llvm_cbe_temp1794 = *(&llvm_cbe__tmp_15_timesCounter);
  *(&llvm_cbe__tmp_15_timesCounter) = (llvm_cbe_temp1794 + ((unsigned int )1));
  goto llvm_cbe__tmp_16_for_start;

  } while (1); /* end of syntactic loop '_tmp_16_for_start' */
llvm_cbe__tmp_17_for_exit:
  println();
  return;
}


void ast_2E_setChild(struct l_ast *llvm_cbe_parent, unsigned int llvm_cbe_index, struct l_ast *llvm_cbe_newChild) {
  unsigned int llvm_cbe_temp2414;
  bool llvm_cbe_temp2416;
  bool llvm_cbe__tmp_29_iftestresult;    /* Address-exposed local */
  bool llvm_cbe_temp2417;
  unsigned char *llvm_cbe_temp2418;
  unsigned char *llvm_cbe_temp2420;
  struct l_ast *llvm_cbe_temp2421;
  struct l_ast **llvm_cbe___temp_167;
  struct l_ast *llvm_cbe_temp2422;
  unsigned char *llvm_cbe_temp2423;
  struct l_ast *llvm_cbe_temp2424;
  struct l_ast *llvm_cbe_temp2426;
  unsigned char *llvm_cbe_temp2427;
  struct l_ast *llvm_cbe_temp2428;
  struct l_ast **llvm_cbe___temp_168;
  struct l_ast *llvm_cbe_temp2429;
  unsigned char *llvm_cbe_temp2430;
  struct l_ast *llvm_cbe_temp2431;
  struct l_ast *llvm_cbe_temp2433;
  unsigned char *llvm_cbe_temp2434;
  struct l_ast *llvm_cbe_temp2435;
  struct l_ast *llvm_cbe_temp2437;
  struct l_ast *llvm_cbe_temp2439;
  struct l_ast **llvm_cbe_temp2444;
  struct l_ast ***llvm_cbe_childptrAddr;
  struct l_ast **llvm_cbe_temp2445;

  llvm_cbe_temp2414 = *(&llvm_cbe_parent->field1);
  llvm_cbe_temp2416 = bool_2E_not((((unsigned int )llvm_cbe_index) < ((unsigned int )llvm_cbe_temp2414)));
  *(&llvm_cbe__tmp_29_iftestresult) = ((llvm_cbe_temp2416) & 1);
  llvm_cbe_temp2417 = *(&llvm_cbe__tmp_29_iftestresult);
  if (llvm_cbe_temp2417) {
    goto llvm_cbe__tmp_26_iftrue;
  } else {
    goto llvm_cbe__tmp_28_ifend;
  }

llvm_cbe__tmp_26_iftrue:
  llvm_cbe_temp2418 = *(&tempvar_385);
  printlnString(llvm_cbe_temp2418);
  llvm_cbe_temp2420 = *(&tempvar_386);
  llvm_cbe_temp2421 = simpleAst(llvm_cbe_temp2420);
  llvm_cbe___temp_167 = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe___temp_167 = llvm_cbe_temp2421;
  llvm_cbe_temp2422 = *llvm_cbe___temp_167;
  llvm_cbe_temp2423 = *(&tempvar_387);
  llvm_cbe_temp2424 = simpleAst(llvm_cbe_temp2423);
  addChild(llvm_cbe_temp2422, llvm_cbe_temp2424);
  llvm_cbe_temp2426 = *llvm_cbe___temp_167;
  llvm_cbe_temp2427 = *(&tempvar_388);
  llvm_cbe_temp2428 = simpleAst(llvm_cbe_temp2427);
  llvm_cbe___temp_168 = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe___temp_168 = llvm_cbe_temp2428;
  llvm_cbe_temp2429 = *llvm_cbe___temp_168;
  llvm_cbe_temp2430 = *(&tempvar_389);
  llvm_cbe_temp2431 = simpleAst(llvm_cbe_temp2430);
  addChild(llvm_cbe_temp2429, llvm_cbe_temp2431);
  llvm_cbe_temp2433 = *llvm_cbe___temp_168;
  llvm_cbe_temp2434 = *(&tempvar_390);
  llvm_cbe_temp2435 = simpleAst(llvm_cbe_temp2434);
  addChild(llvm_cbe_temp2433, llvm_cbe_temp2435);
  llvm_cbe_temp2437 = *llvm_cbe___temp_168;
  addChild(llvm_cbe_temp2426, llvm_cbe_temp2437);
  llvm_cbe_temp2439 = *llvm_cbe___temp_167;
  printAst(llvm_cbe_temp2439, ((unsigned int )0));
  exit(((unsigned int )1));
  goto llvm_cbe__tmp_28_ifend;

llvm_cbe__tmp_28_ifend:
  llvm_cbe_temp2444 = *(&llvm_cbe_parent->field2);
  llvm_cbe_childptrAddr = (struct l_ast ***) alloca(sizeof(struct l_ast **));
  *llvm_cbe_childptrAddr = (&llvm_cbe_temp2444[llvm_cbe_index]);
  llvm_cbe_temp2445 = *llvm_cbe_childptrAddr;
  *llvm_cbe_temp2445 = llvm_cbe_newChild;
  return;
}


void ast_2E_replace(struct l_ast *llvm_cbe_tree, unsigned char *llvm_cbe_placeholder, struct l_ast *llvm_cbe_replacement) {
  unsigned int llvm_cbe_temp2812;
  unsigned int llvm_cbe_count;    /* Address-exposed local */
  unsigned int llvm_cbe_childNum;    /* Address-exposed local */
  unsigned int llvm_cbe_temp2813;
  unsigned int llvm_cbe_temp2814;
  bool *llvm_cbe__tmp_32_for_testvar;
  bool llvm_cbe_temp2816;
  unsigned int llvm_cbe_temp2817;
  struct l_ast *llvm_cbe_temp2818;
  struct l_ast **llvm_cbe_child;
  struct l_ast *llvm_cbe_temp2820;
  unsigned char *llvm_cbe_temp2821;
  bool llvm_cbe_temp2822;
  bool *llvm_cbe__tmp_37_iftestresult;
  bool llvm_cbe_temp2823;
  unsigned int llvm_cbe_temp2824;
  unsigned int llvm_cbe_temp2826;
  unsigned int llvm_cbe_temp2829;
  unsigned int *llvm_cbe__tmp_38_count;
  unsigned int *llvm_cbe__tmp_39_index;
  unsigned int llvm_cbe_temp2830;
  unsigned int llvm_cbe_temp2831;
  bool *llvm_cbe__tmp_42_for_testvar;
  bool llvm_cbe_temp2833;
  unsigned int llvm_cbe_temp2834;
  struct l_ast *llvm_cbe_temp2835;
  struct l_ast **llvm_cbe_c;
  struct l_ast *llvm_cbe_temp2836;
  unsigned int llvm_cbe_temp2838;

  llvm_cbe_temp2812 = *(&llvm_cbe_tree->field1);
  *(&llvm_cbe_count) = llvm_cbe_temp2812;
  *(&llvm_cbe_childNum) = ((unsigned int )0);
  goto llvm_cbe__tmp_30_for_start;

  do {     /* Syntactic loop '_tmp_30_for_start' to make GCC happy */
llvm_cbe__tmp_30_for_start:
  llvm_cbe_temp2813 = *(&llvm_cbe_childNum);
  llvm_cbe_temp2814 = *(&llvm_cbe_count);
  llvm_cbe__tmp_32_for_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_32_for_testvar = (((((signed int )llvm_cbe_temp2813) < ((signed int )llvm_cbe_temp2814))) & 1);
  llvm_cbe_temp2816 = *llvm_cbe__tmp_32_for_testvar;
  if (llvm_cbe_temp2816) {
    goto llvm_cbe__tmp_33_for_body;
  } else {
    goto llvm_cbe__tmp_31_for_exit;
  }

llvm_cbe__tmp_36_ifend:
  llvm_cbe_temp2826 = *(&llvm_cbe_childNum);
  *(&llvm_cbe_childNum) = (llvm_cbe_temp2826 + ((unsigned int )1));
  goto llvm_cbe__tmp_30_for_start;

llvm_cbe__tmp_34_iftrue:
  llvm_cbe_temp2824 = *(&llvm_cbe_childNum);
  ast_2E_setChild(llvm_cbe_tree, llvm_cbe_temp2824, llvm_cbe_replacement);
  goto llvm_cbe__tmp_36_ifend;

llvm_cbe__tmp_33_for_body:
  llvm_cbe_temp2817 = *(&llvm_cbe_childNum);
  llvm_cbe_temp2818 = astChild(llvm_cbe_tree, llvm_cbe_temp2817);
  llvm_cbe_child = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe_child = llvm_cbe_temp2818;
  llvm_cbe_temp2820 = *llvm_cbe_child;
  llvm_cbe_temp2821 = *(&llvm_cbe_temp2820->field0);
  llvm_cbe_temp2822 = cstring_2E_equal(llvm_cbe_placeholder, llvm_cbe_temp2821);
  llvm_cbe__tmp_37_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_37_iftestresult = ((llvm_cbe_temp2822) & 1);
  llvm_cbe_temp2823 = *llvm_cbe__tmp_37_iftestresult;
  if (llvm_cbe_temp2823) {
    goto llvm_cbe__tmp_34_iftrue;
  } else {
    goto llvm_cbe__tmp_36_ifend;
  }

  } while (1); /* end of syntactic loop '_tmp_30_for_start' */
llvm_cbe__tmp_31_for_exit:
  llvm_cbe_temp2829 = *(&llvm_cbe_tree->field1);
  llvm_cbe__tmp_38_count = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe__tmp_38_count = llvm_cbe_temp2829;
  llvm_cbe__tmp_39_index = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe__tmp_39_index = ((unsigned int )0);
  goto llvm_cbe__tmp_40_for_start;

  do {     /* Syntactic loop '_tmp_40_for_start' to make GCC happy */
llvm_cbe__tmp_40_for_start:
  llvm_cbe_temp2830 = *llvm_cbe__tmp_39_index;
  llvm_cbe_temp2831 = *llvm_cbe__tmp_38_count;
  llvm_cbe__tmp_42_for_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_42_for_testvar = (((((signed int )llvm_cbe_temp2830) < ((signed int )llvm_cbe_temp2831))) & 1);
  llvm_cbe_temp2833 = *llvm_cbe__tmp_42_for_testvar;
  if (llvm_cbe_temp2833) {
    goto llvm_cbe__tmp_43_for_body;
  } else {
    goto llvm_cbe__tmp_41_for_exit;
  }

llvm_cbe__tmp_43_for_body:
  llvm_cbe_temp2834 = *llvm_cbe__tmp_39_index;
  llvm_cbe_temp2835 = astChild(llvm_cbe_tree, llvm_cbe_temp2834);
  llvm_cbe_c = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe_c = llvm_cbe_temp2835;
  llvm_cbe_temp2836 = *llvm_cbe_c;
  ast_2E_replace(llvm_cbe_temp2836, llvm_cbe_placeholder, llvm_cbe_replacement);
  llvm_cbe_temp2838 = *llvm_cbe__tmp_39_index;
  *llvm_cbe__tmp_39_index = (llvm_cbe_temp2838 + ((unsigned int )1));
  goto llvm_cbe__tmp_40_for_start;

  } while (1); /* end of syntactic loop '_tmp_40_for_start' */
llvm_cbe__tmp_41_for_exit:
  return;
}


int main(void) {
  bool llvm_cbe_temp23080;
  bool llvm_cbe_temp23081;
  bool llvm_cbe__tmp_97_iftestresult;    /* Address-exposed local */
  bool llvm_cbe_temp23082;
  unsigned int *llvm_cbe_sphereCount;
  unsigned int llvm_cbe_temp23085;
  unsigned char *ltmp_5_1;
  float **llvm_cbe_positions;
  unsigned int *llvm_cbe_index;
  float **llvm_cbe_pos;
  unsigned int llvm_cbe_temp23089;
  unsigned int llvm_cbe_temp23090;
  bool *llvm_cbe__tmp_101_while_testvar;
  bool llvm_cbe_temp23092;
  float *llvm_cbe_temp23094;
  unsigned int llvm_cbe_temp23095;
  float *llvm_cbe_temp23098;
  unsigned int llvm_cbe_temp23099;
  float *llvm_cbe_temp23103;
  unsigned int llvm_cbe_temp23104;
  float *llvm_cbe_temp23108;
  unsigned int llvm_cbe_temp23109;
  unsigned int llvm_cbe_temp23112;
  bool *llvm_cbe_abort;
  double llvm_cbe_temp23114;
  double *llvm_cbe_startTime;
  unsigned int llvm_cbe_temp23115;
  unsigned int *llvm_cbe_billboard;
  unsigned int llvm_cbe_temp23116;
  unsigned int *llvm_cbe_sphereShader;
  bool llvm_cbe_temp23117;
  bool llvm_cbe_temp23118;
  bool *llvm_cbe__tmp_105_while_testvar;
  bool llvm_cbe_temp23119;
  unsigned int llvm_cbe_temp23120;
  unsigned int llvm_cbe_temp23121;
  unsigned int llvm_cbe_temp23122;
  bool *llvm_cbe__tmp_109_iftestresult;
  bool llvm_cbe_temp23124;
  double llvm_cbe_temp23125;
  double llvm_cbe_temp23126;
  double llvm_cbe_temp23128;
  bool *llvm_cbe__tmp_113_iftestresult;
  bool llvm_cbe_temp23130;
  unsigned int llvm_cbe_temp23131;
  unsigned int llvm_cbe_temp23133;
  unsigned int llvm_cbe_temp23135;
  unsigned int llvm_cbe_temp23137;
  int *llvm_cbe_width;
  int *llvm_cbe_height;
  unsigned int llvm_cbe_temp23140;
  unsigned int llvm_cbe_temp23142;
  double *llvm_cbe_aspect;
  unsigned int llvm_cbe_temp23145;
  unsigned int llvm_cbe_temp23146;
  unsigned int llvm_cbe_temp23149;
  unsigned int llvm_cbe_temp23150;
  unsigned int llvm_cbe_temp23153;
  double llvm_cbe_temp23156;
  unsigned int llvm_cbe_temp23158;
  double llvm_cbe_temp23163;
  float *llvm_cbe_sunAngle;
  unsigned int llvm_cbe_temp23165;
  unsigned int llvm_cbe_temp23167;
  unsigned int llvm_cbe_temp23169;
  unsigned char *llvm_cbe_temp23170;
  unsigned int llvm_cbe_temp23171;
  unsigned int *llvm_cbe_lightPosL;
  unsigned int llvm_cbe_temp23172;
  float llvm_cbe_temp23173;
  float llvm_cbe_temp23174;
  float llvm_cbe_temp23175;
  float llvm_cbe_temp23176;
  unsigned int *llvm_cbe_index2;
  float **llvm_cbe_pos2;
  unsigned int llvm_cbe_temp23180;
  unsigned int llvm_cbe_temp23181;
  bool *llvm_cbe__tmp_117_while_testvar;
  bool llvm_cbe_temp23183;
  float *llvm_cbe_temp23185;
  unsigned int llvm_cbe_temp23186;
  float *llvm_cbe_temp23189;
  float llvm_cbe_temp23190;
  float *llvm_cbe_x;
  float *llvm_cbe_temp23192;
  float llvm_cbe_temp23193;
  float *llvm_cbe_y;
  float *llvm_cbe_temp23195;
  float llvm_cbe_temp23196;
  float *llvm_cbe_z;
  float llvm_cbe_temp23197;
  float llvm_cbe_temp23198;
  float llvm_cbe_temp23199;
  float llvm_cbe_temp23203;
  float llvm_cbe_temp23205;
  float llvm_cbe_temp23207;
  unsigned int llvm_cbe_temp23210;
  unsigned int llvm_cbe_temp23216;

  CODE_FOR_MAIN();
  llvm_cbe_temp23080 = init();
  llvm_cbe_temp23081 = bool_2E_not(llvm_cbe_temp23080);
  *(&llvm_cbe__tmp_97_iftestresult) = ((llvm_cbe_temp23081) & 1);
  llvm_cbe_temp23082 = *(&llvm_cbe__tmp_97_iftestresult);
  if (llvm_cbe_temp23082) {
    goto llvm_cbe__tmp_94_iftrue;
  } else {
    goto llvm_cbe__tmp_96_ifend;
  }

llvm_cbe__tmp_94_iftrue:
  return ((unsigned int )100);
llvm_cbe__tmp_96_ifend:
  glfwPollEvents();
  glfwPollEvents();
  llvm_cbe_sphereCount = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_sphereCount = ((unsigned int )1000);
  llvm_cbe_temp23085 = *llvm_cbe_sphereCount;
  ltmp_5_1 =  /*tail*/ (unsigned char*) malloc(((((unsigned int )3) * llvm_cbe_temp23085) * ((unsigned int )4)));
  llvm_cbe_positions = (float **) alloca(sizeof(float *));
  *llvm_cbe_positions = (((float *)ltmp_5_1));
  llvm_cbe_index = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_index = ((unsigned int )0);
  llvm_cbe_pos = (float **) alloca(sizeof(float *));
  *llvm_cbe_pos = (((float *)((unsigned char *)/*NULL*/0)));
  goto llvm_cbe__tmp_98_while_begin;

  do {     /* Syntactic loop '_tmp_98_while_begin' to make GCC happy */
llvm_cbe__tmp_98_while_begin:
  llvm_cbe_temp23089 = *llvm_cbe_index;
  llvm_cbe_temp23090 = *llvm_cbe_sphereCount;
  llvm_cbe__tmp_101_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_101_while_testvar = (((((signed int )llvm_cbe_temp23089) < ((signed int )llvm_cbe_temp23090))) & 1);
  llvm_cbe_temp23092 = *llvm_cbe__tmp_101_while_testvar;
  if (llvm_cbe_temp23092) {
    goto llvm_cbe__tmp_100_while_body;
  } else {
    goto llvm_cbe__tmp_99_while_exit;
  }

llvm_cbe__tmp_100_while_body:
  llvm_cbe_temp23094 = *llvm_cbe_positions;
  llvm_cbe_temp23095 = *llvm_cbe_index;
  *llvm_cbe_pos = (&llvm_cbe_temp23094[(llvm_cbe_temp23095 * ((unsigned int )3))]);
  llvm_cbe_temp23098 = *llvm_cbe_pos;
  llvm_cbe_temp23099 = random(((unsigned int )30));
  *(&llvm_cbe_temp23098[((unsigned int )0)]) = (((float )(signed int )(llvm_cbe_temp23099 - ((unsigned int )15))));
  llvm_cbe_temp23103 = *llvm_cbe_pos;
  llvm_cbe_temp23104 = random(((unsigned int )30));
  *(&llvm_cbe_temp23103[((unsigned int )1)]) = (((float )(signed int )(llvm_cbe_temp23104 - ((unsigned int )15))));
  llvm_cbe_temp23108 = *llvm_cbe_pos;
  llvm_cbe_temp23109 = random(((unsigned int )30));
  *(&llvm_cbe_temp23108[((unsigned int )2)]) = (((float )(signed int )(llvm_cbe_temp23109 - ((unsigned int )15))));
  llvm_cbe_temp23112 = *llvm_cbe_index;
  *llvm_cbe_index = (llvm_cbe_temp23112 + ((unsigned int )1));
  goto llvm_cbe__tmp_98_while_begin;

  } while (1); /* end of syntactic loop '_tmp_98_while_begin' */
llvm_cbe__tmp_99_while_exit:
  llvm_cbe_abort = (bool *) alloca(sizeof(bool ));
  *llvm_cbe_abort = ((0) & 1);
  llvm_cbe_temp23114 = glfwGetTime();
  llvm_cbe_startTime = (double *) alloca(sizeof(double ));
  *llvm_cbe_startTime = llvm_cbe_temp23114;
  llvm_cbe_temp23115 = makeBillboard(((unsigned int )1024), ((unsigned int )1024));
  llvm_cbe_billboard = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_billboard = llvm_cbe_temp23115;
  llvm_cbe_temp23116 = makeMainShader();
  llvm_cbe_sphereShader = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_sphereShader = llvm_cbe_temp23116;
  goto llvm_cbe__tmp_102_while_begin;

  do {     /* Syntactic loop '_tmp_102_while_begin' to make GCC happy */
llvm_cbe__tmp_102_while_begin:
  llvm_cbe_temp23117 = *llvm_cbe_abort;
  llvm_cbe_temp23118 = bool_2E_not(llvm_cbe_temp23117);
  llvm_cbe__tmp_105_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_105_while_testvar = ((llvm_cbe_temp23118) & 1);
  llvm_cbe_temp23119 = *llvm_cbe__tmp_105_while_testvar;
  if (llvm_cbe_temp23119) {
    goto llvm_cbe__tmp_104_while_body;
  } else {
    goto llvm_cbe__tmp_103_while_exit;
  }

llvm_cbe__tmp_115_while_exit:
  glBindTexture2D(((unsigned int )0));
  glUseProgram(((unsigned int )0));
  glfwSwapBuffers();
  glfwPollEvents();
  goto llvm_cbe__tmp_102_while_begin;

  do {     /* Syntactic loop '_tmp_114_while_begin' to make GCC happy */
llvm_cbe__tmp_114_while_begin:
  llvm_cbe_temp23180 = *llvm_cbe_index2;
  llvm_cbe_temp23181 = *llvm_cbe_sphereCount;
  llvm_cbe__tmp_117_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_117_while_testvar = (((((signed int )llvm_cbe_temp23180) < ((signed int )llvm_cbe_temp23181))) & 1);
  llvm_cbe_temp23183 = *llvm_cbe__tmp_117_while_testvar;
  if (llvm_cbe_temp23183) {
    goto llvm_cbe__tmp_116_while_body;
  } else {
    goto llvm_cbe__tmp_115_while_exit;
  }

llvm_cbe__tmp_116_while_body:
  llvm_cbe_temp23185 = *llvm_cbe_positions;
  llvm_cbe_temp23186 = *llvm_cbe_index2;
  *llvm_cbe_pos2 = (&llvm_cbe_temp23185[(llvm_cbe_temp23186 * ((unsigned int )3))]);
  llvm_cbe_temp23189 = *llvm_cbe_pos2;
  llvm_cbe_temp23190 = *(&llvm_cbe_temp23189[((unsigned int )0)]);
  llvm_cbe_x = (float *) alloca(sizeof(float ));
  *llvm_cbe_x = llvm_cbe_temp23190;
  llvm_cbe_temp23192 = *llvm_cbe_pos2;
  llvm_cbe_temp23193 = *(&llvm_cbe_temp23192[((unsigned int )1)]);
  llvm_cbe_y = (float *) alloca(sizeof(float ));
  *llvm_cbe_y = llvm_cbe_temp23193;
  llvm_cbe_temp23195 = *llvm_cbe_pos2;
  llvm_cbe_temp23196 = *(&llvm_cbe_temp23195[((unsigned int )2)]);
  llvm_cbe_z = (float *) alloca(sizeof(float ));
  *llvm_cbe_z = llvm_cbe_temp23196;
  llvm_cbe_temp23197 = *llvm_cbe_x;
  llvm_cbe_temp23198 = *llvm_cbe_y;
  llvm_cbe_temp23199 = *llvm_cbe_z;
  glTranslatef(llvm_cbe_temp23197, llvm_cbe_temp23198, llvm_cbe_temp23199);
  renderQuad((((float )(0x1p+0 / 0x1p+1))));
  llvm_cbe_temp23203 = *llvm_cbe_x;
  llvm_cbe_temp23205 = *llvm_cbe_y;
  llvm_cbe_temp23207 = *llvm_cbe_z;
  glTranslatef((((float )(0x0p+0 - llvm_cbe_temp23203))), (((float )(0x0p+0 - llvm_cbe_temp23205))), (((float )(0x0p+0 - llvm_cbe_temp23207))));
  llvm_cbe_temp23210 = *llvm_cbe_index2;
  *llvm_cbe_index2 = (llvm_cbe_temp23210 + ((unsigned int )1));
  goto llvm_cbe__tmp_114_while_begin;

  } while (1); /* end of syntactic loop '_tmp_114_while_begin' */
llvm_cbe__tmp_112_ifend:
  llvm_cbe_temp23131 = GL_DEPTH_TEST;
  glEnable(llvm_cbe_temp23131);
  llvm_cbe_temp23133 = GL_LEQUAL;
  glDepthFunc(llvm_cbe_temp23133);
  llvm_cbe_temp23135 = GL_ALPHA_TEST;
  glEnable(llvm_cbe_temp23135);
  llvm_cbe_temp23137 = GL_NOTEQUAL;
  glAlphaFunc(llvm_cbe_temp23137, 0x0p+0);
  llvm_cbe_width = (int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_width = ((int )0);
  llvm_cbe_height = (int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_height = ((int )0);
  glfwGetWindowSize(llvm_cbe_width, llvm_cbe_height);
  llvm_cbe_temp23140 = *llvm_cbe_width;
  llvm_cbe_temp23142 = *llvm_cbe_height;
  llvm_cbe_aspect = (double *) alloca(sizeof(double ));
  *llvm_cbe_aspect = ((((double )(signed int )llvm_cbe_temp23140)) / (((double )(signed int )llvm_cbe_temp23142)));
  llvm_cbe_temp23145 = *llvm_cbe_width;
  llvm_cbe_temp23146 = *llvm_cbe_height;
  glViewport(((unsigned int )0), ((unsigned int )0), llvm_cbe_temp23145, llvm_cbe_temp23146);
  glClearColor(0x0p+0, 0x0p+0, 0x0p+0, 0x1p+0);
  llvm_cbe_temp23149 = GL_COLOR_BUFFER_BIT;
  llvm_cbe_temp23150 = GL_DEPTH_BUFFER_BIT;
  glClear((llvm_cbe_temp23149 | llvm_cbe_temp23150));
  llvm_cbe_temp23153 = GL_PROJECTION;
  glMatrixMode(llvm_cbe_temp23153);
  glLoadIdentity();
  llvm_cbe_temp23156 = *llvm_cbe_aspect;
  gluPerspective(0x1.68p+6, llvm_cbe_temp23156, 0x1p+0, 0x1.9p+6);
  llvm_cbe_temp23158 = GL_MODELVIEW;
  glMatrixMode(llvm_cbe_temp23158);
  glLoadIdentity();
  glTranslatef(-0x1p+1, -0x1.8p+0, -0x1.4p+4);
  drawOrientationGrid();
  llvm_cbe_temp23163 = glfwGetTime();
  llvm_cbe_sunAngle = (float *) alloca(sizeof(float ));
  *llvm_cbe_sunAngle = (((float )llvm_cbe_temp23163));
  llvm_cbe_temp23165 = *llvm_cbe_sphereShader;
  glUseProgram(llvm_cbe_temp23165);
  llvm_cbe_temp23167 = *llvm_cbe_billboard;
  glBindTexture2D(llvm_cbe_temp23167);
  llvm_cbe_temp23169 = *llvm_cbe_sphereShader;
  llvm_cbe_temp23170 = *(&tempvar_4908);
  llvm_cbe_temp23171 = glGetUniformLocation(llvm_cbe_temp23169, (GLchar*)llvm_cbe_temp23170);
  llvm_cbe_lightPosL = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_lightPosL = llvm_cbe_temp23171;
  llvm_cbe_temp23172 = *llvm_cbe_lightPosL;
  llvm_cbe_temp23173 = *llvm_cbe_sunAngle;
  llvm_cbe_temp23174 = sinf(llvm_cbe_temp23173);
  llvm_cbe_temp23175 = *llvm_cbe_sunAngle;
  llvm_cbe_temp23176 = cosf(llvm_cbe_temp23175);
  glUniform4f(llvm_cbe_temp23172, llvm_cbe_temp23174, llvm_cbe_temp23176, 0x1p+1, 0x1p+0);
  renderQuad(0x1p+0);
  llvm_cbe_index2 = (unsigned int *) alloca(sizeof(unsigned int ));
  *llvm_cbe_index2 = ((unsigned int )0);
  llvm_cbe_pos2 = (float **) alloca(sizeof(float *));
  *llvm_cbe_pos2 = (((float *)((unsigned char *)/*NULL*/0)));
  goto llvm_cbe__tmp_114_while_begin;

llvm_cbe__tmp_110_iftrue:
  *llvm_cbe_abort = ((1) & 1);
  goto llvm_cbe__tmp_112_ifend;

llvm_cbe__tmp_108_ifend:
  llvm_cbe_temp23125 = glfwGetTime();
  llvm_cbe_temp23126 = *llvm_cbe_startTime;
  llvm_cbe_temp23128 = *(&autoTimeout);
  llvm_cbe__tmp_113_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_113_iftestresult = (((llvm_fcmp_ogt((llvm_cbe_temp23125 - llvm_cbe_temp23126), llvm_cbe_temp23128))) & 1);
  llvm_cbe_temp23130 = *llvm_cbe__tmp_113_iftestresult;
  if (llvm_cbe_temp23130) {
    goto llvm_cbe__tmp_110_iftrue;
  } else {
    goto llvm_cbe__tmp_112_ifend;
  }

llvm_cbe__tmp_106_iftrue:
  *llvm_cbe_abort = ((1) & 1);
  goto llvm_cbe__tmp_108_ifend;

llvm_cbe__tmp_104_while_body:
  llvm_cbe_temp23120 = GLFW_KEY_ESC;
  llvm_cbe_temp23121 = glfwGetKey(llvm_cbe_temp23120);
  llvm_cbe_temp23122 = GL_TRUE;
  llvm_cbe__tmp_109_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_109_iftestresult = (((llvm_cbe_temp23121 == llvm_cbe_temp23122)) & 1);
  llvm_cbe_temp23124 = *llvm_cbe__tmp_109_iftestresult;
  if (llvm_cbe_temp23124) {
    goto llvm_cbe__tmp_106_iftrue;
  } else {
    goto llvm_cbe__tmp_108_ifend;
  }

  } while (1); /* end of syntactic loop '_tmp_102_while_begin' */
llvm_cbe__tmp_103_while_exit:
  llvm_cbe_temp23216 = *llvm_cbe_sphereShader;
  glDeleteProgram(llvm_cbe_temp23216);
  glDeleteTextures(((unsigned int )1), (GLuint*)llvm_cbe_billboard);
  drawPausedOverlay();
  return ((unsigned int )0);
}


void runMain(void) {
  unsigned int llvm_cbe_temp3272;
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp3274;
  unsigned int llvm_cbe_temp3276;

  llvm_cbe_temp3272 = main();
  *(&llvm_cbe_retval) = llvm_cbe_temp3272;
  println();
  llvm_cbe_temp3274 = *(&tempvar_540);
  printString(llvm_cbe_temp3274);
  llvm_cbe_temp3276 = *(&llvm_cbe_retval);
  printlnInt(llvm_cbe_temp3276);
  return;
}


void test(void) {
  runMain();
  return;
}


void printGLError(void) {
  unsigned int llvm_cbe_error;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22614;
  unsigned int llvm_cbe_temp22615;
  bool *llvm_cbe__tmp_53_while_testvar;
  bool llvm_cbe_temp22617;
  unsigned int llvm_cbe_temp22618;
  unsigned char *llvm_cbe_temp22619;
  unsigned char **llvm_cbe_message;
  unsigned char *llvm_cbe_temp22620;
  unsigned int llvm_cbe_temp22622;
  unsigned char *llvm_cbe_temp22624;
  unsigned char *llvm_cbe_temp22626;

  *(&llvm_cbe_error) = ((unsigned int )0);
  goto llvm_cbe__tmp_50_while_begin;

  do {     /* Syntactic loop '_tmp_50_while_begin' to make GCC happy */
llvm_cbe__tmp_50_while_begin:
  llvm_cbe_temp22614 = glGetError();
  *(&llvm_cbe_error) = llvm_cbe_temp22614;
  llvm_cbe_temp22615 = *(&llvm_cbe_error);
  llvm_cbe__tmp_53_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_53_while_testvar = (((llvm_cbe_temp22615 != ((unsigned int )0))) & 1);
  llvm_cbe_temp22617 = *llvm_cbe__tmp_53_while_testvar;
  if (llvm_cbe_temp22617) {
    goto llvm_cbe__tmp_52_while_body;
  } else {
    goto llvm_cbe__tmp_51_while_exit;
  }

llvm_cbe__tmp_52_while_body:
  llvm_cbe_temp22618 = *(&llvm_cbe_error);
  llvm_cbe_temp22619 = (unsigned char*)gluErrorString((GLenum)llvm_cbe_temp22618);
  llvm_cbe_message = (unsigned char **) alloca(sizeof(unsigned char *));
  *llvm_cbe_message = llvm_cbe_temp22619;
  llvm_cbe_temp22620 = *(&tempvar_3745);
  printString(llvm_cbe_temp22620);
  llvm_cbe_temp22622 = *(&llvm_cbe_error);
  printInt(llvm_cbe_temp22622);
  llvm_cbe_temp22624 = *(&tempvar_3746);
  printString(llvm_cbe_temp22624);
  llvm_cbe_temp22626 = *llvm_cbe_message;
  printlnString(llvm_cbe_temp22626);
  goto llvm_cbe__tmp_50_while_begin;

  } while (1); /* end of syntactic loop '_tmp_50_while_begin' */
llvm_cbe__tmp_51_while_exit:
  return;
}


void printGlfwVersion(void) {
  int llvm_cbe_major;    /* Address-exposed local */
  int llvm_cbe_minor;    /* Address-exposed local */
  int llvm_cbe_rev;    /* Address-exposed local */
  unsigned char *llvm_cbe_temp22629;
  unsigned char *llvm_cbe_temp22631;
  unsigned int llvm_cbe_temp22633;
  unsigned char *llvm_cbe_temp22635;
  unsigned int llvm_cbe_temp22637;
  unsigned char *llvm_cbe_temp22639;
  unsigned int llvm_cbe_temp22641;

  *(&llvm_cbe_major) = ((int )0);
  *(&llvm_cbe_minor) = ((int )0);
  *(&llvm_cbe_rev) = ((int )0);
  glfwGetVersion((&llvm_cbe_major), (&llvm_cbe_minor), (&llvm_cbe_rev));
  llvm_cbe_temp22629 = *(&tempvar_3747);
  printString(llvm_cbe_temp22629);
  llvm_cbe_temp22631 = *(&tempvar_3748);
  printString(llvm_cbe_temp22631);
  llvm_cbe_temp22633 = *(&llvm_cbe_major);
  printInt(llvm_cbe_temp22633);
  llvm_cbe_temp22635 = *(&tempvar_3749);
  printString(llvm_cbe_temp22635);
  llvm_cbe_temp22637 = *(&llvm_cbe_minor);
  printInt(llvm_cbe_temp22637);
  llvm_cbe_temp22639 = *(&tempvar_3750);
  printString(llvm_cbe_temp22639);
  llvm_cbe_temp22641 = *(&llvm_cbe_rev);
  printInt(llvm_cbe_temp22641);
  println();
  return;
}


bool checkAndReportShaderError(unsigned int llvm_cbe_shader, unsigned char *llvm_cbe_shaderName) {
  GLint llvm_cbe_compileError;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22644;
  unsigned int llvm_cbe_temp22646;
  unsigned int llvm_cbe_temp22647;
  bool llvm_cbe__tmp_57_iftestresult;    /* Address-exposed local */
  bool llvm_cbe_temp22649;
  unsigned char *llvm_cbe_temp22650;
  GLsizei *llvm_cbe_maxLength;
  unsigned int llvm_cbe_temp22653;
  GLchar *ltmp_6_1;
  GLchar **llvm_cbe_message;
  unsigned int llvm_cbe_temp22655;
  GLchar *llvm_cbe_temp22656;
  GLchar *llvm_cbe_temp22658;

  *(&llvm_cbe_compileError) = ((GLint)0);
  llvm_cbe_temp22644 = GL_COMPILE_STATUS;
  glGetShaderiv(llvm_cbe_shader, llvm_cbe_temp22644, (&llvm_cbe_compileError));
  llvm_cbe_temp22646 = GL_FALSE;
  llvm_cbe_temp22647 = *(&llvm_cbe_compileError);
  *(&llvm_cbe__tmp_57_iftestresult) = (((llvm_cbe_temp22646 == llvm_cbe_temp22647)) & 1);
  llvm_cbe_temp22649 = *(&llvm_cbe__tmp_57_iftestresult);
  if (llvm_cbe_temp22649) {
    goto llvm_cbe__tmp_54_iftrue;
  } else {
    goto llvm_cbe__tmp_55_iffalse;
  }

llvm_cbe__tmp_54_iftrue:
  llvm_cbe_temp22650 = *(&tempvar_3781);
  printString(llvm_cbe_temp22650);
  printlnString(llvm_cbe_shaderName);
  llvm_cbe_maxLength = (GLsizei *) alloca(sizeof(GLsizei ));
  *llvm_cbe_maxLength = ((GLsizei )500);
  llvm_cbe_temp22653 = *llvm_cbe_maxLength;
  ltmp_6_1 =  /*tail*/ (GLchar*)malloc(llvm_cbe_temp22653);
  llvm_cbe_message = (GLchar **) alloca(sizeof(GLchar *));
  *llvm_cbe_message = (((GLchar *)ltmp_6_1));
  llvm_cbe_temp22655 = *llvm_cbe_maxLength;
  llvm_cbe_temp22656 = *llvm_cbe_message;
  glGetShaderInfoLog(llvm_cbe_shader, llvm_cbe_temp22655, llvm_cbe_maxLength, llvm_cbe_temp22656);
  llvm_cbe_temp22658 = *llvm_cbe_message;
  printlnString((unsigned char*)llvm_cbe_temp22658);
  return 0;
llvm_cbe__tmp_55_iffalse:
  return 1;
}


unsigned int createShader(unsigned char *llvm_cbe_vertexSource, unsigned char *llvm_cbe_fragmentSource) {
  unsigned int llvm_cbe_temp22660;
  unsigned int llvm_cbe_program;    /* Address-exposed local */
  const GLchar *llvm_cbe_vertexSource_;    /* Address-exposed local */
  const GLchar *llvm_cbe_fragmentSource_;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22661;
  unsigned int llvm_cbe_temp22662;
  unsigned int llvm_cbe_vertexShader;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22663;
  unsigned int llvm_cbe_temp22664;
  unsigned int llvm_cbe_fragmentShader;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22665;
  unsigned int llvm_cbe_temp22668;
  unsigned int llvm_cbe_temp22671;
  unsigned int llvm_cbe_temp22673;
  unsigned char *llvm_cbe_temp22674;
  bool llvm_cbe_temp22675;
  bool llvm_cbe__tmp_61_iftestresult;    /* Address-exposed local */
  bool llvm_cbe_temp22676;
  unsigned int llvm_cbe_temp22677;
  unsigned int llvm_cbe_temp22679;
  GLchar *llvm_cbe_temp22680;
  bool llvm_cbe_temp22681;
  bool *llvm_cbe__tmp_65_iftestresult;
  bool llvm_cbe_temp22682;
  unsigned int llvm_cbe_temp22683;
  unsigned int llvm_cbe_temp22684;
  unsigned int llvm_cbe_temp22686;
  unsigned int llvm_cbe_temp22687;
  unsigned int llvm_cbe_temp22689;
  unsigned int llvm_cbe_temp22691;

  llvm_cbe_temp22660 = glCreateProgram();
  *(&llvm_cbe_program) = llvm_cbe_temp22660;
  *(&llvm_cbe_vertexSource_) = (GLchar*)llvm_cbe_vertexSource;
  *(&llvm_cbe_fragmentSource_) = (GLchar*)llvm_cbe_fragmentSource;
  llvm_cbe_temp22661 = GL_VERTEX_SHADER;
  llvm_cbe_temp22662 = glCreateShader(llvm_cbe_temp22661);
  *(&llvm_cbe_vertexShader) = llvm_cbe_temp22662;
  llvm_cbe_temp22663 = GL_FRAGMENT_SHADER;
  llvm_cbe_temp22664 = glCreateShader(llvm_cbe_temp22663);
  *(&llvm_cbe_fragmentShader) = llvm_cbe_temp22664;
  llvm_cbe_temp22665 = *(&llvm_cbe_vertexShader);
  glShaderSource(
    llvm_cbe_temp22665,
    ((unsigned int )1),
    & llvm_cbe_vertexSource_,
    NULL);
  llvm_cbe_temp22668 = *(&llvm_cbe_fragmentShader);
  glShaderSource(llvm_cbe_temp22668, 1, (&llvm_cbe_fragmentSource_), NULL);
  llvm_cbe_temp22671 = *(&llvm_cbe_vertexShader);
  glCompileShader(llvm_cbe_temp22671);
  llvm_cbe_temp22673 = *(&llvm_cbe_vertexShader);
  llvm_cbe_temp22674 = *(&tempvar_3813);
  llvm_cbe_temp22675 = checkAndReportShaderError(llvm_cbe_temp22673, llvm_cbe_temp22674);
  *(&llvm_cbe__tmp_61_iftestresult) = ((llvm_cbe_temp22675) & 1);
  llvm_cbe_temp22676 = *(&llvm_cbe__tmp_61_iftestresult);
  if (llvm_cbe_temp22676) {
    goto llvm_cbe__tmp_60_ifend;
  } else {
    goto llvm_cbe__tmp_59_iffalse;
  }

llvm_cbe__tmp_59_iffalse:
  return ((unsigned int )0);
llvm_cbe__tmp_60_ifend:
  llvm_cbe_temp22677 = *(&llvm_cbe_fragmentShader);
  glCompileShader(llvm_cbe_temp22677);
  llvm_cbe_temp22679 = *(&llvm_cbe_fragmentShader);
/*   llvm_cbe_temp22680 = *(&tempvar_3822); */
  llvm_cbe_temp22681 = checkAndReportShaderError(llvm_cbe_temp22679, tempvar_3822);
/*   llvm_cbe_temp22681 = checkAndReportShaderError(llvm_cbe_temp22679, llvm_cbe_temp22680); */
  llvm_cbe__tmp_65_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_65_iftestresult = ((llvm_cbe_temp22681) & 1);
  llvm_cbe_temp22682 = *llvm_cbe__tmp_65_iftestresult;
  if (llvm_cbe_temp22682) {
    goto llvm_cbe__tmp_64_ifend;
  } else {
    goto llvm_cbe__tmp_63_iffalse;
  }

llvm_cbe__tmp_63_iffalse:
  return ((unsigned int )0);
llvm_cbe__tmp_64_ifend:
  llvm_cbe_temp22683 = *(&llvm_cbe_program);
  llvm_cbe_temp22684 = *(&llvm_cbe_vertexShader);
  glAttachShader(llvm_cbe_temp22683, llvm_cbe_temp22684);
  llvm_cbe_temp22686 = *(&llvm_cbe_program);
  llvm_cbe_temp22687 = *(&llvm_cbe_fragmentShader);
  glAttachShader(llvm_cbe_temp22686, llvm_cbe_temp22687);
  llvm_cbe_temp22689 = *(&llvm_cbe_program);
  glLinkProgram(llvm_cbe_temp22689);
  llvm_cbe_temp22691 = *(&llvm_cbe_program);
  return llvm_cbe_temp22691;
}


void renderStripes(float llvm_cbe_count, float llvm_cbe_size, float llvm_cbe_alpha) {
  float llvm_cbe_left;    /* Address-exposed local */
  float llvm_cbe_right;    /* Address-exposed local */
  float llvm_cbe_y;    /* Address-exposed local */
  float llvm_cbe_step;    /* Address-exposed local */
  bool llvm_cbe_black;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22696;
  float llvm_cbe_temp22698;
  bool *llvm_cbe__tmp_69_while_testvar;
  bool llvm_cbe_temp22700;
  bool llvm_cbe_temp22701;
  bool *llvm_cbe__tmp_73_iftestresult;
  bool llvm_cbe_temp22702;
  bool llvm_cbe_temp22705;
  bool llvm_cbe_temp22706;
  float llvm_cbe_temp22707;
  float llvm_cbe_temp22708;
  float llvm_cbe_temp22710;
  float llvm_cbe_temp22711;
  float llvm_cbe_temp22713;
  float llvm_cbe_temp22714;
  float llvm_cbe_temp22716;
  float llvm_cbe_temp22717;
  float llvm_cbe_temp22719;
  float llvm_cbe_temp22720;

  *(&llvm_cbe_left) = (((float )(0x0p+0 - llvm_cbe_size)));
  *(&llvm_cbe_right) = llvm_cbe_size;
  *(&llvm_cbe_y) = (((float )(0x0p+0 - llvm_cbe_size)));
  *(&llvm_cbe_step) = (((float )((((float )(0x1p+1 * llvm_cbe_size))) / llvm_cbe_count)));
  *(&llvm_cbe_black) = ((0) & 1);
  llvm_cbe_temp22696 = GL_QUADS;
  glBegin(llvm_cbe_temp22696);
  goto llvm_cbe__tmp_66_while_begin;

  do {     /* Syntactic loop '_tmp_66_while_begin' to make GCC happy */
llvm_cbe__tmp_66_while_begin:
  llvm_cbe_temp22698 = *(&llvm_cbe_y);
  llvm_cbe__tmp_69_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_69_while_testvar = (((llvm_fcmp_olt(llvm_cbe_temp22698, llvm_cbe_size))) & 1);
  llvm_cbe_temp22700 = *llvm_cbe__tmp_69_while_testvar;
  if (llvm_cbe_temp22700) {
    goto llvm_cbe__tmp_68_while_body;
  } else {
    goto llvm_cbe__tmp_67_while_exit;
  }

llvm_cbe__tmp_72_ifend:
  llvm_cbe_temp22705 = *(&llvm_cbe_black);
  llvm_cbe_temp22706 = bool_2E_not(llvm_cbe_temp22705);
  *(&llvm_cbe_black) = ((llvm_cbe_temp22706) & 1);
  llvm_cbe_temp22707 = *(&llvm_cbe_left);
  llvm_cbe_temp22708 = *(&llvm_cbe_y);
  glVertex3f(llvm_cbe_temp22707, llvm_cbe_temp22708, 0x0p+0);
  llvm_cbe_temp22710 = *(&llvm_cbe_right);
  llvm_cbe_temp22711 = *(&llvm_cbe_y);
  glVertex3f(llvm_cbe_temp22710, llvm_cbe_temp22711, 0x0p+0);
  llvm_cbe_temp22713 = *(&llvm_cbe_y);
  llvm_cbe_temp22714 = *(&llvm_cbe_step);
  *(&llvm_cbe_y) = (((float )(llvm_cbe_temp22713 + llvm_cbe_temp22714)));
  llvm_cbe_temp22716 = *(&llvm_cbe_right);
  llvm_cbe_temp22717 = *(&llvm_cbe_y);
  glVertex3f(llvm_cbe_temp22716, llvm_cbe_temp22717, 0x0p+0);
  llvm_cbe_temp22719 = *(&llvm_cbe_left);
  llvm_cbe_temp22720 = *(&llvm_cbe_y);
  glVertex3f(llvm_cbe_temp22719, llvm_cbe_temp22720, 0x0p+0);
  goto llvm_cbe__tmp_66_while_begin;

llvm_cbe__tmp_70_iftrue:
  glColor4f(0x0p+0, 0x0p+0, 0x0p+0, llvm_cbe_alpha);
  goto llvm_cbe__tmp_72_ifend;

llvm_cbe__tmp_68_while_body:
  llvm_cbe_temp22701 = *(&llvm_cbe_black);
  llvm_cbe__tmp_73_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_73_iftestresult = ((llvm_cbe_temp22701) & 1);
  llvm_cbe_temp22702 = *llvm_cbe__tmp_73_iftestresult;
  if (llvm_cbe_temp22702) {
    goto llvm_cbe__tmp_70_iftrue;
  } else {
    goto llvm_cbe__tmp_71_iffalse;
  }

llvm_cbe__tmp_71_iffalse:
  glColor4f(0x1p+0, 0x1p+0, 0x0p+0, llvm_cbe_alpha);
  goto llvm_cbe__tmp_72_ifend;

  } while (1); /* end of syntactic loop '_tmp_66_while_begin' */
llvm_cbe__tmp_67_while_exit:
  glEnd();
  return;
}


void drawPausedOverlay(void) {
  unsigned int llvm_cbe_temp22723;
  unsigned int llvm_cbe_temp22725;
  unsigned int llvm_cbe_temp22727;
  unsigned int llvm_cbe_temp22728;
  unsigned int llvm_cbe_temp22730;
  unsigned int llvm_cbe_temp22733;

  llvm_cbe_temp22723 = GL_BLEND;
  glPushAttrib(llvm_cbe_temp22723);
  llvm_cbe_temp22725 = GL_BLEND;
  glEnable(llvm_cbe_temp22725);
  llvm_cbe_temp22727 = GL_SRC_ALPHA;
  llvm_cbe_temp22728 = GL_ONE_MINUS_SRC_ALPHA;
  glBlendFunc(llvm_cbe_temp22727, llvm_cbe_temp22728);
  llvm_cbe_temp22730 = GL_PROJECTION;
  glMatrixMode(llvm_cbe_temp22730);
  glLoadIdentity();
  llvm_cbe_temp22733 = GL_MODELVIEW;
  glMatrixMode(llvm_cbe_temp22733);
  glLoadIdentity();
  glScalef(0x1.8p-1, 0x1p+0, 0x1p+0);
  glRotatef(0x1.68p+5, 0x0p+0, 0x0p+0, 0x1p+0);
  renderStripes(0x1.4p+3, 0x1p+1, 0x1p-3);
  glfwSwapBuffers();
  glPopAttrib();
  return;
}


void renderQuad(float llvm_cbe_size) {
  float llvm_cbe_nsize;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22742;
  float llvm_cbe_temp22745;
  float llvm_cbe_temp22746;
  float llvm_cbe_temp22749;
  float llvm_cbe_temp22754;

  *(&llvm_cbe_nsize) = (((float )(0x0p+0 - llvm_cbe_size)));
  llvm_cbe_temp22742 = GL_QUADS;
  glBegin(llvm_cbe_temp22742);
  glTexCoord2f(0x0p+0, 0x0p+0);
  llvm_cbe_temp22745 = *(&llvm_cbe_nsize);
  llvm_cbe_temp22746 = *(&llvm_cbe_nsize);
  glVertex3f(llvm_cbe_temp22745, llvm_cbe_temp22746, 0x0p+0);
  glTexCoord2f(0x0p+0, 0x1p+0);
  llvm_cbe_temp22749 = *(&llvm_cbe_nsize);
  glVertex3f(llvm_cbe_temp22749, llvm_cbe_size, 0x0p+0);
  glTexCoord2f(0x1p+0, 0x1p+0);
  glVertex3f(llvm_cbe_size, llvm_cbe_size, 0x0p+0);
  glTexCoord2f(0x1p+0, 0x0p+0);
  llvm_cbe_temp22754 = *(&llvm_cbe_nsize);
  glVertex3f(llvm_cbe_size, llvm_cbe_temp22754, 0x0p+0);
  glEnd();
  return;
}


void drawCoordSys(float llvm_cbe_length) {
  unsigned int llvm_cbe_temp22757;

  llvm_cbe_temp22757 = GL_LINES;
  glBegin(llvm_cbe_temp22757);
  glColor4f(0x1p+0, 0x0p+0, 0x0p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x1p+0, 0x0p+0, 0x0p+0);
  glColor4f(0x0p+0, 0x1p+0, 0x0p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x1p+0, 0x0p+0);
  glColor4f(0x0p+0, 0x0p+0, 0x1p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x1p+0);
  glEnd();
  return;
}


void drawOrientationGrid(void) {
  float llvm_cbe_size;    /* Address-exposed local */
  float llvm_cbe_temp22769;
  float llvm_cbe_msize;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22771;
  unsigned int llvm_cbe_d;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22780;
  bool *llvm_cbe__tmp_76_for_testvar;
  bool llvm_cbe_temp22782;
  unsigned int llvm_cbe_temp22783;
  float *llvm_cbe_df;
  float llvm_cbe_temp22786;
  float llvm_cbe_temp22787;
  float llvm_cbe_temp22789;
  float llvm_cbe_temp22790;
  float llvm_cbe_temp22792;
  float llvm_cbe_temp22793;
  float llvm_cbe_temp22795;
  float llvm_cbe_temp22796;
  float llvm_cbe_temp22799;
  float llvm_cbe_temp22800;
  float llvm_cbe_temp22802;
  float llvm_cbe_temp22803;
  float llvm_cbe_temp22805;
  float llvm_cbe_temp22806;
  float llvm_cbe_temp22808;
  float llvm_cbe_temp22809;
  float llvm_cbe_temp22812;
  float llvm_cbe_temp22813;
  float llvm_cbe_temp22815;
  float llvm_cbe_temp22816;
  float llvm_cbe_temp22818;
  float llvm_cbe_temp22819;
  float llvm_cbe_temp22821;
  float llvm_cbe_temp22822;
  unsigned int llvm_cbe_temp22824;

  *(&llvm_cbe_size) = 0x1.4p+3;
  llvm_cbe_temp22769 = *(&llvm_cbe_size);
  *(&llvm_cbe_msize) = (((float )(0x0p+0 - llvm_cbe_temp22769)));
  llvm_cbe_temp22771 = GL_LINES;
  glBegin(llvm_cbe_temp22771);
  glColor3f(0x1p+0, 0x1p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x1p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x1p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x1p+0);
  *(&llvm_cbe_d) = ((unsigned int )-10);
  goto llvm_cbe__tmp_74_for_start;

  do {     /* Syntactic loop '_tmp_74_for_start' to make GCC happy */
llvm_cbe__tmp_74_for_start:
  llvm_cbe_temp22780 = *(&llvm_cbe_d);
  llvm_cbe__tmp_76_for_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_76_for_testvar = (((((signed int )llvm_cbe_temp22780) < ((signed int )((unsigned int )10)))) & 1);
  llvm_cbe_temp22782 = *llvm_cbe__tmp_76_for_testvar;
  if (llvm_cbe_temp22782) {
    goto llvm_cbe__tmp_77_for_body;
  } else {
    goto llvm_cbe__tmp_75_for_exit;
  }

llvm_cbe__tmp_77_for_body:
  llvm_cbe_temp22783 = *(&llvm_cbe_d);
  llvm_cbe_df = (float *) alloca(sizeof(float ));
  *llvm_cbe_df = (((float )(signed int )llvm_cbe_temp22783));
  glColor3f(0x1p+0, 0x0p+0, 0x0p+0);
  llvm_cbe_temp22786 = *(&llvm_cbe_msize);
  llvm_cbe_temp22787 = *llvm_cbe_df;
  glVertex3f(0x0p+0, llvm_cbe_temp22786, llvm_cbe_temp22787);
  llvm_cbe_temp22789 = *(&llvm_cbe_size);
  llvm_cbe_temp22790 = *llvm_cbe_df;
  glVertex3f(0x0p+0, llvm_cbe_temp22789, llvm_cbe_temp22790);
  llvm_cbe_temp22792 = *llvm_cbe_df;
  llvm_cbe_temp22793 = *(&llvm_cbe_msize);
  glVertex3f(0x0p+0, llvm_cbe_temp22792, llvm_cbe_temp22793);
  llvm_cbe_temp22795 = *llvm_cbe_df;
  llvm_cbe_temp22796 = *(&llvm_cbe_size);
  glVertex3f(0x0p+0, llvm_cbe_temp22795, llvm_cbe_temp22796);
  glColor3f(0x0p+0, 0x1p+0, 0x0p+0);
  llvm_cbe_temp22799 = *(&llvm_cbe_size);
  llvm_cbe_temp22800 = *llvm_cbe_df;
  glVertex3f(llvm_cbe_temp22799, 0x0p+0, llvm_cbe_temp22800);
  llvm_cbe_temp22802 = *(&llvm_cbe_msize);
  llvm_cbe_temp22803 = *llvm_cbe_df;
  glVertex3f(llvm_cbe_temp22802, 0x0p+0, llvm_cbe_temp22803);
  llvm_cbe_temp22805 = *llvm_cbe_df;
  llvm_cbe_temp22806 = *(&llvm_cbe_size);
  glVertex3f(llvm_cbe_temp22805, 0x0p+0, llvm_cbe_temp22806);
  llvm_cbe_temp22808 = *llvm_cbe_df;
  llvm_cbe_temp22809 = *(&llvm_cbe_msize);
  glVertex3f(llvm_cbe_temp22808, 0x0p+0, llvm_cbe_temp22809);
  glColor3f(0x0p+0, 0x0p+0, 0x1p+0);
  llvm_cbe_temp22812 = *(&llvm_cbe_size);
  llvm_cbe_temp22813 = *llvm_cbe_df;
  glVertex3f(llvm_cbe_temp22812, llvm_cbe_temp22813, 0x0p+0);
  llvm_cbe_temp22815 = *(&llvm_cbe_msize);
  llvm_cbe_temp22816 = *llvm_cbe_df;
  glVertex3f(llvm_cbe_temp22815, llvm_cbe_temp22816, 0x0p+0);
  llvm_cbe_temp22818 = *llvm_cbe_df;
  llvm_cbe_temp22819 = *(&llvm_cbe_size);
  glVertex3f(llvm_cbe_temp22818, llvm_cbe_temp22819, 0x0p+0);
  llvm_cbe_temp22821 = *llvm_cbe_df;
  llvm_cbe_temp22822 = *(&llvm_cbe_msize);
  glVertex3f(llvm_cbe_temp22821, llvm_cbe_temp22822, 0x0p+0);
  llvm_cbe_temp22824 = *(&llvm_cbe_d);
  *(&llvm_cbe_d) = (llvm_cbe_temp22824 + ((unsigned int )1));
  goto llvm_cbe__tmp_74_for_start;

  } while (1); /* end of syntactic loop '_tmp_74_for_start' */
llvm_cbe__tmp_75_for_exit:
  glEnd();
  glColor3f(0x1p+0, 0x1p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x1p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x1p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x1p+0);
  return;
}


void sendCircle(void) {
  float llvm_cbe_temp22834;
  float llvm_cbe_float_pi;    /* Address-exposed local */
  float llvm_cbe_step;    /* Address-exposed local */
  float llvm_cbe_fangle;    /* Address-exposed local */
  float llvm_cbe_temp22837;
  float llvm_cbe_temp22838;
  float llvm_cbe_temp22839;
  bool *llvm_cbe__tmp_81_while_testvar;
  bool llvm_cbe_temp22843;
  float llvm_cbe_temp22844;
  float llvm_cbe_temp22845;
  float llvm_cbe_temp22846;
  float llvm_cbe_temp22847;
  float llvm_cbe_temp22849;
  float llvm_cbe_temp22850;

  llvm_cbe_temp22834 = acosf(0x0p+0);
  *(&llvm_cbe_float_pi) = (((float )(0x1p+1 * llvm_cbe_temp22834)));
  *(&llvm_cbe_step) = (((float )(0x1p+0 / 0x1.4p+3)));
  *(&llvm_cbe_fangle) = 0x0p+0;
  goto llvm_cbe__tmp_78_while_begin;

  do {     /* Syntactic loop '_tmp_78_while_begin' to make GCC happy */
llvm_cbe__tmp_78_while_begin:
  llvm_cbe_temp22837 = *(&llvm_cbe_fangle);
  llvm_cbe_temp22838 = *(&llvm_cbe_step);
  llvm_cbe_temp22839 = *(&llvm_cbe_float_pi);
  llvm_cbe__tmp_81_while_testvar = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_81_while_testvar = (((llvm_fcmp_ole(llvm_cbe_temp22837, (((float )(llvm_cbe_temp22838 + (((float )(0x1p+1 * llvm_cbe_temp22839))))))))) & 1);
  llvm_cbe_temp22843 = *llvm_cbe__tmp_81_while_testvar;
  if (llvm_cbe_temp22843) {
    goto llvm_cbe__tmp_80_while_body;
  } else {
    goto llvm_cbe__tmp_79_while_exit;
  }

llvm_cbe__tmp_80_while_body:
  llvm_cbe_temp22844 = *(&llvm_cbe_fangle);
  llvm_cbe_temp22845 = sinf(llvm_cbe_temp22844);
  llvm_cbe_temp22846 = *(&llvm_cbe_fangle);
  llvm_cbe_temp22847 = cosf(llvm_cbe_temp22846);
  glVertex3f(llvm_cbe_temp22845, llvm_cbe_temp22847, 0x0p+0);
  llvm_cbe_temp22849 = *(&llvm_cbe_fangle);
  llvm_cbe_temp22850 = *(&llvm_cbe_step);
  *(&llvm_cbe_fangle) = (((float )(llvm_cbe_temp22849 + llvm_cbe_temp22850)));
  goto llvm_cbe__tmp_78_while_begin;

  } while (1); /* end of syntactic loop '_tmp_78_while_begin' */
llvm_cbe__tmp_79_while_exit:
  return;
}


void autoSetupViewport(void) {
  int llvm_cbe_width;    /* Address-exposed local */
  int llvm_cbe_height;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22853;
  unsigned int llvm_cbe_temp22854;

  *(&llvm_cbe_width) = ((unsigned int )0);
  *(&llvm_cbe_height) = ((unsigned int )0);
  glfwGetWindowSize((&llvm_cbe_width), (&llvm_cbe_height));
  llvm_cbe_temp22853 = *(&llvm_cbe_width);
  llvm_cbe_temp22854 = *(&llvm_cbe_height);
  glViewport(((unsigned int )0), ((unsigned int )0), llvm_cbe_temp22853, llvm_cbe_temp22854);
  return;
}


unsigned int makeFramebuffer(void) {
  GLuint llvm_cbe_name;    /* Address-exposed local */
  int llvm_cbe_temp22857;

  *(&llvm_cbe_name) = ((unsigned int )0);
  glGenFramebuffersEXT(((unsigned int )1), (&llvm_cbe_name));
  llvm_cbe_temp22857 = *(&llvm_cbe_name);
  return llvm_cbe_temp22857;
}


unsigned int makeTexture(void) {
  GLuint llvm_cbe_name;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22859;

  *(&llvm_cbe_name) = ((unsigned int )0);
  glGenTextures(((unsigned int )1), (&llvm_cbe_name));
  llvm_cbe_temp22859 = *(&llvm_cbe_name);
  return llvm_cbe_temp22859;
}


unsigned int makeRenderbuffer(void) {
  GLuint llvm_cbe_name;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22861;

  *(&llvm_cbe_name) = ((unsigned int )0);
  glGenRenderbuffersEXT(((unsigned int )1), (&llvm_cbe_name));
  llvm_cbe_temp22861 = *(&llvm_cbe_name);
  return llvm_cbe_temp22861;
}


void glBindTexture2D(unsigned int llvm_cbe_texture) {
  unsigned int llvm_cbe_temp22862;

  llvm_cbe_temp22862 = GL_TEXTURE_2D;
  glBindTexture(llvm_cbe_temp22862, llvm_cbe_texture);
  return;
}


struct l_framebuffer *fbo_make(unsigned int llvm_cbe_width, unsigned int llvm_cbe_height) {
  unsigned char *ltmp_7_1;
  struct l_framebuffer *llvm_cbe_fbo;    /* Address-exposed local */
  struct l_framebuffer *llvm_cbe_temp22866;
  struct l_framebuffer *llvm_cbe_temp22868;
  struct l_framebuffer *llvm_cbe_temp22870;
  struct l_framebuffer *llvm_cbe_temp22872;
  struct l_framebuffer *llvm_cbe_temp22874;
  struct l_framebuffer *llvm_cbe_temp22876;
  struct l_framebuffer *llvm_cbe_temp22878;
  struct l_framebuffer *llvm_cbe_temp22879;

  ltmp_7_1 =  /*tail*/ (unsigned char*) malloc(((unsigned int )28));
  *(&llvm_cbe_fbo) = (((struct l_framebuffer *)ltmp_7_1));
  llvm_cbe_temp22866 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22866->field2) = ((unsigned int )0);
  llvm_cbe_temp22868 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22868->field3) = ((unsigned int )0);
  llvm_cbe_temp22870 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22870->field4) = ((unsigned int )0);
  llvm_cbe_temp22872 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22872->field5) = ((unsigned int )0);
  llvm_cbe_temp22874 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22874->field6) = ((unsigned int )0);
  llvm_cbe_temp22876 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22876->field0) = llvm_cbe_width;
  llvm_cbe_temp22878 = *(&llvm_cbe_fbo);
  *(&llvm_cbe_temp22878->field1) = llvm_cbe_height;
  llvm_cbe_temp22879 = *(&llvm_cbe_fbo);
  return llvm_cbe_temp22879;
}


void fbo_attachColorTexture(struct l_framebuffer *llvm_cbe_fbo) {
  unsigned int llvm_cbe_temp22880;
  unsigned int llvm_cbe_texture;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22881;
  unsigned int llvm_cbe_temp22883;
  unsigned int llvm_cbe_temp22884;
  unsigned int llvm_cbe_temp22885;
  unsigned int llvm_cbe_temp22887;
  unsigned int llvm_cbe_temp22888;
  unsigned int llvm_cbe_temp22889;
  unsigned int llvm_cbe_temp22891;
  unsigned int llvm_cbe_temp22892;
  unsigned int llvm_cbe_temp22894;
  unsigned int llvm_cbe_temp22896;
  unsigned int llvm_cbe_temp22897;
  unsigned int llvm_cbe_temp22898;
  unsigned int llvm_cbe_temp22902;

  llvm_cbe_temp22880 = makeTexture();
  *(&llvm_cbe_texture) = llvm_cbe_temp22880;
  llvm_cbe_temp22881 = *(&llvm_cbe_texture);
  glBindTexture2D(llvm_cbe_temp22881);
  llvm_cbe_temp22883 = GL_TEXTURE_2D;
  llvm_cbe_temp22884 = GL_TEXTURE_MIN_FILTER;
  llvm_cbe_temp22885 = GL_LINEAR;
  glTexParameteri(llvm_cbe_temp22883, llvm_cbe_temp22884, llvm_cbe_temp22885);
  llvm_cbe_temp22887 = GL_TEXTURE_2D;
  llvm_cbe_temp22888 = GL_TEXTURE_MAG_FILTER;
  llvm_cbe_temp22889 = GL_LINEAR;
  glTexParameteri(llvm_cbe_temp22887, llvm_cbe_temp22888, llvm_cbe_temp22889);
  llvm_cbe_temp22891 = GL_TEXTURE_2D;
  llvm_cbe_temp22892 = GL_RGBA;
  llvm_cbe_temp22894 = *(&llvm_cbe_fbo->field0);
  llvm_cbe_temp22896 = *(&llvm_cbe_fbo->field1);
  llvm_cbe_temp22897 = GL_RGBA;
  llvm_cbe_temp22898 = GL_UNSIGNED_BYTE;
  glTexImage2D(llvm_cbe_temp22891, ((unsigned int )0), llvm_cbe_temp22892, llvm_cbe_temp22894, llvm_cbe_temp22896, ((unsigned int )0), llvm_cbe_temp22897, llvm_cbe_temp22898, (((unsigned char *)((unsigned char *)/*NULL*/0))));
  llvm_cbe_temp22902 = *(&llvm_cbe_texture);
  *(&llvm_cbe_fbo->field3) = llvm_cbe_temp22902;
  return;
}


void fbo_create(struct l_framebuffer *llvm_cbe_fbo) {
  unsigned int llvm_cbe_temp22903;
  unsigned int llvm_cbe_fboName;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22904;
  unsigned int llvm_cbe_temp22905;
  unsigned int llvm_cbe_temp22907;
  unsigned int llvm_cbe_depthBuffer;    /* Address-exposed local */
  unsigned int llvm_cbe_temp22908;
  unsigned int llvm_cbe_temp22909;
  unsigned int llvm_cbe_temp22911;
  unsigned int llvm_cbe_temp22912;
  unsigned int llvm_cbe_temp22914;
  unsigned int llvm_cbe_temp22916;
  unsigned int llvm_cbe_temp22918;
  unsigned int llvm_cbe_temp22919;
  unsigned int llvm_cbe_temp22920;
  unsigned int llvm_cbe_temp22921;
  unsigned int llvm_cbe_temp22923;
  unsigned int llvm_cbe_temp22924;
  unsigned int llvm_cbe_temp22925;
  unsigned int llvm_cbe_temp22927;
  unsigned int llvm_cbe_temp22930;
  unsigned int llvm_cbe_temp22932;

  llvm_cbe_temp22903 = makeFramebuffer();
  *(&llvm_cbe_fboName) = llvm_cbe_temp22903;
  llvm_cbe_temp22904 = GL_FRAMEBUFFER_EXT;
  llvm_cbe_temp22905 = *(&llvm_cbe_fboName);
  glBindFramebufferEXT(llvm_cbe_temp22904, llvm_cbe_temp22905);
  llvm_cbe_temp22907 = makeRenderbuffer();
  *(&llvm_cbe_depthBuffer) = llvm_cbe_temp22907;
  llvm_cbe_temp22908 = GL_RENDERBUFFER_EXT;
  llvm_cbe_temp22909 = *(&llvm_cbe_depthBuffer);
  glBindRenderbufferEXT(llvm_cbe_temp22908, llvm_cbe_temp22909);
  llvm_cbe_temp22911 = GL_RENDERBUFFER_EXT;
  llvm_cbe_temp22912 = GL_DEPTH_COMPONENT24;
  llvm_cbe_temp22914 = *(&llvm_cbe_fbo->field0);
  llvm_cbe_temp22916 = *(&llvm_cbe_fbo->field1);
  glRenderbufferStorageEXT(llvm_cbe_temp22911, llvm_cbe_temp22912, llvm_cbe_temp22914, llvm_cbe_temp22916);
  llvm_cbe_temp22918 = GL_FRAMEBUFFER_EXT;
  llvm_cbe_temp22919 = GL_DEPTH_ATTACHMENT_EXT;
  llvm_cbe_temp22920 = GL_RENDERBUFFER_EXT;
  llvm_cbe_temp22921 = *(&llvm_cbe_depthBuffer);
  glFramebufferRenderbufferEXT(llvm_cbe_temp22918, llvm_cbe_temp22919, llvm_cbe_temp22920, llvm_cbe_temp22921);
  llvm_cbe_temp22923 = GL_FRAMEBUFFER_EXT;
  llvm_cbe_temp22924 = GL_COLOR_ATTACHMENT0_EXT;
  llvm_cbe_temp22925 = GL_TEXTURE_2D;
  llvm_cbe_temp22927 = *(&llvm_cbe_fbo->field3);
  glFramebufferTexture2DEXT(llvm_cbe_temp22923, llvm_cbe_temp22924, llvm_cbe_temp22925, llvm_cbe_temp22927, ((unsigned int )0));
  llvm_cbe_temp22930 = *(&llvm_cbe_fboName);
  *(&llvm_cbe_fbo->field2) = llvm_cbe_temp22930;
  llvm_cbe_temp22932 = *(&llvm_cbe_depthBuffer);
  *(&llvm_cbe_fbo->field6) = llvm_cbe_temp22932;
  return;
}


void fbo_bind(struct l_framebuffer *llvm_cbe_fbo) {
  unsigned int llvm_cbe_temp22933;
  unsigned int llvm_cbe_temp22935;
  unsigned int llvm_cbe_temp22938;
  unsigned int llvm_cbe_temp22940;

  llvm_cbe_temp22933 = GL_FRAMEBUFFER_EXT;
  llvm_cbe_temp22935 = *(&llvm_cbe_fbo->field2);
  glBindFramebufferEXT(llvm_cbe_temp22933, llvm_cbe_temp22935);
  llvm_cbe_temp22938 = *(&llvm_cbe_fbo->field0);
  llvm_cbe_temp22940 = *(&llvm_cbe_fbo->field1);
  glViewport(((unsigned int )0), ((unsigned int )0), llvm_cbe_temp22938, llvm_cbe_temp22940);
  return;
}


void fbo_unbind(struct l_framebuffer *llvm_cbe_fbo) {
  unsigned int llvm_cbe_temp22942;

  llvm_cbe_temp22942 = GL_FRAMEBUFFER_EXT;
  glBindFramebufferEXT(llvm_cbe_temp22942, ((unsigned int )0));
  return;
}


void fbo_makeColorTexture(struct l_framebuffer *llvm_cbe_fbo) {
  unsigned int llvm_cbe_temp22945;
  unsigned int llvm_cbe_temp22947;

  llvm_cbe_temp22945 = *(&llvm_cbe_fbo->field3);
  glBindTexture2D(llvm_cbe_temp22945);
  llvm_cbe_temp22947 = GL_TEXTURE_2D;
  glGenerateMipmapEXT(llvm_cbe_temp22947);
  glBindTexture2D(((unsigned int )0));
  return;
}


bool isPressed(unsigned int llvm_cbe_key) {
  unsigned int llvm_cbe_temp22950;
  unsigned int llvm_cbe_temp22951;

  llvm_cbe_temp22950 = glfwGetKey(llvm_cbe_key);
  llvm_cbe_temp22951 = GL_TRUE;
  return (llvm_cbe_temp22950 == llvm_cbe_temp22951);
}


bool init(void) {
  bool llvm_cbe_temp22953;
  bool llvm_cbe__tmp_85_iftestresult;    /* Address-exposed local */
  bool llvm_cbe_temp22954;
  unsigned int llvm_cbe_temp22955;
  unsigned int llvm_cbe_temp22956;
  bool *llvm_cbe__tmp_89_iftestresult;
  bool llvm_cbe_temp22958;
  unsigned char *llvm_cbe_temp22959;
  unsigned char *llvm_cbe_temp22961;
  struct l_ast *llvm_cbe_temp22962;
  unsigned int llvm_cbe_temp22964;
  unsigned int llvm_cbe_temp22965;
  unsigned int llvm_cbe_temp22966;
  unsigned int llvm_cbe_temp22967;
  unsigned int llvm_cbe_temp22968;
  bool *llvm_cbe__tmp_93_iftestresult;
  bool llvm_cbe_temp22970;
  unsigned char *llvm_cbe_temp22971;
  unsigned char *llvm_cbe_temp22973;
  struct l_ast *llvm_cbe_temp22974;
  struct l_ast **llvm_cbe___temp_461;
  struct l_ast *llvm_cbe_temp22975;
  unsigned char *llvm_cbe_temp22976;
  struct l_ast *llvm_cbe_temp22977;
  struct l_ast *llvm_cbe_temp22979;
  unsigned char *llvm_cbe_temp22980;
  struct l_ast *llvm_cbe_temp22981;
  struct l_ast *llvm_cbe_temp22983;
  unsigned char *llvm_cbe_temp22984;
  struct l_ast *llvm_cbe_temp22985;
  struct l_ast *llvm_cbe_temp22987;
  unsigned char *llvm_cbe_temp22988;
  struct l_ast *llvm_cbe_temp22989;
  struct l_ast *llvm_cbe_temp22991;
  unsigned char *llvm_cbe_temp22992;
  struct l_ast *llvm_cbe_temp22993;
  struct l_ast *llvm_cbe_temp22995;
  unsigned char *llvm_cbe_temp22996;
  struct l_ast *llvm_cbe_temp22997;
  struct l_ast *llvm_cbe_temp22999;
  unsigned char *llvm_cbe_temp23000;
  struct l_ast *llvm_cbe_temp23001;
  struct l_ast *llvm_cbe_temp23003;
  unsigned char *llvm_cbe_temp23004;
  struct l_ast *llvm_cbe_temp23005;
  struct l_ast *llvm_cbe_temp23007;
  unsigned char *llvm_cbe_temp23008;
  struct l_ast *llvm_cbe_temp23009;
  struct l_ast *llvm_cbe_temp23011;

  llvm_cbe_temp22953 = *(&initialized);
  *(&llvm_cbe__tmp_85_iftestresult) = ((llvm_cbe_temp22953) & 1);
  llvm_cbe_temp22954 = *(&llvm_cbe__tmp_85_iftestresult);
  if (llvm_cbe_temp22954) {
    goto llvm_cbe__tmp_82_iftrue;
  } else {
    goto llvm_cbe__tmp_84_ifend;
  }

llvm_cbe__tmp_82_iftrue:
  return 1;
llvm_cbe__tmp_84_ifend:
  llvm_cbe_temp22955 = GL_TRUE;
  llvm_cbe_temp22956 = glfwInit();
  llvm_cbe__tmp_89_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_89_iftestresult = (((llvm_cbe_temp22955 != llvm_cbe_temp22956)) & 1);
  llvm_cbe_temp22958 = *llvm_cbe__tmp_89_iftestresult;
  if (llvm_cbe_temp22958) {
    goto llvm_cbe__tmp_86_iftrue;
  } else {
    goto llvm_cbe__tmp_88_ifend;
  }

llvm_cbe__tmp_86_iftrue:
  llvm_cbe_temp22959 = *(&tempvar_4461);
  printlnString(llvm_cbe_temp22959);
  llvm_cbe_temp22961 = *(&tempvar_4462);
  llvm_cbe_temp22962 = simpleAst(llvm_cbe_temp22961);
  printAst(llvm_cbe_temp22962, ((unsigned int )0));
  return 0;
llvm_cbe__tmp_88_ifend:
  llvm_cbe_temp22964 = GL_TRUE;
  llvm_cbe_temp22965 = *(&windowWidth);
  llvm_cbe_temp22966 = *(&windowHeight);
  llvm_cbe_temp22967 = GLFW_WINDOW;
  llvm_cbe_temp22968 = glfwOpenWindow(llvm_cbe_temp22965, llvm_cbe_temp22966, ((unsigned int )8), ((unsigned int )8), ((unsigned int )8), ((unsigned int )8), ((unsigned int )16), ((unsigned int )0), llvm_cbe_temp22967);
  llvm_cbe__tmp_93_iftestresult = (bool *) alloca(sizeof(bool ));
  *llvm_cbe__tmp_93_iftestresult = (((llvm_cbe_temp22964 != llvm_cbe_temp22968)) & 1);
  llvm_cbe_temp22970 = *llvm_cbe__tmp_93_iftestresult;
  if (llvm_cbe_temp22970) {
    goto llvm_cbe__tmp_90_iftrue;
  } else {
    goto llvm_cbe__tmp_92_ifend;
  }

llvm_cbe__tmp_90_iftrue:
  llvm_cbe_temp22971 = *(&tempvar_4504);
  printlnString(llvm_cbe_temp22971);
  llvm_cbe_temp22973 = *(&tempvar_4505);
  llvm_cbe_temp22974 = simpleAst(llvm_cbe_temp22973);
  llvm_cbe___temp_461 = (struct l_ast **) alloca(sizeof(struct l_ast *));
  *llvm_cbe___temp_461 = llvm_cbe_temp22974;
  llvm_cbe_temp22975 = *llvm_cbe___temp_461;
  llvm_cbe_temp22976 = *(&tempvar_4506);
  llvm_cbe_temp22977 = simpleAst(llvm_cbe_temp22976);
  addChild(llvm_cbe_temp22975, llvm_cbe_temp22977);
  llvm_cbe_temp22979 = *llvm_cbe___temp_461;
  llvm_cbe_temp22980 = *(&tempvar_4507);
  llvm_cbe_temp22981 = simpleAst(llvm_cbe_temp22980);
  addChild(llvm_cbe_temp22979, llvm_cbe_temp22981);
  llvm_cbe_temp22983 = *llvm_cbe___temp_461;
  llvm_cbe_temp22984 = *(&tempvar_4508);
  llvm_cbe_temp22985 = simpleAst(llvm_cbe_temp22984);
  addChild(llvm_cbe_temp22983, llvm_cbe_temp22985);
  llvm_cbe_temp22987 = *llvm_cbe___temp_461;
  llvm_cbe_temp22988 = *(&tempvar_4509);
  llvm_cbe_temp22989 = simpleAst(llvm_cbe_temp22988);
  addChild(llvm_cbe_temp22987, llvm_cbe_temp22989);
  llvm_cbe_temp22991 = *llvm_cbe___temp_461;
  llvm_cbe_temp22992 = *(&tempvar_4510);
  llvm_cbe_temp22993 = simpleAst(llvm_cbe_temp22992);
  addChild(llvm_cbe_temp22991, llvm_cbe_temp22993);
  llvm_cbe_temp22995 = *llvm_cbe___temp_461;
  llvm_cbe_temp22996 = *(&tempvar_4511);
  llvm_cbe_temp22997 = simpleAst(llvm_cbe_temp22996);
  addChild(llvm_cbe_temp22995, llvm_cbe_temp22997);
  llvm_cbe_temp22999 = *llvm_cbe___temp_461;
  llvm_cbe_temp23000 = *(&tempvar_4512);
  llvm_cbe_temp23001 = simpleAst(llvm_cbe_temp23000);
  addChild(llvm_cbe_temp22999, llvm_cbe_temp23001);
  llvm_cbe_temp23003 = *llvm_cbe___temp_461;
  llvm_cbe_temp23004 = *(&tempvar_4513);
  llvm_cbe_temp23005 = simpleAst(llvm_cbe_temp23004);
  addChild(llvm_cbe_temp23003, llvm_cbe_temp23005);
  llvm_cbe_temp23007 = *llvm_cbe___temp_461;
  llvm_cbe_temp23008 = *(&tempvar_4514);
  llvm_cbe_temp23009 = simpleAst(llvm_cbe_temp23008);
  addChild(llvm_cbe_temp23007, llvm_cbe_temp23009);
  llvm_cbe_temp23011 = *llvm_cbe___temp_461;
  printAst(llvm_cbe_temp23011, ((unsigned int )0));
  return 0;
llvm_cbe__tmp_92_ifend:
  glewInit();
  *(&initialized) = ((1) & 1);
  return 1;
}


unsigned int makeBillboardGShader(void) {
  unsigned char *llvm_cbe_temp23014;
  unsigned char *llvm_cbe_temp23015;
  unsigned int llvm_cbe_temp23016;
  unsigned int llvm_cbe_shader;    /* Address-exposed local */
  unsigned int llvm_cbe_temp23017;

  llvm_cbe_temp23014 = *(&tempvar_4515);
  llvm_cbe_temp23015 = *(&tempvar_4516);
  llvm_cbe_temp23016 = createShader(llvm_cbe_temp23014, llvm_cbe_temp23015);
  *(&llvm_cbe_shader) = llvm_cbe_temp23016;
  llvm_cbe_temp23017 = *(&llvm_cbe_shader);
  return llvm_cbe_temp23017;
}


unsigned int makeBillboard(unsigned int llvm_cbe_width, unsigned int llvm_cbe_height) {
  struct l_framebuffer *llvm_cbe_temp23018;
  struct l_framebuffer *llvm_cbe_fbo;    /* Address-exposed local */
  struct l_framebuffer *llvm_cbe_temp23019;
  struct l_framebuffer *llvm_cbe_temp23021;
  struct l_framebuffer *llvm_cbe_temp23023;
  unsigned int llvm_cbe_temp23025;
  unsigned int llvm_cbe_shader;    /* Address-exposed local */
  unsigned int llvm_cbe_temp23026;
  unsigned int llvm_cbe_temp23029;
  unsigned int llvm_cbe_temp23032;
  unsigned int llvm_cbe_temp23036;
  unsigned int llvm_cbe_temp23037;
  unsigned int llvm_cbe_temp23040;
  unsigned int llvm_cbe_temp23044;
  unsigned int llvm_cbe_temp23048;
  unsigned int llvm_cbe_temp23050;
  unsigned int llvm_cbe_temp23057;
  unsigned int llvm_cbe_temp23060;
  unsigned int llvm_cbe_temp23063;
  unsigned int llvm_cbe_temp23066;
  struct l_framebuffer *llvm_cbe_temp23068;
  struct l_framebuffer *llvm_cbe_temp23070;
  struct l_framebuffer *llvm_cbe_temp23073;
  unsigned int llvm_cbe_temp23074;

  llvm_cbe_temp23018 = fbo_make(llvm_cbe_width, llvm_cbe_height);
  *(&llvm_cbe_fbo) = llvm_cbe_temp23018;
  llvm_cbe_temp23019 = *(&llvm_cbe_fbo);
  fbo_attachColorTexture(llvm_cbe_temp23019);
  llvm_cbe_temp23021 = *(&llvm_cbe_fbo);
  fbo_create(llvm_cbe_temp23021);
  llvm_cbe_temp23023 = *(&llvm_cbe_fbo);
  fbo_bind(llvm_cbe_temp23023);
  llvm_cbe_temp23025 = makeBillboardGShader();
  *(&llvm_cbe_shader) = llvm_cbe_temp23025;
  llvm_cbe_temp23026 = GL_TEXTURE;
  glMatrixMode(llvm_cbe_temp23026);
  glPushMatrix();
  llvm_cbe_temp23029 = GL_PROJECTION;
  glMatrixMode(llvm_cbe_temp23029);
  glPushMatrix();
  llvm_cbe_temp23032 = GL_MODELVIEW;
  glMatrixMode(llvm_cbe_temp23032);
  glPushMatrix();
  glClearColor(0x0p+0, 0x1p+0, 0x0p+0, 0x0p+0);
  llvm_cbe_temp23036 = GL_COLOR_BUFFER_BIT;
  llvm_cbe_temp23037 = GL_DEPTH_BUFFER_BIT;
  glClear((llvm_cbe_temp23036 | llvm_cbe_temp23037));
  llvm_cbe_temp23040 = GL_PROJECTION;
  glMatrixMode(llvm_cbe_temp23040);
  glLoadIdentity();
  gluPerspective(0x1.68p+6, 0x1p+0, 0x1p+0, 0x1.9p+6);
  llvm_cbe_temp23044 = GL_MODELVIEW;
  glMatrixMode(llvm_cbe_temp23044);
  glLoadIdentity();
  gluLookAt(0x0p+0, 0x0p+0, 0x1p+0, 0x0p+0, 0x0p+0, 0x0p+0, 0x0p+0, 0x1p+0, 0x0p+0);
  llvm_cbe_temp23048 = *(&llvm_cbe_shader);
  glUseProgram(llvm_cbe_temp23048);
  llvm_cbe_temp23050 = GL_TRIANGLE_FAN;
  glBegin(llvm_cbe_temp23050);
  glColor4f(0x1p+0, 0x0p+0, 0x0p+0, 0x1p+0);
  glVertex3f(0x0p+0, 0x0p+0, 0x0p+0);
  sendCircle();
  glEnd();
  glUseProgram(((unsigned int )0));
  llvm_cbe_temp23057 = GL_TEXTURE;
  glMatrixMode(llvm_cbe_temp23057);
  glPopMatrix();
  llvm_cbe_temp23060 = GL_PROJECTION;
  glMatrixMode(llvm_cbe_temp23060);
  glPopMatrix();
  llvm_cbe_temp23063 = GL_MODELVIEW;
  glMatrixMode(llvm_cbe_temp23063);
  glPopMatrix();
  llvm_cbe_temp23066 = *(&llvm_cbe_shader);
  glDeleteProgram(llvm_cbe_temp23066);
  llvm_cbe_temp23068 = *(&llvm_cbe_fbo);
  fbo_makeColorTexture(llvm_cbe_temp23068);
  llvm_cbe_temp23070 = *(&llvm_cbe_fbo);
  fbo_unbind(llvm_cbe_temp23070);
  llvm_cbe_temp23073 = *(&llvm_cbe_fbo);
  llvm_cbe_temp23074 = *(&llvm_cbe_temp23073->field3);
  return llvm_cbe_temp23074;
}


unsigned int makeMainShader(void) {
  unsigned char *llvm_cbe_temp23075;
  unsigned char *llvm_cbe_temp23076;
  unsigned int llvm_cbe_temp23077;

  llvm_cbe_temp23075 = *(&tempvar_4579);
  llvm_cbe_temp23076 = *(&tempvar_4580);
  llvm_cbe_temp23077 = createShader(llvm_cbe_temp23075, llvm_cbe_temp23076);
  return llvm_cbe_temp23077;
}


unsigned int random(unsigned int llvm_cbe_max) {
  unsigned int llvm_cbe_temp23078;

  llvm_cbe_temp23078 = rand();
  return (((signed int )(((signed int )llvm_cbe_temp23078) % ((signed int )llvm_cbe_max))));
}


void long_(void) {
  *(&autoTimeout) = 0x1.f4p+9;
  test();
  return;
}


void short_(void) {
  *(&autoTimeout) = 0x1p+0;
  test();
  return;
}

