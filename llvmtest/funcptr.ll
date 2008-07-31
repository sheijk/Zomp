; ModuleID = 'funcptr.bc'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-s0:0:64-f80:128:128"
target triple = "i686-apple-darwin8"

define i32 @apply(i32 (i32)* %f, i32 %arg) {
entry:
	%f_addr = alloca i32 (i32)*		; <i32 (i32)**> [#uses=2]
	%arg_addr = alloca i32		; <i32*> [#uses=2]
	%retval = alloca i32, align 4		; <i32*> [#uses=2]
	%tmp = alloca i32, align 4		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 (i32)* %f, i32 (i32)** %f_addr
	store i32 %arg, i32* %arg_addr
	%tmp1 = load i32 (i32)** %f_addr, align 4		; <i32 (i32)*> [#uses=1]
	%tmp2 = load i32* %arg_addr, align 4		; <i32> [#uses=1]
	%tmp3 = call i32 %tmp1( i32 %tmp2 )		; <i32> [#uses=1]
	store i32 %tmp3, i32* %tmp, align 4
	%tmp4 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp4, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval5 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval5
}

define i32 @negate(i32 %i) {
entry:
	%i_addr = alloca i32		; <i32*> [#uses=2]
	%retval = alloca i32, align 4		; <i32*> [#uses=2]
	%tmp = alloca i32, align 4		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 %i, i32* %i_addr
	%tmp1 = load i32* %i_addr, align 4		; <i32> [#uses=1]
	%tmp2 = sub i32 0, %tmp1		; <i32> [#uses=1]
	store i32 %tmp2, i32* %tmp, align 4
	%tmp3 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp3, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval4 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval4
}

define i32 @main() {
entry:
	%retval = alloca i32, align 4		; <i32*> [#uses=2]
	%tmp = alloca i32, align 4		; <i32*> [#uses=2]
	%func = alloca i32 (i32)*, align 4		; <i32 (i32)**> [#uses=2]
	%i = alloca i32, align 4		; <i32*> [#uses=1]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 (i32)* @negate, i32 (i32)** %func, align 4
	%tmp1 = load i32 (i32)** %func, align 4		; <i32 (i32)*> [#uses=1]
	%tmp2 = call i32 @apply( i32 (i32)* %tmp1, i32 10 )		; <i32> [#uses=1]
	store i32 %tmp2, i32* %i, align 4
	store i32 0, i32* %tmp, align 4
	%tmp3 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp3, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval4 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval4
}
