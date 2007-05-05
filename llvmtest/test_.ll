; ModuleID = 'makecode.bc'
target datalayout = "E-p:32:32"
target endian = big
target pointersize = 32
target triple = "powerpc-apple-darwin8.7.0"
%str = internal constant [13 x sbyte] c"hello, llvm\0A\00"		; <[13 x sbyte]*> [#uses=1]

implementation   ; Functions:

int %foo() {
    ret int 200
}

int %main(int %argc, sbyte** %argv) {
entry:
	%tmp = alloca int, align 4		; <int*> [#uses=2]
	"alloca point" = cast int 0 to int		; <int> [#uses=0]
	%tmp = getelementptr [13 x sbyte]* %str, int 0, uint 0		; <sbyte*> [#uses=1]
	call int (sbyte*, ...)* %printf( sbyte* %tmp )		; <int> [#uses=0]
	br label %return
    
return:		; preds = %entry
	%f = alloca int, align 4
	%f = call int ()* %foo()
	ret int %f
}

declare int %printf(sbyte*, ...)
