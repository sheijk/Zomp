; ModuleID = 'simple.bc'
target datalayout = "E-p:32:32"
target endian = big
target pointersize = 32
target triple = "powerpc-apple-darwin8.7.0"

%message = internal constant [13 x sbyte] c"hello, llvm\0A\00"
%result = internal constant int 7

implementation   ; Functions:

int %five() {
    %tmp = alloca int
    %tmp = load int* %result
    ret int %tmp
}

int %main() {
	%tmp = alloca int, align 4
; 	"alloca point" = cast int 0 to int
	%tmp = getelementptr [13 x sbyte]* %message, int 0, uint 0
	call int (sbyte*, ...)* %printf( sbyte* %tmp )
    
    %retval = alloca int
    %retval = call int()* %five()
    ret int %retval
}


declare int %printf(sbyte*, ...)

