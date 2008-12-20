
target datalayout = "E-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64"
target triple = "powerpc-apple-darwin8"

%struct.foo_t = type { i32, float }

define void @createfoo(%struct.foo_t* %out) {
  ; initializing:
  %foo.1_p = getelementptr %struct.foo_t* %out, i32 0, i32 0
  store i32 0, i32* %foo.1_p
  %foo.2_p = getelementptr %struct.foo_t* %out, i32 0, i32 1
  store float 0.0, float* %foo.2_p

  ; accessing elements:
  %ptr = getelementptr %struct.foo_t* %out, i32 0, i32 0
  %val = load i32* %ptr
  add i32 1, %val
  ret void
}

define void @testfoo() {
  %tmp = alloca %struct.foo_t
  call void @createfoo(%struct.foo_t* %tmp)
  ret void
}

define void @"test"() {
  ; allocating var p : (x :float, y :float, z :float)/{ float, float, float} on stack
  %"p" = alloca { float, float, float}
  ; allocating var pa : (x :float, y :float, z :float)*/{ float, float, float}* on stack
  %"pa" = alloca { float, float, float}*
  ; assigning new value to %"pa"
  store { float, float, float}* %"p", { float, float, float}** %"pa"
  ; calling function printFloat(float %"temp7178")
  ; loading float*
  ; obtaining address of %"p".x
  %"temp7177" = getelementptr { float, float, float}* %"p", i32 0, i32 0
  %"temp7178" = load float* %"temp7177"
  ; call void (float)* @"printFloat"(float %"temp7178")
  ret void
}

; define void @argument(%struct.foo_t %p) {
; ;   ; %x_addr = getelementptr {float,float,float} %p, i32 0
;   ret void
; }

