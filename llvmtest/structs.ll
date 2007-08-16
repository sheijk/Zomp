
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

