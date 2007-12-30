

define i32 @main() {
entry:
  %foo_ptr = alloca i1
  br label %start
  
start:
  store i1 true, i1* %foo_ptr
  %goon = load i1* %foo_ptr
  br i1 %goon, label %start, label %start
  
  ret i32 4
}


