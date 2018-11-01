@.str = private constant [14 x i8] c"Hello world!\0A\00"

define void @print7(i8* %a) {
  %x = getelementptr i8* %a, i32 6
  %r = call i32 @puts(i8* %x)
  ret void
}

declare i32 @puts(i8*)

define i32 @main() {
  %x = getelementptr [14 x i8]* @.str, i32 0, i32 0
  call void @print7(i8* %x)
  ret i32 0
}
