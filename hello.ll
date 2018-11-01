@hellostr = internal constant [5 x i8] c"Hello"

declare void @printInt(i32)
declare void @printString(i8*)
define i32 @main() {
entry: 
;       %t0 = getelementptr [5 x i8]* @hellostr, i32 0
       %t0 = bitcast [5 x i8]* @hellostr to i8*
       call void @printString(i8* %t0)
       ret i32 0
}
