declare void @printInt(i32)
define i32 @main() {
entry: 
       %i1 = add i32 2, 2
       call void @printInt(i32 %i1)
       ret i32 0
}
