declare void @printInt(i32)
define i32 @main() {
entry: 
       %i1 = add i32 2, 2
       %i2 = mul i32 3, %i1
       call void @printInt(i32 %i2)
       ret i32 0
}
