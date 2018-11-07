declare void @printInt(i32)
define i32 @main() {
entry:
	call void @printInt(i32 2)
	%0 = sdiv i32 30, 20
	%1 = add i32 3, %0
	call void @printInt(i32 %1)
	ret i32 0
}
