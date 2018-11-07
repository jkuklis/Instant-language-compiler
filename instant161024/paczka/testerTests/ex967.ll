declare void @printInt(i32)
define i32 @main() {
entry:
	call void @printInt(i32 29)
	%0 = add i32 32, 27
	%1 = sdiv i32 49, 61
	%2 = sub i32 %0, %1
	call void @printInt(i32 %2)
	ret i32 0
}
