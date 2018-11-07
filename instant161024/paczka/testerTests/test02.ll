declare void @printInt(i32)
define i32 @main() {
entry:
	%0 = sub i32 44, 2
	call void @printInt(i32 %0)
	ret i32 0
}
