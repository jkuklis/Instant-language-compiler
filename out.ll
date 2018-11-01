declare void @printInt(i64)
define i32 @main() {
entry:
	%0 = add i64 3, 4
	call void @printInt(i64 %0)
	%1 = add i64 4, 2
	call void @printInt(i64 %1)
	ret i32 0
}
