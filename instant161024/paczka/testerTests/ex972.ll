declare void @printInt(i32)
define i32 @main() {
entry:
	call void @printInt(i32 10)
	call void @printInt(i32 28)
	ret i32 0
}
