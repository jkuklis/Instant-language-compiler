declare void @printInt(i32)
define i32 @main() {
entry:
	call void @printInt(i32 18)
	ret i32 0
}
