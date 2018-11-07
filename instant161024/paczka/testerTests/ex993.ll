declare void @printInt(i32)
define i32 @main() {
entry:
	call void @printInt(i32 15)
	call void @printInt(i32 34)
	call void @printInt(i32 34)
	ret i32 0
}
