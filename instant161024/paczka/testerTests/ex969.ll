declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%0 = sub i32 30, 11
	call void @printInt(i32 %0)
	store i32 2, i32* %a
	%1 = load i32, i32* %a
	call void @printInt(i32 %1)
	ret i32 0
}
