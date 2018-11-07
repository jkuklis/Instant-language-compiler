declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%0 = mul i32 13, 9
	%1 = mul i32 %0, 21
	%2 = mul i32 %1, 42
	store i32 %2, i32* %a
	call void @printInt(i32 12)
	%3 = load i32, i32* %a
	call void @printInt(i32 %3)
	%4 = load i32, i32* %a
	call void @printInt(i32 %4)
	call void @printInt(i32 19)
	ret i32 0
}
