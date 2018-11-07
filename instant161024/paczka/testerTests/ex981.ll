declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	call void @printInt(i32 29)
	call void @printInt(i32 15)
	call void @printInt(i32 7)
	%0 = add i32 6, 43
	call void @printInt(i32 %0)
	call void @printInt(i32 46)
	call void @printInt(i32 31)
	store i32 70, i32* %a
	call void @printInt(i32 27)
	%1 = load i32, i32* %a
	%2 = add i32 %1, 27
	%3 = mul i32 %2, 7
	call void @printInt(i32 %3)
	call void @printInt(i32 73)
	ret i32 0
}
