declare void @printInt(i32)
define i32 @main() {
entry:
	%b = alloca i32
	%a = alloca i32
	%0 = sub i32 24, 19
	call void @printInt(i32 %0)
	%1 = sdiv i32 18, 21
	%2 = sub i32 29, %1
	call void @printInt(i32 %2)
	%3 = add i32 37, 59
	%4 = sub i32 18, 60
	%5 = mul i32 %3, %4
	store i32 %5, i32* %a
	store i32 43, i32* %b
	%6 = load i32, i32* %a
	call void @printInt(i32 %6)
	%7 = load i32, i32* %b
	%8 = sdiv i32 100, %7
	call void @printInt(i32 %8)
	ret i32 0
}
