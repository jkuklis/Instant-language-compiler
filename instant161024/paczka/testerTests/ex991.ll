declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	call void @printInt(i32 11)
	store i32 24, i32* %a
	%0 = load i32, i32* %a
	%1 = load i32, i32* %a
	%2 = sub i32 %0, %1
	call void @printInt(i32 %2)
	store i32 46, i32* %b
	call void @printInt(i32 42)
	%3 = sub i32 13, 100
	%4 = load i32, i32* %a
	%5 = sub i32 32, %4
	%6 = sub i32 %3, %5
	%7 = sub i32 56, %6
	%8 = load i32, i32* %b
	%9 = sdiv i32 %7, %8
	call void @printInt(i32 %9)
	%10 = load i32, i32* %a
	call void @printInt(i32 %10)
	%11 = load i32, i32* %b
	call void @printInt(i32 %11)
	call void @printInt(i32 17)
	ret i32 0
}
