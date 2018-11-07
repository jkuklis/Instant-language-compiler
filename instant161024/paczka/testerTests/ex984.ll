declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	call void @printInt(i32 42)
	store i32 30, i32* %a
	call void @printInt(i32 74)
	%0 = load i32, i32* %a
	%1 = sdiv i32 36, %0
	call void @printInt(i32 %1)
	%2 = load i32, i32* %a
	call void @printInt(i32 %2)
	call void @printInt(i32 43)
	%3 = load i32, i32* %a
	store i32 %3, i32* %b
	%4 = load i32, i32* %b
	store i32 %4, i32* %c
	%5 = load i32, i32* %a
	%6 = load i32, i32* %b
	%7 = mul i32 %5, %6
	call void @printInt(i32 %7)
	call void @printInt(i32 36)
	call void @printInt(i32 10)
	%8 = load i32, i32* %a
	call void @printInt(i32 %8)
	call void @printInt(i32 24)
	%9 = load i32, i32* %a
	%10 = load i32, i32* %b
	%11 = load i32, i32* %c
	%12 = mul i32 %10, %11
	%13 = sub i32 %9, %12
	call void @printInt(i32 %13)
	call void @printInt(i32 84)
	call void @printInt(i32 51)
	ret i32 0
}
