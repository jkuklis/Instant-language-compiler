declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%0 = mul i32 3, 12
	%1 = sdiv i32 46, %0
	call void @printInt(i32 %1)
	call void @printInt(i32 73)
	%2 = add i32 43, 40
	call void @printInt(i32 %2)
	%3 = sdiv i32 3, 20
	call void @printInt(i32 %3)
	call void @printInt(i32 49)
	call void @printInt(i32 12)
	call void @printInt(i32 41)
	%4 = mul i32 26, 35
	call void @printInt(i32 %4)
	store i32 42, i32* %a
	store i32 1, i32* %b
	%5 = load i32, i32* %a
	call void @printInt(i32 %5)
	%6 = load i32, i32* %a
	%7 = load i32, i32* %a
	%8 = add i32 %6, %7
	call void @printInt(i32 %8)
	%9 = load i32, i32* %b
	call void @printInt(i32 %9)
	%10 = load i32, i32* %b
	%11 = sdiv i32 %10, 11
	%12 = load i32, i32* %a
	%13 = add i32 %11, %12
	%14 = mul i32 %13, 53
	%15 = load i32, i32* %a
	%16 = sub i32 %14, %15
	%17 = sub i32 %16, 31
	call void @printInt(i32 %17)
	ret i32 0
}
