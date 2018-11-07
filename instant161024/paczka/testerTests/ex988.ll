declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	call void @printInt(i32 39)
	%0 = sub i32 1, 12
	%1 = mul i32 %0, 8
	%2 = sdiv i32 52, 20
	%3 = sub i32 %1, %2
	%4 = add i32 %3, 65
	%5 = add i32 %4, 41
	call void @printInt(i32 %5)
	call void @printInt(i32 80)
	call void @printInt(i32 80)
	call void @printInt(i32 30)
	%6 = add i32 44, 74
	call void @printInt(i32 %6)
	call void @printInt(i32 34)
	call void @printInt(i32 30)
	%7 = add i32 23, 15
	%8 = sdiv i32 %7, 13
	%9 = add i32 72, %8
	store i32 %9, i32* %a
	%10 = load i32, i32* %a
	%11 = load i32, i32* %a
	%12 = load i32, i32* %a
	%13 = add i32 %12, 52
	%14 = add i32 %11, %13
	%15 = mul i32 %10, %14
	%16 = load i32, i32* %a
	%17 = add i32 %15, %16
	call void @printInt(i32 %17)
	call void @printInt(i32 36)
	ret i32 0
}
