declare void @printInt(i32)
define i32 @main() {
entry:
	store i32 3, i32* %a
	%0 = add i32 3, 4
	call void @printInt(i32 %0)
	%1 = load i32, i32* %a
	%2 = add i32 4, %1
	call void @printInt(i32 %2)
	store i32 5, i32* %a
	%3 = load i32, i32* %a
	%4 = add i32 4, %3
	call void @printInt(i32 %4)
	%5 = load i32, i32* %a
	%6 = add i32 %5, 3
	store i32 %6, i32* %a
	%7 = load i32, i32* %a
	%8 = mul i32 6, 3
	%9 = sdiv i32 5, 2
	%10 = sub i32 3, 1
	%11 = add i32 %9, %10
	%12 = add i32 %8, %11
	%13 = add i32 %7, %12
	store i32 %13, i32* %a
	%14 = load i32, i32* %a
	call void @printInt(i32 %14)
	%15 = load i32, i32* %a
	%16 = add i32 %15, 3
	store i32 %16, i32* %b
	ret i32 0
}
