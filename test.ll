declare void @printInt(i32)
define i32 @main() {
entry:
	%0 = add i32 3, 4
	call void @printInt(i32 %0)
	%1 = add i32 4, 3
	call void @printInt(i32 %1)
	%2 = add i32 4, 5
	call void @printInt(i32 %2)
	%3 = add i32 5, 3
	%4 = mul i32 6, 3
	%5 = sdiv i32 5, 2
	%6 = sub i32 3, 1
	%7 = add i32 %5, %6
	%8 = add i32 %4, %7
	%9 = add i32 %3, %8
	call void @printInt(i32 %9)
	%10 = add i32 %9, 3
	ret i32 0
}
