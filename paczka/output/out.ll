declare void @printInt(i32)
define i32 @main() {
entry:
	%a_1 = add i32 0, 3
	%0 = add i32 3, 4
	call void @printInt(i32 %0)
	%1 = add i32 4, %a_1
	call void @printInt(i32 %1)
	%a_2 = add i32 0, 5
	%2 = add i32 4, %a_2
	call void @printInt(i32 %2)
	%3 = add i32 %a_2, 3
	%a_3 = add i32 0, %3
	%4 = mul i32 6, 3
	%5 = sdiv i32 5, 2
	%6 = sub i32 3, 1
	%7 = add i32 %5, %6
	%8 = add i32 %4, %7
	%9 = add i32 %a_3, %8
	%a_4 = add i32 0, %9
	call void @printInt(i32 %a_4)
	%10 = add i32 %a_4, 3
	%b_1 = add i32 0, %10
	ret i32 0
}
