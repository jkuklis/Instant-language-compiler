declare void @printInt(i64)
define i32 @main() {
entry:
	%a_0 = add i64 0, 3
	%0 = add i64 3, 4
	call void @printInt(i64 %0)
	%1 = add i64 4, %a_0
	call void @printInt(i64 %1)
	%a_1 = add i64 0, 5
	%2 = add i64 4, %a_1
	call void @printInt(i64 %2)
	%3 = add i64 %a_1, 3
	%a_2 = add i64 0, %3
	%4 = mul i64 6, 3
	%5 = sdiv i64 5, 2
	%6 = sub i64 3, 1
	%7 = add i64 %5, %6
	%8 = add i64 %4, %7
	%9 = add i64 %a_2, %8
	%a_3 = add i64 0, %9
	call void @printInt(i64 %a_3)
	%10 = add i64 %a_3, 3
	%b_0 = add i64 0, %10
	ret i32 0
}
