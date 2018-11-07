declare void @printInt(i32)
define i32 @main() {
entry:
	%0 = sub i32 1, 1
	%1 = sub i32 1, 1
	%2 = sub i32 1, 1
	%3 = sub i32 1, 1
	%4 = sub i32 1, 1
	%5 = sub i32 1, 1
	%6 = sub i32 1, 1
	%7 = sub i32 1, 1
	%8 = sub i32 1, 1
	%9 = sub i32 1, 1
	%10 = sub i32 1, 1
	%11 = sub i32 1, 1
	%12 = sub i32 1, 1
	%13 = sub i32 1, 1
	%14 = sub i32 1, 1
	%15 = sub i32 1, 1
	%16 = sub i32 1, 1
	%17 = sub i32 1, 1
	%18 = sub i32 1, 1
	%19 = add i32 %17, %18
	%20 = add i32 %16, %19
	%21 = add i32 %15, %20
	%22 = add i32 %14, %21
	%23 = add i32 %13, %22
	%24 = add i32 %12, %23
	%25 = add i32 %11, %24
	%26 = add i32 %10, %25
	%27 = add i32 %9, %26
	%28 = add i32 %8, %27
	%29 = add i32 %7, %28
	%30 = add i32 %6, %29
	%31 = add i32 %5, %30
	%32 = add i32 %4, %31
	%33 = add i32 %3, %32
	%34 = add i32 %2, %33
	%35 = add i32 %1, %34
	%36 = add i32 %0, %35
	%37 = add i32 1, %36
	call void @printInt(i32 %37)
	ret i32 0
}
