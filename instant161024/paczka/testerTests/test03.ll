declare void @printInt(i32)
define i32 @main() {
entry:
	%0 = add i32 1, 1
	%1 = add i32 1, %0
	%2 = add i32 1, %1
	%3 = add i32 1, %2
	%4 = add i32 1, %3
	%5 = add i32 1, %4
	%6 = add i32 1, %5
	%7 = add i32 1, %6
	%8 = add i32 1, %7
	%9 = add i32 1, %8
	%10 = add i32 1, %9
	%11 = add i32 1, %10
	%12 = add i32 1, %11
	%13 = add i32 1, %12
	%14 = add i32 1, %13
	%15 = add i32 1, %14
	%16 = add i32 1, %15
	%17 = add i32 1, %16
	%18 = add i32 1, %17
	%19 = add i32 1, %18
	%20 = add i32 1, %19
	%21 = add i32 1, %20
	%22 = add i32 1, %21
	%23 = add i32 1, %22
	%24 = add i32 1, %23
	%25 = add i32 1, %24
	%26 = add i32 1, %25
	%27 = add i32 1, %26
	%28 = add i32 1, %27
	%29 = add i32 1, %28
	%30 = add i32 1, %29
	%31 = add i32 1, %30
	%32 = add i32 1, %31
	%33 = add i32 1, %32
	%34 = add i32 1, %33
	%35 = add i32 1, %34
	%36 = add i32 1, %35
	%37 = add i32 1, %36
	%38 = add i32 1, %37
	%39 = add i32 1, %38
	%40 = add i32 1, %39
	call void @printInt(i32 %40)
	ret i32 0
}
