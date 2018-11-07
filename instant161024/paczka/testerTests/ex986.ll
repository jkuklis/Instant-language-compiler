declare void @printInt(i32)
define i32 @main() {
entry:
	%b = alloca i32
	%d = alloca i32
	%c = alloca i32
	%a = alloca i32
	%g = alloca i32
	%f = alloca i32
	%e = alloca i32
	%0 = add i32 24, 12
	%1 = sdiv i32 35, 27
	%2 = add i32 20, %1
	%3 = sub i32 24, 33
	%4 = sub i32 %3, 34
	%5 = mul i32 3, 6
	%6 = add i32 27, %5
	%7 = add i32 43, 58
	%8 = sub i32 80, %7
	%9 = sdiv i32 4, %8
	%10 = add i32 12, %9
	%11 = sdiv i32 %6, %10
	%12 = sub i32 %4, %11
	%13 = add i32 86, 38
	%14 = sub i32 25, 29
	%15 = add i32 %13, %14
	%16 = sub i32 69, 0
	%17 = sdiv i32 2, %16
	%18 = mul i32 %15, %17
	%19 = mul i32 95, 14
	%20 = sdiv i32 %18, %19
	%21 = sub i32 %12, %20
	%22 = sub i32 %21, 10
	%23 = mul i32 %2, %22
	%24 = add i32 %23, 96
	%25 = sub i32 %24, 2
	%26 = mul i32 %0, %25
	%27 = mul i32 %26, 5
	%28 = sub i32 %27, 17
	%29 = sdiv i32 %28, 25
	store i32 %29, i32* %a
	store i32 5, i32* %b
	call void @printInt(i32 10)
	%30 = load i32, i32* %b
	store i32 %30, i32* %c
	call void @printInt(i32 29)
	%31 = load i32, i32* %a
	call void @printInt(i32 %31)
	%32 = sdiv i32 7, 33
	store i32 %32, i32* %d
	%33 = load i32, i32* %d
	call void @printInt(i32 %33)
	store i32 52, i32* %e
	%34 = load i32, i32* %c
	call void @printInt(i32 %34)
	call void @printInt(i32 38)
	call void @printInt(i32 2)
	%35 = load i32, i32* %b
	store i32 %35, i32* %f
	call void @printInt(i32 1)
	%36 = load i32, i32* %a
	call void @printInt(i32 %36)
	store i32 56, i32* %g
	%37 = load i32, i32* %c
	%38 = load i32, i32* %b
	%39 = sdiv i32 %37, %38
	call void @printInt(i32 %39)
	call void @printInt(i32 19)
	%40 = load i32, i32* %b
	%41 = load i32, i32* %d
	%42 = sub i32 %40, %41
	call void @printInt(i32 %42)
	ret i32 0
}
