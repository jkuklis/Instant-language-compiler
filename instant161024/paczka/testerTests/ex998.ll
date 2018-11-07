declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%g = alloca i32
	%e = alloca i32
	%f = alloca i32
	%d = alloca i32
	%c = alloca i32
	%b = alloca i32
	%i = alloca i32
	%h = alloca i32
	%0 = sdiv i32 22, 25
	%1 = sub i32 61, %0
	store i32 %1, i32* %a
	%2 = load i32, i32* %a
	%3 = sdiv i32 20, %2
	call void @printInt(i32 %3)
	store i32 18, i32* %b
	%4 = load i32, i32* %a
	%5 = load i32, i32* %a
	%6 = sub i32 %5, 33
	%7 = sub i32 %4, %6
	%8 = mul i32 21, %7
	%9 = load i32, i32* %a
	%10 = sdiv i32 %8, %9
	%11 = load i32, i32* %a
	%12 = sdiv i32 %10, %11
	call void @printInt(i32 %12)
	%13 = load i32, i32* %a
	call void @printInt(i32 %13)
	%14 = load i32, i32* %b
	store i32 %14, i32* %c
	%15 = sdiv i32 9, 19
	store i32 %15, i32* %d
	store i32 32, i32* %e
	store i32 23, i32* %f
	call void @printInt(i32 7)
	call void @printInt(i32 37)
	%16 = load i32, i32* %c
	%17 = sdiv i32 %16, 8
	store i32 %17, i32* %g
	%18 = load i32, i32* %g
	call void @printInt(i32 %18)
	call void @printInt(i32 78)
	%19 = load i32, i32* %e
	%20 = sub i32 67, %19
	%21 = load i32, i32* %g
	%22 = mul i32 %20, %21
	call void @printInt(i32 %22)
	%23 = load i32, i32* %e
	call void @printInt(i32 %23)
	store i32 14, i32* %h
	call void @printInt(i32 1)
	%24 = load i32, i32* %a
	call void @printInt(i32 %24)
	%25 = load i32, i32* %a
	%26 = sdiv i32 %25, 6
	%27 = mul i32 44, %26
	%28 = load i32, i32* %f
	%29 = add i32 %28, 39
	%30 = add i32 %27, %29
	%31 = mul i32 38, %30
	call void @printInt(i32 %31)
	%32 = load i32, i32* %d
	call void @printInt(i32 %32)
	call void @printInt(i32 31)
	store i32 41, i32* %i
	ret i32 0
}
