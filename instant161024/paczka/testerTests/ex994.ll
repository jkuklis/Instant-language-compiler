declare void @printInt(i32)
define i32 @main() {
entry:
	%c = alloca i32
	%a = alloca i32
	%b = alloca i32
	%f = alloca i32
	%e = alloca i32
	%h = alloca i32
	%g = alloca i32
	%d = alloca i32
	store i32 44, i32* %a
	%0 = load i32, i32* %a
	%1 = sdiv i32 %0, 35
	%2 = load i32, i32* %a
	%3 = sdiv i32 %1, %2
	%4 = add i32 54, %3
	%5 = load i32, i32* %a
	%6 = add i32 %4, %5
	call void @printInt(i32 %6)
	store i32 23, i32* %b
	%7 = add i32 31, 52
	store i32 %7, i32* %c
	%8 = load i32, i32* %c
	call void @printInt(i32 %8)
	%9 = load i32, i32* %b
	call void @printInt(i32 %9)
	store i32 53, i32* %d
	%10 = load i32, i32* %c
	store i32 %10, i32* %e
	call void @printInt(i32 18)
	call void @printInt(i32 40)
	%11 = load i32, i32* %b
	%12 = load i32, i32* %c
	%13 = mul i32 13, %12
	%14 = sub i32 %13, 36
	%15 = mul i32 %11, %14
	call void @printInt(i32 %15)
	%16 = load i32, i32* %e
	store i32 %16, i32* %f
	call void @printInt(i32 7)
	%17 = load i32, i32* %f
	call void @printInt(i32 %17)
	%18 = load i32, i32* %b
	%19 = sdiv i32 8, %18
	%20 = mul i32 %19, 46
	%21 = add i32 35, %20
	call void @printInt(i32 %21)
	%22 = load i32, i32* %a
	call void @printInt(i32 %22)
	store i32 108, i32* %g
	%23 = load i32, i32* %c
	store i32 %23, i32* %h
	%24 = load i32, i32* %c
	call void @printInt(i32 %24)
	call void @printInt(i32 29)
	ret i32 0
}
