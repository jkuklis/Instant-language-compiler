declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%e = alloca i32
	%d = alloca i32
	%0 = add i32 64, 87
	%1 = sdiv i32 38, %0
	call void @printInt(i32 %1)
	call void @printInt(i32 3)
	store i32 16, i32* %a
	%2 = load i32, i32* %a
	%3 = mul i32 88, %2
	call void @printInt(i32 %3)
	call void @printInt(i32 18)
	%4 = mul i32 47, 98
	%5 = load i32, i32* %a
	%6 = sdiv i32 42, %5
	%7 = add i32 %4, %6
	call void @printInt(i32 %7)
	call void @printInt(i32 48)
	%8 = load i32, i32* %a
	call void @printInt(i32 %8)
	%9 = load i32, i32* %a
	%10 = load i32, i32* %a
	%11 = load i32, i32* %a
	%12 = add i32 %10, %11
	%13 = add i32 %9, %12
	call void @printInt(i32 %13)
	call void @printInt(i32 10)
	call void @printInt(i32 26)
	call void @printInt(i32 37)
	%14 = load i32, i32* %a
	store i32 %14, i32* %b
	store i32 2, i32* %c
	%15 = load i32, i32* %c
	call void @printInt(i32 %15)
	call void @printInt(i32 8)
	%16 = load i32, i32* %b
	%17 = sub i32 %16, 21
	%18 = mul i32 50, %17
	call void @printInt(i32 %18)
	%19 = load i32, i32* %a
	call void @printInt(i32 %19)
	%20 = load i32, i32* %b
	%21 = sdiv i32 %20, 53
	store i32 %21, i32* %d
	%22 = load i32, i32* %b
	%23 = sdiv i32 6, 39
	%24 = add i32 %22, %23
	store i32 %24, i32* %e
	ret i32 0
}
