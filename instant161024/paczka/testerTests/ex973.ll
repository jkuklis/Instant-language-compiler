declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%e = alloca i32
	%d = alloca i32
	%0 = mul i32 89, 27
	%1 = add i32 33, %0
	call void @printInt(i32 %1)
	%2 = sub i32 41, 57
	%3 = sub i32 %2, 5
	call void @printInt(i32 %3)
	%4 = add i32 40, 2
	call void @printInt(i32 %4)
	store i32 11, i32* %a
	store i32 130, i32* %b
	%5 = load i32, i32* %a
	call void @printInt(i32 %5)
	%6 = load i32, i32* %b
	%7 = load i32, i32* %a
	%8 = sdiv i32 %6, %7
	call void @printInt(i32 %8)
	%9 = load i32, i32* %a
	%10 = sdiv i32 38, %9
	call void @printInt(i32 %10)
	%11 = load i32, i32* %b
	%12 = sub i32 18, %11
	%13 = load i32, i32* %b
	%14 = add i32 27, %13
	%15 = sdiv i32 %12, %14
	call void @printInt(i32 %15)
	%16 = load i32, i32* %a
	%17 = add i32 72, %16
	call void @printInt(i32 %17)
	store i32 13, i32* %c
	%18 = load i32, i32* %c
	call void @printInt(i32 %18)
	store i32 16, i32* %d
	call void @printInt(i32 10)
	%19 = sub i32 18, 31
	store i32 %19, i32* %e
	ret i32 0
}
