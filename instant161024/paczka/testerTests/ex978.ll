declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%e = alloca i32
	%c = alloca i32
	%f = alloca i32
	%d = alloca i32
	call void @printInt(i32 23)
	store i32 47, i32* %a
	%0 = load i32, i32* %a
	%1 = add i32 %0, 1
	call void @printInt(i32 %1)
	%2 = load i32, i32* %a
	call void @printInt(i32 %2)
	%3 = add i32 58, 23
	%4 = sub i32 %3, 20
	%5 = mul i32 1, %4
	store i32 %5, i32* %b
	%6 = load i32, i32* %b
	call void @printInt(i32 %6)
	%7 = load i32, i32* %a
	store i32 %7, i32* %c
	%8 = add i32 9, 46
	call void @printInt(i32 %8)
	%9 = load i32, i32* %a
	%10 = sub i32 %9, 6
	call void @printInt(i32 %10)
	call void @printInt(i32 60)
	%11 = load i32, i32* %a
	%12 = load i32, i32* %b
	%13 = add i32 %11, %12
	%14 = mul i32 %13, 29
	call void @printInt(i32 %14)
	store i32 12, i32* %d
	call void @printInt(i32 20)
	%15 = sub i32 48, 2
	store i32 %15, i32* %e
	%16 = load i32, i32* %e
	call void @printInt(i32 %16)
	%17 = add i32 29, 37
	call void @printInt(i32 %17)
	%18 = load i32, i32* %c
	call void @printInt(i32 %18)
	%19 = mul i32 10, 24
	call void @printInt(i32 %19)
	%20 = load i32, i32* %b
	call void @printInt(i32 %20)
	%21 = load i32, i32* %e
	store i32 %21, i32* %f
	ret i32 0
}
