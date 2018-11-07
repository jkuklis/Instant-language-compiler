declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%b = alloca i32
	%d = alloca i32
	call void @printInt(i32 38)
	%0 = mul i32 60, 148
	%1 = sdiv i32 %0, 8
	call void @printInt(i32 %1)
	%2 = sub i32 52, 56
	%3 = sub i32 70, %2
	%4 = add i32 %3, 34
	%5 = sdiv i32 %4, 1
	%6 = mul i32 21, %5
	%7 = sdiv i32 18, 58
	%8 = sdiv i32 15, 77
	%9 = add i32 %7, %8
	%10 = mul i32 18, 41
	%11 = sub i32 %9, %10
	%12 = add i32 %6, %11
	%13 = add i32 45, %12
	store i32 %13, i32* %a
	%14 = load i32, i32* %a
	store i32 %14, i32* %b
	%15 = load i32, i32* %a
	%16 = load i32, i32* %a
	%17 = sub i32 %15, %16
	call void @printInt(i32 %17)
	call void @printInt(i32 60)
	call void @printInt(i32 42)
	call void @printInt(i32 14)
	%18 = load i32, i32* %a
	call void @printInt(i32 %18)
	%19 = load i32, i32* %b
	%20 = load i32, i32* %a
	%21 = add i32 %19, %20
	store i32 %21, i32* %c
	%22 = sub i32 60, 43
	call void @printInt(i32 %22)
	call void @printInt(i32 31)
	%23 = sub i32 21, 46
	%24 = sdiv i32 %23, 45
	call void @printInt(i32 %24)
	%25 = load i32, i32* %b
	call void @printInt(i32 %25)
	%26 = load i32, i32* %a
	%27 = load i32, i32* %c
	%28 = sub i32 %26, %27
	call void @printInt(i32 %28)
	call void @printInt(i32 33)
	%29 = load i32, i32* %c
	store i32 %29, i32* %d
	ret i32 0
}
