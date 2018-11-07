declare void @printInt(i32)
define i32 @main() {
entry:
	%b = alloca i32
	%a = alloca i32
	%c = alloca i32
	%d = alloca i32
	call void @printInt(i32 2)
	call void @printInt(i32 15)
	call void @printInt(i32 46)
	call void @printInt(i32 56)
	%0 = mul i32 29, 74
	%1 = sub i32 71, %0
	call void @printInt(i32 %1)
	call void @printInt(i32 57)
	call void @printInt(i32 59)
	call void @printInt(i32 6)
	%2 = add i32 1, 69
	%3 = sub i32 %2, 22
	%4 = sub i32 1, 44
	%5 = sub i32 %4, 35
	%6 = sub i32 10, %5
	%7 = mul i32 %6, 64
	%8 = sdiv i32 %3, %7
	call void @printInt(i32 %8)
	call void @printInt(i32 42)
	call void @printInt(i32 6)
	call void @printInt(i32 27)
	call void @printInt(i32 22)
	store i32 35, i32* %a
	%9 = load i32, i32* %a
	call void @printInt(i32 %9)
	%10 = load i32, i32* %a
	%11 = sub i32 %10, 8
	call void @printInt(i32 %11)
	store i32 3, i32* %b
	call void @printInt(i32 66)
	%12 = load i32, i32* %b
	%13 = load i32, i32* %b
	%14 = mul i32 %12, %13
	call void @printInt(i32 %14)
	%15 = load i32, i32* %a
	%16 = load i32, i32* %b
	%17 = add i32 %15, %16
	%18 = add i32 49, %17
	call void @printInt(i32 %18)
	%19 = load i32, i32* %b
	call void @printInt(i32 %19)
	%20 = load i32, i32* %b
	%21 = load i32, i32* %b
	%22 = mul i32 %20, %21
	store i32 %22, i32* %c
	%23 = load i32, i32* %b
	%24 = load i32, i32* %b
	%25 = load i32, i32* %b
	%26 = mul i32 %24, %25
	%27 = add i32 %23, %26
	%28 = load i32, i32* %c
	%29 = mul i32 %28, 142
	%30 = sub i32 %27, %29
	%31 = load i32, i32* %a
	%32 = add i32 %30, %31
	call void @printInt(i32 %32)
	call void @printInt(i32 31)
	%33 = load i32, i32* %c
	call void @printInt(i32 %33)
	%34 = load i32, i32* %b
	store i32 %34, i32* %d
	call void @printInt(i32 18)
	ret i32 0
}
