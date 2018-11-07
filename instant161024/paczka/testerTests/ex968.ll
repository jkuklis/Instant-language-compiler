declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%0 = mul i32 32, 37
	call void @printInt(i32 %0)
	call void @printInt(i32 46)
	call void @printInt(i32 54)
	call void @printInt(i32 20)
	call void @printInt(i32 35)
	%1 = sdiv i32 90, 52
	%2 = add i32 31, 20
	%3 = sub i32 %1, %2
	%4 = sub i32 35, %3
	%5 = mul i32 3, %4
	%6 = sub i32 42, %5
	call void @printInt(i32 %6)
	store i32 40, i32* %a
	%7 = load i32, i32* %a
	call void @printInt(i32 %7)
	%8 = load i32, i32* %a
	%9 = load i32, i32* %a
	%10 = load i32, i32* %a
	%11 = load i32, i32* %a
	%12 = mul i32 %10, %11
	%13 = load i32, i32* %a
	%14 = sub i32 %12, %13
	%15 = mul i32 %14, 33
	%16 = add i32 18, %15
	%17 = load i32, i32* %a
	%18 = mul i32 %16, %17
	%19 = add i32 %9, %18
	%20 = add i32 %8, %19
	call void @printInt(i32 %20)
	%21 = load i32, i32* %a
	call void @printInt(i32 %21)
	%22 = mul i32 42, 9
	%23 = mul i32 %22, 5
	call void @printInt(i32 %23)
	store i32 18, i32* %b
	%24 = load i32, i32* %a
	call void @printInt(i32 %24)
	%25 = load i32, i32* %b
	%26 = load i32, i32* %a
	%27 = mul i32 %25, %26
	store i32 %27, i32* %c
	ret i32 0
}
