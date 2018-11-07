declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%e = alloca i32
	%c = alloca i32
	%b = alloca i32
	%i = alloca i32
	%d = alloca i32
	%f = alloca i32
	%h = alloca i32
	%g = alloca i32
	store i32 23, i32* %a
	%0 = load i32, i32* %a
	%1 = mul i32 43, %0
	%2 = add i32 29, 46
	%3 = sdiv i32 62, %2
	%4 = sub i32 %1, %3
	call void @printInt(i32 %4)
	%5 = load i32, i32* %a
	%6 = load i32, i32* %a
	%7 = add i32 %5, %6
	%8 = sdiv i32 %7, 1
	%9 = add i32 %8, 38
	%10 = sub i32 18, 21
	%11 = add i32 %9, %10
	call void @printInt(i32 %11)
	call void @printInt(i32 6)
	store i32 19, i32* %b
	%12 = load i32, i32* %b
	%13 = sdiv i32 20, %12
	call void @printInt(i32 %13)
	%14 = load i32, i32* %a
	call void @printInt(i32 %14)
	%15 = mul i32 99, 10
	call void @printInt(i32 %15)
	%16 = load i32, i32* %b
	store i32 %16, i32* %c
	call void @printInt(i32 23)
	%17 = load i32, i32* %c
	%18 = sdiv i32 %17, 64
	store i32 %18, i32* %d
	%19 = sub i32 77, 28
	%20 = mul i32 29, %19
	call void @printInt(i32 %20)
	%21 = load i32, i32* %b
	call void @printInt(i32 %21)
	call void @printInt(i32 21)
	%22 = load i32, i32* %c
	%23 = mul i32 %22, 15
	call void @printInt(i32 %23)
	%24 = load i32, i32* %d
	%25 = load i32, i32* %d
	%26 = mul i32 %24, %25
	call void @printInt(i32 %26)
	%27 = load i32, i32* %c
	call void @printInt(i32 %27)
	%28 = sdiv i32 34, 12
	call void @printInt(i32 %28)
	store i32 92, i32* %e
	%29 = load i32, i32* %a
	call void @printInt(i32 %29)
	%30 = load i32, i32* %b
	%31 = mul i32 %30, 65
	%32 = sub i32 %31, 2
	%33 = add i32 27, %32
	call void @printInt(i32 %33)
	store i32 40, i32* %f
	%34 = load i32, i32* %d
	call void @printInt(i32 %34)
	%35 = load i32, i32* %e
	call void @printInt(i32 %35)
	call void @printInt(i32 9)
	%36 = load i32, i32* %e
	store i32 %36, i32* %g
	store i32 27, i32* %h
	%37 = load i32, i32* %e
	store i32 %37, i32* %i
	call void @printInt(i32 13)
	call void @printInt(i32 35)
	%38 = load i32, i32* %i
	call void @printInt(i32 %38)
	call void @printInt(i32 6)
	%39 = load i32, i32* %i
	call void @printInt(i32 %39)
	%40 = load i32, i32* %c
	call void @printInt(i32 %40)
	%41 = load i32, i32* %e
	call void @printInt(i32 %41)
	%42 = load i32, i32* %f
	call void @printInt(i32 %42)
	%43 = load i32, i32* %i
	%44 = mul i32 12, %43
	call void @printInt(i32 %44)
	ret i32 0
}
