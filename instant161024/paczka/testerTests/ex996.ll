declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%b = alloca i32
	%h = alloca i32
	%d = alloca i32
	%j = alloca i32
	%i = alloca i32
	%g = alloca i32
	%f = alloca i32
	%e = alloca i32
	store i32 10, i32* %a
	%0 = load i32, i32* %a
	call void @printInt(i32 %0)
	%1 = load i32, i32* %a
	call void @printInt(i32 %1)
	%2 = load i32, i32* %a
	%3 = mul i32 24, %2
	%4 = sub i32 53, 14
	%5 = add i32 %4, 23
	%6 = add i32 %3, %5
	store i32 %6, i32* %b
	%7 = load i32, i32* %a
	%8 = load i32, i32* %a
	%9 = sdiv i32 %7, %8
	%10 = sdiv i32 20, 39
	%11 = load i32, i32* %b
	%12 = sub i32 %10, %11
	%13 = sub i32 41, %12
	%14 = add i32 %13, 25
	%15 = load i32, i32* %a
	%16 = load i32, i32* %b
	%17 = sub i32 %15, %16
	%18 = mul i32 %14, %17
	%19 = sub i32 %9, %18
	call void @printInt(i32 %19)
	%20 = sdiv i32 12, 14
	%21 = mul i32 46, %20
	call void @printInt(i32 %21)
	store i32 47, i32* %c
	%22 = load i32, i32* %c
	%23 = add i32 %22, 5
	call void @printInt(i32 %23)
	%24 = load i32, i32* %c
	call void @printInt(i32 %24)
	store i32 15, i32* %d
	%25 = sdiv i32 48, 17
	call void @printInt(i32 %25)
	store i32 33, i32* %e
	%26 = load i32, i32* %d
	call void @printInt(i32 %26)
	store i32 100, i32* %f
	%27 = load i32, i32* %d
	%28 = sub i32 %27, 42
	store i32 %28, i32* %g
	%29 = load i32, i32* %c
	%30 = load i32, i32* %a
	%31 = load i32, i32* %a
	%32 = mul i32 61, %31
	%33 = load i32, i32* %b
	%34 = mul i32 %32, %33
	%35 = add i32 69, %34
	%36 = sdiv i32 16, %35
	%37 = load i32, i32* %a
	%38 = mul i32 %36, %37
	%39 = add i32 %30, %38
	%40 = mul i32 %29, %39
	store i32 %40, i32* %h
	call void @printInt(i32 11)
	%41 = load i32, i32* %h
	%42 = load i32, i32* %c
	%43 = sub i32 %41, %42
	%44 = sdiv i32 %43, 4
	call void @printInt(i32 %44)
	%45 = load i32, i32* %h
	call void @printInt(i32 %45)
	store i32 69, i32* %i
	call void @printInt(i32 30)
	call void @printInt(i32 4)
	%46 = load i32, i32* %b
	%47 = sub i32 25, %46
	call void @printInt(i32 %47)
	store i32 11, i32* %j
	call void @printInt(i32 41)
	ret i32 0
}
