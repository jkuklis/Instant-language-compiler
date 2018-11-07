declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%e = alloca i32
	%b = alloca i32
	%f = alloca i32
	%d = alloca i32
	%g = alloca i32
	call void @printInt(i32 43)
	store i32 23, i32* %a
	%0 = load i32, i32* %a
	call void @printInt(i32 %0)
	call void @printInt(i32 29)
	%1 = load i32, i32* %a
	%2 = mul i32 49, %1
	%3 = mul i32 15, %2
	%4 = add i32 5, %3
	call void @printInt(i32 %4)
	%5 = load i32, i32* %a
	%6 = sub i32 %5, 2
	call void @printInt(i32 %6)
	%7 = load i32, i32* %a
	call void @printInt(i32 %7)
	%8 = load i32, i32* %a
	%9 = mul i32 7, %8
	call void @printInt(i32 %9)
	call void @printInt(i32 40)
	call void @printInt(i32 8)
	store i32 88, i32* %b
	call void @printInt(i32 22)
	%10 = load i32, i32* %a
	call void @printInt(i32 %10)
	call void @printInt(i32 59)
	store i32 104, i32* %c
	%11 = load i32, i32* %c
	call void @printInt(i32 %11)
	call void @printInt(i32 54)
	%12 = add i32 23, 10
	%13 = load i32, i32* %a
	%14 = sub i32 %12, %13
	store i32 %14, i32* %d
	%15 = load i32, i32* %a
	store i32 %15, i32* %e
	call void @printInt(i32 25)
	%16 = load i32, i32* %c
	call void @printInt(i32 %16)
	call void @printInt(i32 26)
	%17 = load i32, i32* %c
	%18 = mul i32 %17, 25
	%19 = load i32, i32* %b
	%20 = sdiv i32 %18, %19
	call void @printInt(i32 %20)
	%21 = load i32, i32* %b
	call void @printInt(i32 %21)
	call void @printInt(i32 48)
	call void @printInt(i32 8)
	%22 = load i32, i32* %e
	call void @printInt(i32 %22)
	%23 = load i32, i32* %b
	store i32 %23, i32* %f
	%24 = load i32, i32* %a
	call void @printInt(i32 %24)
	%25 = load i32, i32* %e
	%26 = load i32, i32* %c
	%27 = sub i32 %25, %26
	%28 = add i32 %27, 30
	%29 = sdiv i32 0, %28
	call void @printInt(i32 %29)
	%30 = load i32, i32* %a
	%31 = sub i32 %30, 25
	%32 = load i32, i32* %f
	%33 = sub i32 56, 5
	%34 = mul i32 %32, %33
	%35 = load i32, i32* %c
	%36 = sub i32 0, %35
	%37 = sub i32 %34, %36
	%38 = sub i32 %37, 0
	%39 = mul i32 %31, %38
	call void @printInt(i32 %39)
	call void @printInt(i32 36)
	%40 = load i32, i32* %d
	store i32 %40, i32* %g
	%41 = load i32, i32* %e
	call void @printInt(i32 %41)
	%42 = load i32, i32* %f
	%43 = sub i32 47, %42
	call void @printInt(i32 %43)
	%44 = load i32, i32* %a
	%45 = mul i32 %44, 11
	call void @printInt(i32 %45)
	%46 = load i32, i32* %c
	call void @printInt(i32 %46)
	ret i32 0
}
