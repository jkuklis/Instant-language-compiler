declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%d = alloca i32
	call void @printInt(i32 65)
	store i32 20, i32* %a
	%0 = load i32, i32* %a
	%1 = mul i32 17, 12
	%2 = mul i32 %0, %1
	%3 = load i32, i32* %a
	%4 = add i32 %2, %3
	call void @printInt(i32 %4)
	call void @printInt(i32 42)
	%5 = sdiv i32 38, 10
	%6 = mul i32 86, 53
	%7 = load i32, i32* %a
	%8 = sub i32 %6, %7
	%9 = sdiv i32 %5, %8
	%10 = load i32, i32* %a
	%11 = mul i32 11, %10
	%12 = sdiv i32 3, %11
	%13 = load i32, i32* %a
	%14 = load i32, i32* %a
	%15 = sub i32 28, %14
	%16 = sdiv i32 %15, 23
	%17 = load i32, i32* %a
	%18 = add i32 %16, %17
	%19 = mul i32 24, %18
	%20 = add i32 34, %19
	%21 = sub i32 %13, %20
	%22 = load i32, i32* %a
	%23 = add i32 %21, %22
	%24 = sub i32 %12, %23
	%25 = sub i32 %24, 59
	%26 = load i32, i32* %a
	%27 = sub i32 %26, 44
	%28 = add i32 %25, %27
	%29 = mul i32 %9, %28
	call void @printInt(i32 %29)
	%30 = load i32, i32* %a
	store i32 %30, i32* %b
	call void @printInt(i32 29)
	%31 = load i32, i32* %b
	%32 = sub i32 9, 3
	%33 = sub i32 %31, %32
	%34 = load i32, i32* %b
	%35 = mul i32 %34, 21
	%36 = load i32, i32* %b
	%37 = sub i32 %35, %36
	%38 = load i32, i32* %b
	%39 = load i32, i32* %b
	%40 = mul i32 5, %39
	%41 = sdiv i32 %38, %40
	%42 = sub i32 %37, %41
	%43 = load i32, i32* %b
	%44 = mul i32 %42, %43
	%45 = sub i32 %33, %44
	%46 = sdiv i32 35, %45
	%47 = load i32, i32* %a
	%48 = sdiv i32 %47, 5
	%49 = mul i32 15, %48
	%50 = sub i32 %46, %49
	store i32 %50, i32* %c
	%51 = load i32, i32* %b
	%52 = sub i32 56, %51
	%53 = sub i32 14, %52
	store i32 %53, i32* %d
	%54 = load i32, i32* %c
	call void @printInt(i32 %54)
	call void @printInt(i32 12)
	call void @printInt(i32 8)
	call void @printInt(i32 50)
	%55 = load i32, i32* %a
	%56 = add i32 %55, 126
	call void @printInt(i32 %56)
	%57 = load i32, i32* %a
	call void @printInt(i32 %57)
	%58 = load i32, i32* %a
	call void @printInt(i32 %58)
	call void @printInt(i32 62)
	call void @printInt(i32 23)
	call void @printInt(i32 5)
	ret i32 0
}
