declare void @printInt(i32)
define i32 @main() {
entry:
	%c = alloca i32
	%a = alloca i32
	%e = alloca i32
	%b = alloca i32
	%f = alloca i32
	%d = alloca i32
	%g = alloca i32
	%h = alloca i32
	%0 = sdiv i32 50, 42
	%1 = mul i32 22, %0
	call void @printInt(i32 %1)
	store i32 17, i32* %a
	%2 = load i32, i32* %a
	call void @printInt(i32 %2)
	%3 = mul i32 5, 11
	%4 = load i32, i32* %a
	%5 = mul i32 %3, %4
	%6 = sub i32 %5, 12
	%7 = load i32, i32* %a
	%8 = add i32 %6, %7
	call void @printInt(i32 %8)
	call void @printInt(i32 82)
	store i32 42, i32* %b
	call void @printInt(i32 38)
	store i32 27, i32* %c
	%9 = load i32, i32* %c
	call void @printInt(i32 %9)
	%10 = load i32, i32* %c
	%11 = add i32 %10, 7
	%12 = sub i32 4, %11
	call void @printInt(i32 %12)
	%13 = load i32, i32* %c
	%14 = load i32, i32* %a
	%15 = load i32, i32* %b
	%16 = sdiv i32 %14, %15
	%17 = add i32 %13, %16
	store i32 %17, i32* %d
	call void @printInt(i32 9)
	call void @printInt(i32 52)
	%18 = load i32, i32* %c
	call void @printInt(i32 %18)
	%19 = mul i32 39, 18
	call void @printInt(i32 %19)
	%20 = load i32, i32* %c
	%21 = mul i32 %20, 23
	call void @printInt(i32 %21)
	%22 = load i32, i32* %c
	%23 = add i32 %22, 43
	store i32 %23, i32* %e
	call void @printInt(i32 50)
	%24 = sdiv i32 30, 32
	%25 = load i32, i32* %b
	%26 = mul i32 %25, 48
	%27 = mul i32 %24, %26
	store i32 %27, i32* %f
	%28 = load i32, i32* %f
	store i32 %28, i32* %g
	%29 = load i32, i32* %b
	call void @printInt(i32 %29)
	call void @printInt(i32 31)
	%30 = load i32, i32* %e
	call void @printInt(i32 %30)
	%31 = load i32, i32* %f
	%32 = sub i32 9, %31
	call void @printInt(i32 %32)
	call void @printInt(i32 46)
	%33 = load i32, i32* %e
	%34 = load i32, i32* %d
	%35 = load i32, i32* %a
	%36 = sub i32 16, 10
	%37 = sdiv i32 %35, %36
	%38 = add i32 %34, %37
	%39 = sdiv i32 %33, %38
	call void @printInt(i32 %39)
	%40 = load i32, i32* %c
	call void @printInt(i32 %40)
	call void @printInt(i32 140)
	%41 = load i32, i32* %f
	call void @printInt(i32 %41)
	call void @printInt(i32 61)
	call void @printInt(i32 8)
	call void @printInt(i32 29)
	%42 = load i32, i32* %g
	%43 = load i32, i32* %d
	%44 = add i32 %42, %43
	%45 = load i32, i32* %c
	%46 = sdiv i32 %44, %45
	%47 = load i32, i32* %c
	%48 = sdiv i32 111, %47
	%49 = mul i32 %46, %48
	%50 = load i32, i32* %a
	%51 = sdiv i32 %49, %50
	call void @printInt(i32 %51)
	%52 = load i32, i32* %e
	%53 = load i32, i32* %e
	%54 = sub i32 %52, %53
	call void @printInt(i32 %54)
	call void @printInt(i32 6)
	%55 = load i32, i32* %b
	store i32 %55, i32* %h
	call void @printInt(i32 7)
	%56 = load i32, i32* %d
	%57 = sub i32 %56, 38
	call void @printInt(i32 %57)
	%58 = load i32, i32* %c
	%59 = sdiv i32 23, %58
	%60 = mul i32 %59, 37
	%61 = sub i32 5, %60
	call void @printInt(i32 %61)
	%62 = load i32, i32* %c
	call void @printInt(i32 %62)
	%63 = load i32, i32* %c
	call void @printInt(i32 %63)
	call void @printInt(i32 70)
	ret i32 0
}
