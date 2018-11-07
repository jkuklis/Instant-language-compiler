declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%d = alloca i32
	%k = alloca i32
	%h = alloca i32
	%g = alloca i32
	%f = alloca i32
	%e = alloca i32
	%i = alloca i32
	%n = alloca i32
	%m = alloca i32
	%l = alloca i32
	%j = alloca i32
	%0 = mul i32 64, 33
	call void @printInt(i32 %0)
	%1 = mul i32 16, 29
	store i32 %1, i32* %a
	%2 = load i32, i32* %a
	call void @printInt(i32 %2)
	%3 = load i32, i32* %a
	call void @printInt(i32 %3)
	store i32 3, i32* %b
	%4 = load i32, i32* %a
	store i32 %4, i32* %c
	%5 = load i32, i32* %b
	store i32 %5, i32* %d
	%6 = load i32, i32* %b
	%7 = load i32, i32* %b
	%8 = mul i32 %7, 25
	%9 = mul i32 %6, %8
	%10 = add i32 32, %9
	%11 = load i32, i32* %c
	%12 = mul i32 %10, %11
	call void @printInt(i32 %12)
	%13 = sub i32 18, 16
	call void @printInt(i32 %13)
	%14 = load i32, i32* %b
	call void @printInt(i32 %14)
	store i32 42, i32* %e
	%15 = load i32, i32* %a
	store i32 %15, i32* %f
	%16 = load i32, i32* %c
	call void @printInt(i32 %16)
	%17 = mul i32 30, 28
	store i32 %17, i32* %g
	call void @printInt(i32 80)
	%18 = load i32, i32* %f
	call void @printInt(i32 %18)
	%19 = add i32 64, 32
	%20 = load i32, i32* %d
	%21 = sdiv i32 %19, %20
	call void @printInt(i32 %21)
	%22 = load i32, i32* %a
	call void @printInt(i32 %22)
	%23 = load i32, i32* %a
	%24 = load i32, i32* %b
	%25 = add i32 %24, 8
	%26 = sub i32 %23, %25
	%27 = add i32 19, %26
	%28 = mul i32 %27, 64
	%29 = sub i32 14, %28
	%30 = load i32, i32* %g
	%31 = sub i32 %30, 13
	%32 = load i32, i32* %a
	%33 = add i32 7, %32
	%34 = sub i32 %33, 22
	%35 = add i32 %34, 54
	%36 = load i32, i32* %b
	%37 = load i32, i32* %e
	%38 = load i32, i32* %c
	%39 = add i32 %38, 55
	%40 = add i32 %37, %39
	%41 = load i32, i32* %c
	%42 = load i32, i32* %b
	%43 = mul i32 %41, %42
	%44 = mul i32 %40, %43
	%45 = sub i32 %36, %44
	%46 = mul i32 %35, %45
	%47 = add i32 %31, %46
	%48 = add i32 %29, %47
	%49 = add i32 61, %48
	%50 = load i32, i32* %d
	%51 = sub i32 %49, %50
	store i32 %51, i32* %h
	call void @printInt(i32 42)
	%52 = load i32, i32* %g
	call void @printInt(i32 %52)
	call void @printInt(i32 4)
	call void @printInt(i32 26)
	%53 = load i32, i32* %h
	%54 = load i32, i32* %f
	%55 = sub i32 %53, %54
	store i32 %55, i32* %i
	%56 = load i32, i32* %a
	%57 = sub i32 %56, 15
	store i32 %57, i32* %j
	call void @printInt(i32 41)
	%58 = load i32, i32* %b
	call void @printInt(i32 %58)
	%59 = load i32, i32* %e
	call void @printInt(i32 %59)
	call void @printInt(i32 3)
	store i32 14, i32* %k
	%60 = load i32, i32* %a
	%61 = load i32, i32* %d
	%62 = load i32, i32* %i
	%63 = mul i32 %61, %62
	%64 = sdiv i32 %60, %63
	call void @printInt(i32 %64)
	%65 = load i32, i32* %k
	call void @printInt(i32 %65)
	%66 = load i32, i32* %k
	%67 = sub i32 %66, 55
	store i32 %67, i32* %l
	%68 = load i32, i32* %a
	%69 = sdiv i32 80, 18
	%70 = add i32 %68, %69
	store i32 %70, i32* %m
	%71 = load i32, i32* %c
	call void @printInt(i32 %71)
	%72 = load i32, i32* %h
	call void @printInt(i32 %72)
	%73 = load i32, i32* %d
	call void @printInt(i32 %73)
	call void @printInt(i32 47)
	%74 = load i32, i32* %k
	%75 = sdiv i32 6, %74
	call void @printInt(i32 %75)
	store i32 46, i32* %n
	ret i32 0
}
