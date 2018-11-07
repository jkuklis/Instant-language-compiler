declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%h = alloca i32
	%c = alloca i32
	%d = alloca i32
	%j = alloca i32
	%g = alloca i32
	%f = alloca i32
	%e = alloca i32
	%m = alloca i32
	%o = alloca i32
	%n = alloca i32
	%l = alloca i32
	%k = alloca i32
	%i = alloca i32
	%0 = mul i32 23, 25
	call void @printInt(i32 %0)
	call void @printInt(i32 20)
	call void @printInt(i32 56)
	call void @printInt(i32 51)
	call void @printInt(i32 52)
	store i32 27, i32* %a
	%1 = load i32, i32* %a
	call void @printInt(i32 %1)
	%2 = load i32, i32* %a
	call void @printInt(i32 %2)
	%3 = load i32, i32* %a
	store i32 %3, i32* %b
	%4 = load i32, i32* %a
	%5 = load i32, i32* %a
	%6 = mul i32 %5, 4
	%7 = load i32, i32* %a
	%8 = sdiv i32 %6, %7
	%9 = mul i32 %4, %8
	call void @printInt(i32 %9)
	%10 = load i32, i32* %a
	%11 = load i32, i32* %b
	%12 = sub i32 %10, %11
	store i32 %12, i32* %c
	%13 = load i32, i32* %b
	%14 = load i32, i32* %a
	%15 = mul i32 %14, 18
	%16 = sdiv i32 %13, %15
	%17 = mul i32 %16, 7
	%18 = load i32, i32* %a
	%19 = mul i32 %17, %18
	store i32 %19, i32* %d
	%20 = load i32, i32* %b
	%21 = mul i32 %20, 16
	%22 = load i32, i32* %c
	%23 = mul i32 %21, %22
	%24 = sub i32 %23, 27
	store i32 %24, i32* %e
	%25 = load i32, i32* %b
	%26 = load i32, i32* %b
	%27 = load i32, i32* %c
	%28 = load i32, i32* %d
	%29 = add i32 9, %28
	%30 = sub i32 34, %29
	%31 = sub i32 %27, %30
	%32 = mul i32 45, %31
	%33 = sdiv i32 %26, %32
	%34 = mul i32 72, %33
	%35 = sub i32 67, %34
	%36 = sub i32 %25, %35
	%37 = load i32, i32* %c
	%38 = add i32 6, %37
	%39 = sub i32 %36, %38
	call void @printInt(i32 %39)
	call void @printInt(i32 6)
	store i32 87, i32* %f
	%40 = load i32, i32* %d
	%41 = sub i32 4, %40
	store i32 %41, i32* %g
	%42 = load i32, i32* %d
	call void @printInt(i32 %42)
	%43 = load i32, i32* %c
	call void @printInt(i32 %43)
	%44 = load i32, i32* %e
	%45 = sdiv i32 %44, 43
	call void @printInt(i32 %45)
	%46 = load i32, i32* %f
	store i32 %46, i32* %h
	call void @printInt(i32 43)
	%47 = add i32 14, 55
	%48 = load i32, i32* %c
	%49 = add i32 %47, %48
	store i32 %49, i32* %i
	call void @printInt(i32 35)
	%50 = load i32, i32* %g
	call void @printInt(i32 %50)
	%51 = load i32, i32* %g
	%52 = load i32, i32* %f
	%53 = sdiv i32 %52, 44
	%54 = sdiv i32 %51, %53
	call void @printInt(i32 %54)
	%55 = load i32, i32* %g
	call void @printInt(i32 %55)
	%56 = load i32, i32* %h
	call void @printInt(i32 %56)
	%57 = load i32, i32* %h
	call void @printInt(i32 %57)
	%58 = load i32, i32* %f
	call void @printInt(i32 %58)
	call void @printInt(i32 27)
	store i32 28, i32* %j
	%59 = load i32, i32* %h
	%60 = load i32, i32* %e
	%61 = sub i32 %60, 13
	%62 = sub i32 %59, %61
	store i32 %62, i32* %k
	%63 = load i32, i32* %h
	call void @printInt(i32 %63)
	call void @printInt(i32 39)
	%64 = load i32, i32* %b
	%65 = load i32, i32* %d
	%66 = sub i32 %65, 41
	%67 = load i32, i32* %a
	%68 = load i32, i32* %j
	%69 = load i32, i32* %h
	%70 = mul i32 %68, %69
	%71 = load i32, i32* %e
	%72 = add i32 57, %71
	%73 = sub i32 %70, %72
	%74 = mul i32 %67, %73
	%75 = mul i32 %66, %74
	%76 = sub i32 %64, %75
	%77 = load i32, i32* %h
	%78 = sub i32 %76, %77
	store i32 %78, i32* %l
	%79 = load i32, i32* %j
	call void @printInt(i32 %79)
	%80 = sub i32 6, 44
	%81 = mul i32 48, %80
	call void @printInt(i32 %81)
	%82 = load i32, i32* %c
	%83 = sub i32 %82, 9
	store i32 %83, i32* %m
	%84 = load i32, i32* %a
	call void @printInt(i32 %84)
	call void @printInt(i32 37)
	call void @printInt(i32 57)
	%85 = load i32, i32* %j
	store i32 %85, i32* %n
	%86 = load i32, i32* %b
	%87 = add i32 %86, 12
	%88 = load i32, i32* %d
	%89 = add i32 29, %88
	%90 = load i32, i32* %a
	%91 = sub i32 %89, %90
	%92 = mul i32 %91, 15
	%93 = sdiv i32 %87, %92
	store i32 %93, i32* %o
	call void @printInt(i32 75)
	%94 = load i32, i32* %m
	call void @printInt(i32 %94)
	%95 = load i32, i32* %m
	call void @printInt(i32 %95)
	ret i32 0
}
