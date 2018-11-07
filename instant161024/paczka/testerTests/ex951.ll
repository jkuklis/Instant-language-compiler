declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%f = alloca i32
	%e = alloca i32
	%b = alloca i32
	%g = alloca i32
	%d = alloca i32
	%c = alloca i32
	%l = alloca i32
	%j = alloca i32
	%i = alloca i32
	%h = alloca i32
	%r = alloca i32
	%q = alloca i32
	%p = alloca i32
	%o = alloca i32
	%n = alloca i32
	%m = alloca i32
	%k = alloca i32
	store i32 19, i32* %a
	%0 = load i32, i32* %a
	call void @printInt(i32 %0)
	store i32 32, i32* %b
	call void @printInt(i32 13)
	call void @printInt(i32 28)
	%1 = sdiv i32 3, 69
	store i32 %1, i32* %c
	%2 = load i32, i32* %b
	%3 = mul i32 47, %2
	%4 = sdiv i32 15, %3
	%5 = load i32, i32* %b
	%6 = add i32 %4, %5
	store i32 %6, i32* %d
	%7 = load i32, i32* %a
	%8 = add i32 11, %7
	%9 = load i32, i32* %d
	%10 = load i32, i32* %a
	%11 = sdiv i32 8, 18
	%12 = add i32 %10, %11
	%13 = sdiv i32 %9, %12
	%14 = add i32 %13, 49
	%15 = sdiv i32 %8, %14
	%16 = load i32, i32* %a
	%17 = sdiv i32 %15, %16
	%18 = sdiv i32 %17, 12
	store i32 %18, i32* %e
	%19 = load i32, i32* %e
	call void @printInt(i32 %19)
	store i32 20, i32* %f
	call void @printInt(i32 13)
	%20 = load i32, i32* %d
	call void @printInt(i32 %20)
	%21 = load i32, i32* %f
	call void @printInt(i32 %21)
	store i32 84, i32* %g
	%22 = load i32, i32* %c
	store i32 %22, i32* %h
	%23 = load i32, i32* %e
	call void @printInt(i32 %23)
	%24 = load i32, i32* %d
	%25 = load i32, i32* %f
	%26 = sub i32 %24, %25
	call void @printInt(i32 %26)
	%27 = sdiv i32 5, 13
	store i32 %27, i32* %i
	call void @printInt(i32 39)
	call void @printInt(i32 67)
	%28 = sdiv i32 50, 24
	call void @printInt(i32 %28)
	%29 = load i32, i32* %g
	%30 = add i32 64, 66
	%31 = mul i32 16, 10
	%32 = sub i32 %30, %31
	%33 = add i32 21, %32
	%34 = sdiv i32 %29, %33
	%35 = sub i32 32, 28
	%36 = add i32 54, %35
	%37 = add i32 %36, 4
	%38 = mul i32 %34, %37
	call void @printInt(i32 %38)
	%39 = load i32, i32* %c
	%40 = load i32, i32* %c
	%41 = mul i32 %39, %40
	%42 = load i32, i32* %a
	%43 = load i32, i32* %d
	%44 = sub i32 %42, %43
	%45 = sub i32 0, %44
	%46 = mul i32 %41, %45
	%47 = load i32, i32* %g
	%48 = add i32 %46, %47
	call void @printInt(i32 %48)
	%49 = load i32, i32* %a
	%50 = add i32 13, %49
	%51 = load i32, i32* %h
	%52 = mul i32 %50, %51
	call void @printInt(i32 %52)
	%53 = load i32, i32* %f
	%54 = add i32 61, %53
	%55 = sdiv i32 13, 15
	%56 = add i32 %54, %55
	call void @printInt(i32 %56)
	%57 = load i32, i32* %e
	%58 = sub i32 %57, 33
	%59 = load i32, i32* %e
	%60 = mul i32 31, %59
	%61 = sub i32 %58, %60
	call void @printInt(i32 %61)
	%62 = load i32, i32* %b
	call void @printInt(i32 %62)
	call void @printInt(i32 43)
	store i32 33, i32* %j
	%63 = sub i32 37, 30
	%64 = add i32 %63, 7
	call void @printInt(i32 %64)
	%65 = add i32 12, 24
	%66 = add i32 %65, 5
	%67 = add i32 1, %66
	%68 = load i32, i32* %g
	%69 = sdiv i32 %68, 81
	%70 = add i32 %67, %69
	%71 = load i32, i32* %a
	%72 = mul i32 %70, %71
	call void @printInt(i32 %72)
	call void @printInt(i32 7)
	%73 = load i32, i32* %e
	call void @printInt(i32 %73)
	call void @printInt(i32 25)
	%74 = load i32, i32* %f
	%75 = sub i32 11, %74
	store i32 %75, i32* %k
	%76 = load i32, i32* %j
	call void @printInt(i32 %76)
	%77 = mul i32 23, 99
	%78 = sdiv i32 %77, 10
	call void @printInt(i32 %78)
	call void @printInt(i32 15)
	%79 = load i32, i32* %f
	%80 = mul i32 9, %79
	%81 = add i32 34, 20
	%82 = sdiv i32 %80, %81
	store i32 %82, i32* %l
	%83 = load i32, i32* %l
	store i32 %83, i32* %m
	%84 = load i32, i32* %g
	call void @printInt(i32 %84)
	%85 = load i32, i32* %f
	store i32 %85, i32* %n
	store i32 34, i32* %o
	%86 = load i32, i32* %c
	call void @printInt(i32 %86)
	%87 = load i32, i32* %e
	call void @printInt(i32 %87)
	%88 = load i32, i32* %i
	%89 = mul i32 %88, 23
	store i32 %89, i32* %p
	%90 = load i32, i32* %b
	call void @printInt(i32 %90)
	store i32 73, i32* %q
	%91 = load i32, i32* %l
	call void @printInt(i32 %91)
	store i32 35, i32* %r
	%92 = load i32, i32* %b
	call void @printInt(i32 %92)
	ret i32 0
}
