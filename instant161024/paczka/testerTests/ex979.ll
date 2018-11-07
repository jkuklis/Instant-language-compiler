declare void @printInt(i32)
define i32 @main() {
entry:
	%d = alloca i32
	%b = alloca i32
	%c = alloca i32
	%h = alloca i32
	%f = alloca i32
	%a = alloca i32
	%i = alloca i32
	%g = alloca i32
	%e = alloca i32
	%o = alloca i32
	%n = alloca i32
	%l = alloca i32
	%k = alloca i32
	%j = alloca i32
	%v = alloca i32
	%u = alloca i32
	%t = alloca i32
	%s = alloca i32
	%r = alloca i32
	%q = alloca i32
	%p = alloca i32
	%m = alloca i32
	store i32 52, i32* %a
	%0 = load i32, i32* %a
	store i32 %0, i32* %b
	call void @printInt(i32 54)
	%1 = load i32, i32* %b
	call void @printInt(i32 %1)
	%2 = sub i32 20, 25
	%3 = sdiv i32 27, %2
	store i32 %3, i32* %c
	call void @printInt(i32 28)
	%4 = load i32, i32* %b
	%5 = add i32 25, %4
	%6 = load i32, i32* %c
	%7 = load i32, i32* %c
	%8 = mul i32 %6, %7
	%9 = mul i32 %5, %8
	%10 = mul i32 %9, 101
	call void @printInt(i32 %10)
	%11 = load i32, i32* %a
	call void @printInt(i32 %11)
	%12 = sdiv i32 49, 31
	call void @printInt(i32 %12)
	%13 = load i32, i32* %c
	%14 = sub i32 33, %13
	%15 = load i32, i32* %c
	%16 = mul i32 41, %15
	%17 = sdiv i32 %14, %16
	call void @printInt(i32 %17)
	store i32 15, i32* %d
	%18 = load i32, i32* %d
	store i32 %18, i32* %e
	%19 = load i32, i32* %d
	store i32 %19, i32* %f
	%20 = load i32, i32* %b
	call void @printInt(i32 %20)
	%21 = load i32, i32* %b
	call void @printInt(i32 %21)
	call void @printInt(i32 56)
	%22 = load i32, i32* %a
	call void @printInt(i32 %22)
	%23 = load i32, i32* %d
	call void @printInt(i32 %23)
	call void @printInt(i32 59)
	%24 = load i32, i32* %d
	call void @printInt(i32 %24)
	%25 = load i32, i32* %f
	call void @printInt(i32 %25)
	store i32 65, i32* %g
	%26 = load i32, i32* %g
	%27 = load i32, i32* %b
	%28 = sub i32 %26, %27
	%29 = load i32, i32* %f
	%30 = sub i32 %28, %29
	call void @printInt(i32 %30)
	%31 = load i32, i32* %c
	call void @printInt(i32 %31)
	%32 = load i32, i32* %e
	%33 = load i32, i32* %f
	%34 = sub i32 %32, %33
	%35 = add i32 %34, 19
	%36 = mul i32 %35, 19
	call void @printInt(i32 %36)
	%37 = load i32, i32* %d
	%38 = sdiv i32 %37, 16
	call void @printInt(i32 %38)
	call void @printInt(i32 50)
	call void @printInt(i32 4)
	%39 = load i32, i32* %b
	store i32 %39, i32* %h
	%40 = load i32, i32* %h
	%41 = sdiv i32 37, %40
	store i32 %41, i32* %i
	%42 = load i32, i32* %h
	call void @printInt(i32 %42)
	call void @printInt(i32 34)
	call void @printInt(i32 69)
	%43 = load i32, i32* %d
	%44 = mul i32 33, %43
	call void @printInt(i32 %44)
	store i32 23, i32* %j
	store i32 1, i32* %k
	call void @printInt(i32 21)
	%45 = load i32, i32* %e
	call void @printInt(i32 %45)
	store i32 26, i32* %l
	call void @printInt(i32 38)
	%46 = sub i32 2, 19
	call void @printInt(i32 %46)
	call void @printInt(i32 27)
	call void @printInt(i32 47)
	%47 = load i32, i32* %h
	call void @printInt(i32 %47)
	store i32 86, i32* %m
	%48 = load i32, i32* %d
	%49 = load i32, i32* %h
	%50 = sub i32 %48, %49
	%51 = sub i32 %50, 26
	%52 = sdiv i32 %51, 20
	call void @printInt(i32 %52)
	call void @printInt(i32 17)
	call void @printInt(i32 73)
	%53 = load i32, i32* %j
	%54 = sdiv i32 %53, 6
	%55 = add i32 5, 15
	%56 = mul i32 %54, %55
	call void @printInt(i32 %56)
	call void @printInt(i32 85)
	%57 = load i32, i32* %f
	store i32 %57, i32* %n
	%58 = load i32, i32* %i
	call void @printInt(i32 %58)
	%59 = load i32, i32* %g
	call void @printInt(i32 %59)
	store i32 24, i32* %o
	store i32 16, i32* %p
	call void @printInt(i32 44)
	%60 = load i32, i32* %l
	%61 = sdiv i32 24, %60
	%62 = add i32 66, %61
	%63 = add i32 %62, 87
	%64 = add i32 97, %63
	%65 = load i32, i32* %i
	%66 = mul i32 %64, %65
	store i32 %66, i32* %q
	%67 = sub i32 16, 29
	call void @printInt(i32 %67)
	%68 = load i32, i32* %d
	%69 = sub i32 18, %68
	call void @printInt(i32 %69)
	call void @printInt(i32 28)
	%70 = load i32, i32* %d
	call void @printInt(i32 %70)
	%71 = load i32, i32* %k
	%72 = sdiv i32 11, %71
	store i32 %72, i32* %r
	call void @printInt(i32 17)
	store i32 47, i32* %s
	call void @printInt(i32 54)
	%73 = load i32, i32* %n
	store i32 %73, i32* %t
	%74 = load i32, i32* %o
	store i32 %74, i32* %u
	%75 = load i32, i32* %b
	%76 = sub i32 %75, 75
	call void @printInt(i32 %76)
	%77 = load i32, i32* %h
	%78 = mul i32 8, %77
	%79 = add i32 0, 45
	%80 = load i32, i32* %a
	%81 = add i32 %79, %80
	%82 = add i32 %81, 37
	%83 = mul i32 %78, %82
	store i32 %83, i32* %v
	%84 = load i32, i32* %c
	call void @printInt(i32 %84)
	call void @printInt(i32 67)
	ret i32 0
}
