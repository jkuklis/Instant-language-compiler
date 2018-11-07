declare void @printInt(i32)
define i32 @main() {
entry:
	%g = alloca i32
	%e = alloca i32
	%d = alloca i32
	%h = alloca i32
	%i = alloca i32
	%a = alloca i32
	%f = alloca i32
	%c = alloca i32
	%k = alloca i32
	%b = alloca i32
	%r = alloca i32
	%n = alloca i32
	%m = alloca i32
	%j = alloca i32
	%y = alloca i32
	%x = alloca i32
	%w = alloca i32
	%v = alloca i32
	%u = alloca i32
	%t = alloca i32
	%s = alloca i32
	%q = alloca i32
	%p = alloca i32
	%o = alloca i32
	%l = alloca i32
	call void @printInt(i32 41)
	call void @printInt(i32 26)
	%0 = add i32 19, 45
	call void @printInt(i32 %0)
	store i32 11, i32* %a
	store i32 1, i32* %b
	%1 = load i32, i32* %b
	%2 = sdiv i32 43, %1
	call void @printInt(i32 %2)
	%3 = load i32, i32* %b
	store i32 %3, i32* %c
	store i32 13, i32* %d
	%4 = mul i32 32, 31
	store i32 %4, i32* %e
	%5 = load i32, i32* %a
	%6 = add i32 %5, 5
	%7 = load i32, i32* %e
	%8 = sub i32 %6, %7
	%9 = mul i32 27, %8
	store i32 %9, i32* %f
	%10 = load i32, i32* %c
	call void @printInt(i32 %10)
	call void @printInt(i32 7)
	store i32 21, i32* %g
	%11 = load i32, i32* %e
	%12 = mul i32 %11, 30
	call void @printInt(i32 %12)
	call void @printInt(i32 17)
	store i32 13, i32* %h
	%13 = load i32, i32* %f
	store i32 %13, i32* %i
	%14 = load i32, i32* %e
	call void @printInt(i32 %14)
	%15 = load i32, i32* %g
	call void @printInt(i32 %15)
	call void @printInt(i32 3)
	%16 = load i32, i32* %f
	%17 = load i32, i32* %h
	%18 = sub i32 %16, %17
	%19 = load i32, i32* %g
	%20 = add i32 %18, %19
	%21 = load i32, i32* %i
	%22 = add i32 %20, %21
	%23 = load i32, i32* %e
	%24 = sdiv i32 0, %23
	%25 = load i32, i32* %d
	%26 = mul i32 %24, %25
	%27 = load i32, i32* %c
	%28 = sdiv i32 %26, %27
	%29 = sub i32 %22, %28
	%30 = load i32, i32* %g
	%31 = add i32 %29, %30
	call void @printInt(i32 %31)
	%32 = load i32, i32* %e
	%33 = load i32, i32* %c
	%34 = mul i32 %32, %33
	call void @printInt(i32 %34)
	call void @printInt(i32 13)
	%35 = load i32, i32* %e
	store i32 %35, i32* %j
	%36 = load i32, i32* %d
	%37 = load i32, i32* %j
	%38 = sdiv i32 %36, %37
	call void @printInt(i32 %38)
	%39 = load i32, i32* %d
	%40 = mul i32 %39, 77
	call void @printInt(i32 %40)
	%41 = load i32, i32* %d
	%42 = add i32 52, %41
	%43 = load i32, i32* %i
	%44 = add i32 5, %43
	%45 = sub i32 %42, %44
	%46 = add i32 28, %45
	%47 = load i32, i32* %f
	%48 = mul i32 %46, %47
	%49 = add i32 24, %48
	%50 = sdiv i32 7, %49
	call void @printInt(i32 %50)
	%51 = load i32, i32* %i
	store i32 %51, i32* %k
	%52 = load i32, i32* %g
	%53 = sub i32 %52, 5
	call void @printInt(i32 %53)
	%54 = sdiv i32 72, 81
	%55 = add i32 36, %54
	%56 = load i32, i32* %i
	%57 = sub i32 18, %56
	%58 = load i32, i32* %h
	%59 = sdiv i32 %57, %58
	%60 = mul i32 %55, %59
	%61 = sdiv i32 28, %60
	%62 = mul i32 13, %61
	store i32 %62, i32* %l
	%63 = load i32, i32* %g
	call void @printInt(i32 %63)
	%64 = load i32, i32* %a
	store i32 %64, i32* %m
	%65 = load i32, i32* %d
	%66 = load i32, i32* %a
	%67 = sdiv i32 %65, %66
	store i32 %67, i32* %n
	%68 = load i32, i32* %g
	store i32 %68, i32* %o
	store i32 70, i32* %p
	call void @printInt(i32 35)
	call void @printInt(i32 29)
	store i32 10, i32* %q
	%69 = load i32, i32* %h
	call void @printInt(i32 %69)
	call void @printInt(i32 29)
	store i32 27, i32* %r
	call void @printInt(i32 13)
	%70 = load i32, i32* %h
	store i32 %70, i32* %s
	call void @printInt(i32 1)
	%71 = load i32, i32* %h
	call void @printInt(i32 %71)
	call void @printInt(i32 0)
	%72 = load i32, i32* %m
	call void @printInt(i32 %72)
	call void @printInt(i32 17)
	%73 = sdiv i32 11, 20
	call void @printInt(i32 %73)
	%74 = load i32, i32* %k
	call void @printInt(i32 %74)
	%75 = load i32, i32* %k
	call void @printInt(i32 %75)
	%76 = load i32, i32* %g
	store i32 %76, i32* %t
	%77 = load i32, i32* %g
	call void @printInt(i32 %77)
	call void @printInt(i32 53)
	store i32 11, i32* %u
	%78 = load i32, i32* %r
	call void @printInt(i32 %78)
	call void @printInt(i32 1)
	store i32 31, i32* %v
	call void @printInt(i32 41)
	%79 = load i32, i32* %n
	store i32 %79, i32* %w
	call void @printInt(i32 33)
	store i32 77, i32* %x
	%80 = load i32, i32* %d
	%81 = load i32, i32* %a
	%82 = sdiv i32 %80, %81
	call void @printInt(i32 %82)
	%83 = load i32, i32* %e
	store i32 %83, i32* %y
	ret i32 0
}
