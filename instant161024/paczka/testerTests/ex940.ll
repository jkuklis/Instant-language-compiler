declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%g = alloca i32
	%f = alloca i32
	%b = alloca i32
	%c = alloca i32
	%r = alloca i32
	%l = alloca i32
	%j = alloca i32
	%h = alloca i32
	%e = alloca i32
	%d = alloca i32
	%q = alloca i32
	%i = alloca i32
	%x = alloca i32
	%v = alloca i32
	%u = alloca i32
	%s = alloca i32
	%o = alloca i32
	%m = alloca i32
	%w = alloca i32
	%z = alloca i32
	%y = alloca i32
	%t = alloca i32
	%p = alloca i32
	%n = alloca i32
	%k = alloca i32
	call void @printInt(i32 38)
	store i32 7, i32* %a
	%0 = load i32, i32* %a
	store i32 %0, i32* %b
	call void @printInt(i32 46)
	%1 = load i32, i32* %a
	%2 = load i32, i32* %b
	%3 = sdiv i32 %1, %2
	call void @printInt(i32 %3)
	%4 = load i32, i32* %a
	%5 = mul i32 2, %4
	call void @printInt(i32 %5)
	%6 = load i32, i32* %a
	call void @printInt(i32 %6)
	call void @printInt(i32 35)
	store i32 31, i32* %c
	%7 = load i32, i32* %b
	%8 = load i32, i32* %c
	%9 = sub i32 52, %8
	%10 = sub i32 %7, %9
	call void @printInt(i32 %10)
	%11 = load i32, i32* %c
	%12 = load i32, i32* %c
	%13 = add i32 %11, %12
	%14 = add i32 2, %13
	%15 = sub i32 67, %14
	store i32 %15, i32* %d
	%16 = load i32, i32* %b
	store i32 %16, i32* %e
	%17 = load i32, i32* %b
	call void @printInt(i32 %17)
	%18 = load i32, i32* %c
	store i32 %18, i32* %f
	store i32 1, i32* %g
	call void @printInt(i32 80)
	%19 = load i32, i32* %g
	%20 = load i32, i32* %e
	%21 = sub i32 %20, 41
	%22 = load i32, i32* %e
	%23 = load i32, i32* %g
	%24 = sdiv i32 101, %23
	%25 = add i32 8, %24
	%26 = sub i32 %22, %25
	%27 = sub i32 %21, %26
	%28 = sdiv i32 %19, %27
	call void @printInt(i32 %28)
	call void @printInt(i32 18)
	call void @printInt(i32 52)
	%29 = load i32, i32* %d
	%30 = sub i32 %29, 35
	store i32 %30, i32* %h
	%31 = load i32, i32* %f
	call void @printInt(i32 %31)
	%32 = load i32, i32* %e
	%33 = mul i32 %32, 76
	store i32 %33, i32* %i
	store i32 88, i32* %j
	%34 = load i32, i32* %j
	%35 = load i32, i32* %c
	%36 = sub i32 %34, %35
	call void @printInt(i32 %36)
	store i32 64, i32* %k
	call void @printInt(i32 27)
	%37 = load i32, i32* %f
	call void @printInt(i32 %37)
	call void @printInt(i32 37)
	store i32 18, i32* %l
	call void @printInt(i32 44)
	%38 = load i32, i32* %d
	%39 = mul i32 %38, 60
	call void @printInt(i32 %39)
	%40 = load i32, i32* %h
	call void @printInt(i32 %40)
	%41 = load i32, i32* %g
	%42 = mul i32 39, %41
	call void @printInt(i32 %42)
	%43 = load i32, i32* %l
	%44 = sdiv i32 11, %43
	call void @printInt(i32 %44)
	%45 = load i32, i32* %l
	store i32 %45, i32* %m
	%46 = load i32, i32* %e
	store i32 %46, i32* %n
	%47 = load i32, i32* %f
	%48 = load i32, i32* %c
	%49 = mul i32 %47, %48
	store i32 %49, i32* %o
	%50 = load i32, i32* %a
	store i32 %50, i32* %p
	%51 = load i32, i32* %i
	call void @printInt(i32 %51)
	%52 = load i32, i32* %g
	store i32 %52, i32* %q
	%53 = load i32, i32* %m
	call void @printInt(i32 %53)
	call void @printInt(i32 22)
	%54 = load i32, i32* %g
	store i32 %54, i32* %r
	call void @printInt(i32 8)
	%55 = load i32, i32* %f
	call void @printInt(i32 %55)
	%56 = load i32, i32* %o
	%57 = load i32, i32* %h
	%58 = sub i32 %56, %57
	store i32 %58, i32* %s
	call void @printInt(i32 60)
	store i32 28, i32* %t
	%59 = load i32, i32* %o
	store i32 %59, i32* %u
	%60 = load i32, i32* %f
	%61 = sub i32 %60, 2
	call void @printInt(i32 %61)
	%62 = load i32, i32* %r
	store i32 %62, i32* %v
	call void @printInt(i32 3)
	%63 = load i32, i32* %q
	%64 = sdiv i32 37, %63
	call void @printInt(i32 %64)
	%65 = load i32, i32* %v
	call void @printInt(i32 %65)
	%66 = load i32, i32* %f
	call void @printInt(i32 %66)
	%67 = load i32, i32* %q
	%68 = add i32 15, %67
	call void @printInt(i32 %68)
	%69 = load i32, i32* %l
	call void @printInt(i32 %69)
	%70 = load i32, i32* %h
	call void @printInt(i32 %70)
	store i32 15, i32* %w
	%71 = load i32, i32* %b
	call void @printInt(i32 %71)
	%72 = load i32, i32* %a
	call void @printInt(i32 %72)
	%73 = load i32, i32* %m
	call void @printInt(i32 %73)
	call void @printInt(i32 13)
	%74 = load i32, i32* %a
	%75 = mul i32 2, %74
	call void @printInt(i32 %75)
	%76 = load i32, i32* %q
	%77 = add i32 %76, 31
	%78 = add i32 19, %77
	call void @printInt(i32 %78)
	call void @printInt(i32 23)
	%79 = load i32, i32* %b
	call void @printInt(i32 %79)
	%80 = load i32, i32* %r
	call void @printInt(i32 %80)
	call void @printInt(i32 6)
	call void @printInt(i32 18)
	%81 = load i32, i32* %g
	call void @printInt(i32 %81)
	call void @printInt(i32 6)
	%82 = load i32, i32* %j
	%83 = add i32 %82, 49
	call void @printInt(i32 %83)
	store i32 18, i32* %x
	%84 = load i32, i32* %a
	%85 = mul i32 %84, 60
	call void @printInt(i32 %85)
	%86 = load i32, i32* %u
	call void @printInt(i32 %86)
	%87 = load i32, i32* %l
	%88 = sdiv i32 48, %87
	%89 = load i32, i32* %i
	%90 = add i32 %88, %89
	%91 = sdiv i32 79, 12
	%92 = sdiv i32 %90, %91
	call void @printInt(i32 %92)
	%93 = load i32, i32* %x
	call void @printInt(i32 %93)
	%94 = load i32, i32* %u
	call void @printInt(i32 %94)
	call void @printInt(i32 83)
	%95 = load i32, i32* %d
	call void @printInt(i32 %95)
	call void @printInt(i32 10)
	%96 = load i32, i32* %v
	call void @printInt(i32 %96)
	%97 = load i32, i32* %f
	%98 = load i32, i32* %s
	%99 = mul i32 %98, 36
	%100 = load i32, i32* %r
	%101 = load i32, i32* %j
	%102 = sub i32 %100, %101
	%103 = mul i32 %99, %102
	%104 = add i32 10, 14
	%105 = mul i32 %103, %104
	%106 = sub i32 %97, %105
	call void @printInt(i32 %106)
	%107 = sub i32 21, 12
	%108 = load i32, i32* %w
	%109 = add i32 %107, %108
	call void @printInt(i32 %109)
	%110 = load i32, i32* %x
	call void @printInt(i32 %110)
	%111 = load i32, i32* %d
	call void @printInt(i32 %111)
	call void @printInt(i32 69)
	call void @printInt(i32 76)
	%112 = load i32, i32* %r
	call void @printInt(i32 %112)
	call void @printInt(i32 35)
	%113 = load i32, i32* %g
	call void @printInt(i32 %113)
	%114 = load i32, i32* %j
	call void @printInt(i32 %114)
	%115 = load i32, i32* %b
	call void @printInt(i32 %115)
	%116 = load i32, i32* %i
	store i32 %116, i32* %y
	%117 = load i32, i32* %h
	%118 = sdiv i32 %117, 29
	store i32 %118, i32* %z
	call void @printInt(i32 23)
	call void @printInt(i32 9)
	%119 = load i32, i32* %s
	call void @printInt(i32 %119)
	%120 = load i32, i32* %r
	call void @printInt(i32 %120)
	ret i32 0
}
