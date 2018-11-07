declare void @printInt(i32)
define i32 @main() {
entry:
	%c = alloca i32
	%h = alloca i32
	%f = alloca i32
	%e = alloca i32
	%b = alloca i32
	%a = alloca i32
	%i = alloca i32
	%d = alloca i32
	%j = alloca i32
	%p = alloca i32
	%n = alloca i32
	%m = alloca i32
	%k = alloca i32
	%t = alloca i32
	%r = alloca i32
	%q = alloca i32
	%l = alloca i32
	%g = alloca i32
	%x = alloca i32
	%w = alloca i32
	%v = alloca i32
	%u = alloca i32
	%s = alloca i32
	%o = alloca i32
	call void @printInt(i32 2)
	call void @printInt(i32 19)
	call void @printInt(i32 6)
	store i32 47, i32* %a
	store i32 21, i32* %b
	%0 = load i32, i32* %b
	call void @printInt(i32 %0)
	%1 = load i32, i32* %a
	call void @printInt(i32 %1)
	%2 = load i32, i32* %b
	store i32 %2, i32* %c
	%3 = load i32, i32* %a
	call void @printInt(i32 %3)
	%4 = load i32, i32* %b
	call void @printInt(i32 %4)
	%5 = load i32, i32* %a
	%6 = mul i32 86, %5
	store i32 %6, i32* %d
	call void @printInt(i32 55)
	call void @printInt(i32 21)
	%7 = load i32, i32* %c
	%8 = mul i32 99, %7
	store i32 %8, i32* %e
	%9 = mul i32 14, 3
	%10 = mul i32 90, %9
	%11 = add i32 %10, 7
	call void @printInt(i32 %11)
	%12 = load i32, i32* %c
	%13 = sub i32 25, %12
	store i32 %13, i32* %f
	call void @printInt(i32 29)
	%14 = load i32, i32* %c
	%15 = sub i32 17, %14
	store i32 %15, i32* %g
	%16 = load i32, i32* %f
	%17 = add i32 %16, 72
	%18 = add i32 %17, 25
	call void @printInt(i32 %18)
	call void @printInt(i32 23)
	call void @printInt(i32 79)
	%19 = load i32, i32* %a
	%20 = sub i32 54, %19
	%21 = add i32 %20, 21
	%22 = mul i32 17, %21
	call void @printInt(i32 %22)
	call void @printInt(i32 51)
	store i32 18, i32* %h
	%23 = mul i32 51, 46
	%24 = add i32 50, %23
	call void @printInt(i32 %24)
	%25 = load i32, i32* %e
	call void @printInt(i32 %25)
	%26 = load i32, i32* %e
	call void @printInt(i32 %26)
	call void @printInt(i32 36)
	%27 = load i32, i32* %d
	call void @printInt(i32 %27)
	%28 = load i32, i32* %b
	call void @printInt(i32 %28)
	store i32 71, i32* %i
	%29 = add i32 10, 61
	%30 = mul i32 %29, 12
	call void @printInt(i32 %30)
	%31 = load i32, i32* %f
	%32 = sdiv i32 %31, 63
	call void @printInt(i32 %32)
	%33 = load i32, i32* %d
	%34 = sub i32 13, 14
	%35 = add i32 %33, %34
	call void @printInt(i32 %35)
	%36 = load i32, i32* %b
	call void @printInt(i32 %36)
	call void @printInt(i32 60)
	call void @printInt(i32 15)
	%37 = load i32, i32* %c
	call void @printInt(i32 %37)
	%38 = load i32, i32* %b
	%39 = load i32, i32* %a
	%40 = add i32 %38, %39
	%41 = mul i32 %40, 30
	%42 = mul i32 37, %41
	%43 = sdiv i32 %42, 27
	call void @printInt(i32 %43)
	%44 = load i32, i32* %e
	call void @printInt(i32 %44)
	%45 = add i32 27, 67
	call void @printInt(i32 %45)
	store i32 14, i32* %j
	call void @printInt(i32 34)
	%46 = load i32, i32* %f
	call void @printInt(i32 %46)
	%47 = load i32, i32* %j
	%48 = load i32, i32* %f
	%49 = load i32, i32* %f
	%50 = sub i32 %48, %49
	%51 = load i32, i32* %e
	%52 = add i32 %50, %51
	%53 = sdiv i32 %47, %52
	%54 = add i32 %53, 2
	%55 = load i32, i32* %h
	%56 = add i32 %54, %55
	%57 = sdiv i32 30, %56
	call void @printInt(i32 %57)
	%58 = load i32, i32* %h
	call void @printInt(i32 %58)
	%59 = load i32, i32* %h
	call void @printInt(i32 %59)
	call void @printInt(i32 2)
	%60 = load i32, i32* %f
	call void @printInt(i32 %60)
	%61 = load i32, i32* %i
	store i32 %61, i32* %k
	%62 = load i32, i32* %d
	%63 = load i32, i32* %k
	%64 = sub i32 %62, %63
	%65 = mul i32 %64, 62
	call void @printInt(i32 %65)
	%66 = load i32, i32* %e
	call void @printInt(i32 %66)
	%67 = load i32, i32* %j
	%68 = mul i32 22, 84
	%69 = add i32 %68, 69
	%70 = sub i32 %67, %69
	%71 = sdiv i32 3, 59
	%72 = sub i32 %71, 3
	%73 = sdiv i32 %70, %72
	store i32 %73, i32* %l
	call void @printInt(i32 24)
	%74 = load i32, i32* %i
	call void @printInt(i32 %74)
	%75 = load i32, i32* %i
	call void @printInt(i32 %75)
	store i32 50, i32* %m
	%76 = load i32, i32* %k
	store i32 %76, i32* %n
	call void @printInt(i32 13)
	%77 = mul i32 18, 33
	call void @printInt(i32 %77)
	%78 = load i32, i32* %n
	store i32 %78, i32* %o
	store i32 40, i32* %p
	call void @printInt(i32 15)
	%79 = load i32, i32* %h
	call void @printInt(i32 %79)
	%80 = mul i32 25, 55
	%81 = load i32, i32* %n
	%82 = mul i32 %80, %81
	%83 = sdiv i32 37, %82
	call void @printInt(i32 %83)
	%84 = load i32, i32* %c
	%85 = load i32, i32* %e
	%86 = sub i32 7, %85
	%87 = add i32 %84, %86
	call void @printInt(i32 %87)
	call void @printInt(i32 1)
	%88 = mul i32 56, 43
	call void @printInt(i32 %88)
	store i32 80, i32* %q
	%89 = load i32, i32* %a
	call void @printInt(i32 %89)
	call void @printInt(i32 24)
	call void @printInt(i32 45)
	%90 = load i32, i32* %c
	store i32 %90, i32* %r
	%91 = load i32, i32* %j
	%92 = load i32, i32* %c
	%93 = add i32 %91, %92
	%94 = mul i32 51, %93
	%95 = load i32, i32* %m
	%96 = add i32 %94, %95
	call void @printInt(i32 %96)
	%97 = sdiv i32 62, 22
	%98 = add i32 31, 78
	%99 = add i32 %97, %98
	%100 = sdiv i32 %99, 47
	%101 = mul i32 60, %100
	call void @printInt(i32 %101)
	%102 = load i32, i32* %r
	call void @printInt(i32 %102)
	%103 = load i32, i32* %q
	%104 = mul i32 22, 22
	%105 = sdiv i32 %103, %104
	call void @printInt(i32 %105)
	call void @printInt(i32 46)
	call void @printInt(i32 61)
	store i32 30, i32* %s
	%106 = load i32, i32* %p
	store i32 %106, i32* %t
	call void @printInt(i32 70)
	call void @printInt(i32 59)
	%107 = load i32, i32* %m
	%108 = load i32, i32* %t
	%109 = sdiv i32 %107, %108
	call void @printInt(i32 %109)
	call void @printInt(i32 34)
	store i32 24, i32* %u
	%110 = load i32, i32* %h
	%111 = load i32, i32* %p
	%112 = load i32, i32* %l
	%113 = sub i32 %111, %112
	%114 = sdiv i32 %110, %113
	%115 = add i32 %114, 55
	%116 = add i32 %115, 12
	store i32 %116, i32* %v
	%117 = load i32, i32* %d
	call void @printInt(i32 %117)
	call void @printInt(i32 22)
	%118 = mul i32 17, 13
	%119 = sdiv i32 %118, 29
	call void @printInt(i32 %119)
	%120 = load i32, i32* %g
	%121 = load i32, i32* %h
	%122 = mul i32 %120, %121
	call void @printInt(i32 %122)
	%123 = load i32, i32* %i
	store i32 %123, i32* %w
	store i32 37, i32* %x
	ret i32 0
}
