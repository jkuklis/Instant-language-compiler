declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%b = alloca i32
	%e = alloca i32
	%d = alloca i32
	%g = alloca i32
	%f = alloca i32
	%i = alloca i32
	%h = alloca i32
	%l = alloca i32
	%j = alloca i32
	%p = alloca i32
	%o = alloca i32
	%n = alloca i32
	%m = alloca i32
	%k = alloca i32
	call void @printInt(i32 0)
	%0 = sdiv i32 24, 23
	%1 = sub i32 20, %0
	%2 = sub i32 34, %1
	%3 = sub i32 33, %2
	%4 = sub i32 %3, 41
	%5 = mul i32 %4, 1
	store i32 %5, i32* %a
	%6 = load i32, i32* %a
	%7 = load i32, i32* %a
	%8 = sub i32 %6, %7
	call void @printInt(i32 %8)
	%9 = load i32, i32* %a
	call void @printInt(i32 %9)
	%10 = load i32, i32* %a
	%11 = add i32 66, %10
	%12 = mul i32 5, %11
	%13 = add i32 42, %12
	call void @printInt(i32 %13)
	%14 = load i32, i32* %a
	call void @printInt(i32 %14)
	store i32 14, i32* %b
	%15 = load i32, i32* %a
	store i32 %15, i32* %c
	%16 = load i32, i32* %b
	call void @printInt(i32 %16)
	call void @printInt(i32 77)
	call void @printInt(i32 50)
	%17 = load i32, i32* %c
	call void @printInt(i32 %17)
	%18 = load i32, i32* %c
	%19 = sub i32 6, %18
	call void @printInt(i32 %19)
	store i32 36, i32* %d
	call void @printInt(i32 21)
	%20 = load i32, i32* %b
	%21 = load i32, i32* %d
	%22 = sub i32 %21, 42
	%23 = load i32, i32* %d
	%24 = sub i32 %22, %23
	%25 = add i32 %24, 62
	%26 = sdiv i32 %20, %25
	%27 = sub i32 16, 30
	%28 = load i32, i32* %b
	%29 = add i32 %27, %28
	%30 = mul i32 57, 15
	%31 = sub i32 %29, %30
	%32 = sdiv i32 56, %31
	%33 = mul i32 %26, %32
	%34 = load i32, i32* %b
	%35 = sub i32 %33, %34
	%36 = add i32 57, 21
	%37 = mul i32 %35, %36
	call void @printInt(i32 %37)
	%38 = load i32, i32* %d
	%39 = add i32 29, %38
	call void @printInt(i32 %39)
	%40 = load i32, i32* %a
	store i32 %40, i32* %e
	%41 = load i32, i32* %c
	%42 = mul i32 59, %41
	%43 = sdiv i32 %42, 26
	call void @printInt(i32 %43)
	%44 = load i32, i32* %b
	call void @printInt(i32 %44)
	%45 = load i32, i32* %e
	call void @printInt(i32 %45)
	%46 = load i32, i32* %e
	%47 = add i32 1, 44
	%48 = add i32 %47, 6
	%49 = load i32, i32* %e
	%50 = load i32, i32* %b
	%51 = add i32 %49, %50
	%52 = sdiv i32 %48, %51
	%53 = sdiv i32 95, %52
	%54 = sub i32 %46, %53
	call void @printInt(i32 %54)
	%55 = load i32, i32* %c
	%56 = load i32, i32* %b
	%57 = mul i32 %56, 64
	%58 = sdiv i32 %55, %57
	%59 = add i32 27, %58
	%60 = add i32 %59, 7
	%61 = mul i32 11, %60
	call void @printInt(i32 %61)
	store i32 43, i32* %f
	call void @printInt(i32 15)
	%62 = load i32, i32* %f
	call void @printInt(i32 %62)
	store i32 77, i32* %g
	%63 = load i32, i32* %a
	call void @printInt(i32 %63)
	call void @printInt(i32 11)
	%64 = load i32, i32* %f
	%65 = load i32, i32* %a
	%66 = add i32 %64, %65
	%67 = load i32, i32* %e
	%68 = sub i32 %66, %67
	call void @printInt(i32 %68)
	call void @printInt(i32 50)
	call void @printInt(i32 6)
	%69 = load i32, i32* %a
	call void @printInt(i32 %69)
	%70 = load i32, i32* %d
	%71 = add i32 22, %70
	store i32 %71, i32* %h
	call void @printInt(i32 43)
	%72 = load i32, i32* %b
	%73 = mul i32 %72, 3
	%74 = load i32, i32* %h
	%75 = mul i32 2, 34
	%76 = load i32, i32* %a
	%77 = mul i32 %76, 15
	%78 = sub i32 %75, %77
	%79 = mul i32 %74, %78
	%80 = mul i32 %73, %79
	%81 = mul i32 13, %80
	%82 = mul i32 72, %81
	call void @printInt(i32 %82)
	%83 = load i32, i32* %b
	call void @printInt(i32 %83)
	%84 = load i32, i32* %g
	call void @printInt(i32 %84)
	call void @printInt(i32 1)
	%85 = load i32, i32* %d
	%86 = add i32 61, %85
	%87 = load i32, i32* %d
	%88 = mul i32 %86, %87
	call void @printInt(i32 %88)
	%89 = load i32, i32* %e
	%90 = sdiv i32 %89, 45
	%91 = load i32, i32* %c
	%92 = sdiv i32 %90, %91
	call void @printInt(i32 %92)
	call void @printInt(i32 3)
	store i32 29, i32* %i
	%93 = load i32, i32* %i
	%94 = sub i32 55, %93
	%95 = load i32, i32* %e
	%96 = sdiv i32 107, %95
	%97 = load i32, i32* %f
	%98 = sub i32 %96, %97
	%99 = sub i32 %94, %98
	%100 = load i32, i32* %g
	%101 = load i32, i32* %c
	%102 = sub i32 11, %101
	%103 = mul i32 %100, %102
	%104 = mul i32 %99, %103
	call void @printInt(i32 %104)
	call void @printInt(i32 58)
	%105 = load i32, i32* %g
	%106 = add i32 77, %105
	store i32 %106, i32* %j
	%107 = load i32, i32* %j
	call void @printInt(i32 %107)
	%108 = load i32, i32* %e
	%109 = mul i32 %108, 10
	call void @printInt(i32 %109)
	%110 = load i32, i32* %d
	call void @printInt(i32 %110)
	call void @printInt(i32 48)
	%111 = load i32, i32* %h
	%112 = load i32, i32* %g
	%113 = sdiv i32 %111, %112
	call void @printInt(i32 %113)
	%114 = load i32, i32* %i
	call void @printInt(i32 %114)
	%115 = load i32, i32* %a
	call void @printInt(i32 %115)
	call void @printInt(i32 25)
	store i32 60, i32* %k
	%116 = load i32, i32* %g
	%117 = mul i32 %116, 1
	call void @printInt(i32 %117)
	%118 = add i32 13, 2
	%119 = load i32, i32* %c
	%120 = sdiv i32 %118, %119
	store i32 %120, i32* %l
	%121 = load i32, i32* %g
	call void @printInt(i32 %121)
	store i32 2, i32* %m
	%122 = load i32, i32* %i
	call void @printInt(i32 %122)
	call void @printInt(i32 55)
	%123 = load i32, i32* %e
	%124 = sub i32 15, 14
	%125 = load i32, i32* %h
	%126 = sdiv i32 %124, %125
	%127 = mul i32 %123, %126
	call void @printInt(i32 %127)
	%128 = load i32, i32* %f
	%129 = load i32, i32* %c
	%130 = add i32 %128, %129
	%131 = add i32 %130, 65
	call void @printInt(i32 %131)
	call void @printInt(i32 21)
	%132 = load i32, i32* %l
	call void @printInt(i32 %132)
	call void @printInt(i32 7)
	%133 = load i32, i32* %d
	call void @printInt(i32 %133)
	store i32 45, i32* %n
	%134 = load i32, i32* %g
	store i32 %134, i32* %o
	call void @printInt(i32 35)
	%135 = sdiv i32 4, 49
	call void @printInt(i32 %135)
	%136 = load i32, i32* %c
	call void @printInt(i32 %136)
	call void @printInt(i32 20)
	%137 = load i32, i32* %c
	%138 = load i32, i32* %l
	%139 = add i32 %137, %138
	call void @printInt(i32 %139)
	%140 = load i32, i32* %c
	store i32 %140, i32* %p
	%141 = mul i32 46, 38
	call void @printInt(i32 %141)
	ret i32 0
}
