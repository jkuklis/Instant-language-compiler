declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%j = alloca i32
	%c = alloca i32
	%h = alloca i32
	%e = alloca i32
	%m = alloca i32
	%l = alloca i32
	%f = alloca i32
	%d = alloca i32
	%n = alloca i32
	%k = alloca i32
	%b = alloca i32
	%s = alloca i32
	%r = alloca i32
	%x = alloca i32
	%v = alloca i32
	%u = alloca i32
	%q = alloca i32
	%o = alloca i32
	%i = alloca i32
	%g = alloca i32
	%z = alloca i32
	%y = alloca i32
	%w = alloca i32
	%t = alloca i32
	%p = alloca i32
	%aa = alloca i32
	call void @printInt(i32 19)
	call void @printInt(i32 42)
	%0 = sub i32 56, 34
	%1 = add i32 20, %0
	%2 = mul i32 %1, 47
	%3 = add i32 54, %2
	%4 = mul i32 6, 46
	%5 = mul i32 46, 47
	%6 = add i32 16, %5
	%7 = add i32 53, %6
	%8 = sub i32 40, %7
	%9 = sub i32 %8, 47
	%10 = mul i32 %4, %9
	%11 = sdiv i32 %3, %10
	%12 = sdiv i32 23, 38
	%13 = sub i32 %11, %12
	%14 = sub i32 %13, 32
	%15 = mul i32 18, 78
	%16 = sdiv i32 %15, 23
	%17 = sub i32 %16, 18
	%18 = mul i32 85, 47
	%19 = add i32 %18, 10
	%20 = mul i32 %19, 49
	%21 = mul i32 %17, %20
	%22 = mul i32 %21, 5
	%23 = mul i32 %22, 27
	%24 = sub i32 %14, %23
	%25 = mul i32 %24, 28
	%26 = sdiv i32 6, 53
	%27 = sub i32 %26, 18
	%28 = sub i32 %25, %27
	store i32 %28, i32* %a
	%29 = load i32, i32* %a
	call void @printInt(i32 %29)
	%30 = load i32, i32* %a
	%31 = sdiv i32 %30, 70
	store i32 %31, i32* %b
	call void @printInt(i32 18)
	%32 = load i32, i32* %a
	%33 = add i32 19, %32
	store i32 %33, i32* %c
	call void @printInt(i32 18)
	%34 = load i32, i32* %c
	store i32 %34, i32* %d
	%35 = load i32, i32* %b
	call void @printInt(i32 %35)
	%36 = load i32, i32* %b
	call void @printInt(i32 %36)
	%37 = load i32, i32* %c
	call void @printInt(i32 %37)
	call void @printInt(i32 26)
	%38 = load i32, i32* %d
	call void @printInt(i32 %38)
	store i32 65, i32* %e
	%39 = load i32, i32* %e
	call void @printInt(i32 %39)
	store i32 19, i32* %f
	call void @printInt(i32 1)
	store i32 38, i32* %g
	store i32 28, i32* %h
	%40 = load i32, i32* %e
	store i32 %40, i32* %i
	%41 = load i32, i32* %e
	call void @printInt(i32 %41)
	%42 = load i32, i32* %d
	store i32 %42, i32* %j
	call void @printInt(i32 24)
	call void @printInt(i32 64)
	%43 = load i32, i32* %a
	%44 = load i32, i32* %c
	%45 = sub i32 31, 2
	%46 = mul i32 %44, %45
	%47 = add i32 %43, %46
	call void @printInt(i32 %47)
	%48 = load i32, i32* %j
	%49 = mul i32 25, %48
	store i32 %49, i32* %k
	%50 = load i32, i32* %h
	call void @printInt(i32 %50)
	%51 = load i32, i32* %h
	%52 = add i32 81, %51
	%53 = add i32 26, %52
	call void @printInt(i32 %53)
	call void @printInt(i32 44)
	%54 = load i32, i32* %d
	call void @printInt(i32 %54)
	%55 = load i32, i32* %j
	%56 = sdiv i32 50, 91
	%57 = sub i32 %55, %56
	call void @printInt(i32 %57)
	%58 = load i32, i32* %e
	%59 = sdiv i32 %58, 69
	call void @printInt(i32 %59)
	%60 = load i32, i32* %h
	call void @printInt(i32 %60)
	call void @printInt(i32 16)
	%61 = load i32, i32* %h
	call void @printInt(i32 %61)
	%62 = load i32, i32* %j
	call void @printInt(i32 %62)
	%63 = load i32, i32* %e
	%64 = load i32, i32* %f
	%65 = add i32 %63, %64
	store i32 %65, i32* %l
	store i32 37, i32* %m
	%66 = load i32, i32* %g
	%67 = add i32 1, 74
	%68 = sdiv i32 %66, %67
	call void @printInt(i32 %68)
	call void @printInt(i32 46)
	%69 = load i32, i32* %k
	%70 = load i32, i32* %l
	%71 = sdiv i32 %70, 40
	%72 = sub i32 %69, %71
	call void @printInt(i32 %72)
	%73 = load i32, i32* %f
	store i32 %73, i32* %n
	%74 = load i32, i32* %n
	%75 = sub i32 44, 56
	%76 = load i32, i32* %m
	%77 = load i32, i32* %l
	%78 = mul i32 22, %77
	%79 = load i32, i32* %j
	%80 = mul i32 %78, %79
	%81 = sub i32 %80, 22
	%82 = mul i32 %81, 17
	%83 = load i32, i32* %a
	%84 = add i32 %82, %83
	%85 = load i32, i32* %l
	%86 = mul i32 %84, %85
	%87 = load i32, i32* %m
	%88 = load i32, i32* %l
	%89 = mul i32 %87, %88
	%90 = sub i32 92, %89
	%91 = add i32 %86, %90
	%92 = sub i32 %91, 4
	%93 = load i32, i32* %n
	%94 = add i32 %92, %93
	%95 = sdiv i32 %76, %94
	%96 = sub i32 %75, %95
	%97 = mul i32 13, %96
	%98 = sub i32 13, %97
	%99 = sdiv i32 %98, 63
	%100 = add i32 %74, %99
	store i32 %100, i32* %o
	%101 = load i32, i32* %m
	store i32 %101, i32* %p
	%102 = load i32, i32* %d
	store i32 %102, i32* %q
	%103 = load i32, i32* %a
	call void @printInt(i32 %103)
	store i32 43, i32* %r
	call void @printInt(i32 13)
	%104 = load i32, i32* %o
	store i32 %104, i32* %s
	%105 = load i32, i32* %k
	%106 = sdiv i32 %105, 48
	%107 = sdiv i32 91, %106
	%108 = mul i32 39, %107
	%109 = load i32, i32* %f
	%110 = load i32, i32* %a
	%111 = load i32, i32* %s
	%112 = load i32, i32* %k
	%113 = add i32 %111, %112
	%114 = sdiv i32 %110, %113
	%115 = sub i32 %109, %114
	%116 = add i32 %108, %115
	%117 = load i32, i32* %h
	%118 = mul i32 24, %117
	%119 = sub i32 %116, %118
	store i32 %119, i32* %t
	store i32 12, i32* %u
	%120 = load i32, i32* %r
	call void @printInt(i32 %120)
	%121 = load i32, i32* %m
	store i32 %121, i32* %v
	%122 = load i32, i32* %f
	%123 = sdiv i32 %122, 43
	%124 = load i32, i32* %s
	%125 = load i32, i32* %c
	%126 = load i32, i32* %u
	%127 = load i32, i32* %n
	%128 = load i32, i32* %c
	%129 = sdiv i32 %127, %128
	%130 = add i32 %126, %129
	%131 = sub i32 22, %130
	%132 = sub i32 %125, %131
	%133 = add i32 %124, %132
	%134 = sub i32 %123, %133
	%135 = sub i32 %134, 8
	store i32 %135, i32* %w
	call void @printInt(i32 17)
	store i32 6, i32* %x
	%136 = load i32, i32* %x
	call void @printInt(i32 %136)
	call void @printInt(i32 19)
	%137 = load i32, i32* %v
	call void @printInt(i32 %137)
	call void @printInt(i32 21)
	store i32 37, i32* %y
	%138 = load i32, i32* %i
	store i32 %138, i32* %z
	%139 = add i32 13, 43
	call void @printInt(i32 %139)
	store i32 12, i32* %aa
	%140 = load i32, i32* %b
	call void @printInt(i32 %140)
	%141 = add i32 77, 46
	%142 = mul i32 %141, 67
	call void @printInt(i32 %142)
	call void @printInt(i32 18)
	call void @printInt(i32 64)
	call void @printInt(i32 10)
	%143 = sub i32 63, 36
	%144 = load i32, i32* %c
	%145 = mul i32 %143, %144
	call void @printInt(i32 %145)
	%146 = load i32, i32* %q
	call void @printInt(i32 %146)
	%147 = load i32, i32* %j
	%148 = add i32 %147, 47
	call void @printInt(i32 %148)
	%149 = load i32, i32* %r
	%150 = load i32, i32* %j
	%151 = sdiv i32 43, %150
	%152 = mul i32 %149, %151
	%153 = sub i32 22, %152
	call void @printInt(i32 %153)
	ret i32 0
}
