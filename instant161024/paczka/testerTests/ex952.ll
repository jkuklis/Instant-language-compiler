declare void @printInt(i32)
define i32 @main() {
entry:
	%e = alloca i32
	%c = alloca i32
	%b = alloca i32
	%a = alloca i32
	%h = alloca i32
	%f = alloca i32
	%k = alloca i32
	%d = alloca i32
	%s = alloca i32
	%m = alloca i32
	%j = alloca i32
	%p = alloca i32
	%n = alloca i32
	%g = alloca i32
	%y = alloca i32
	%w = alloca i32
	%u = alloca i32
	%t = alloca i32
	%r = alloca i32
	%z = alloca i32
	%x = alloca i32
	%v = alloca i32
	%q = alloca i32
	%o = alloca i32
	%l = alloca i32
	%i = alloca i32
	%0 = add i32 30, 40
	%1 = sub i32 33, %0
	%2 = add i32 %1, 60
	%3 = mul i32 3, %2
	call void @printInt(i32 %3)
	call void @printInt(i32 10)
	call void @printInt(i32 48)
	call void @printInt(i32 47)
	store i32 62, i32* %a
	call void @printInt(i32 63)
	%4 = sdiv i32 25, 29
	%5 = sdiv i32 %4, 12
	call void @printInt(i32 %5)
	store i32 70, i32* %b
	call void @printInt(i32 19)
	store i32 88, i32* %c
	%6 = load i32, i32* %a
	%7 = sub i32 98, %6
	%8 = load i32, i32* %c
	%9 = sub i32 %7, %8
	%10 = load i32, i32* %c
	%11 = sdiv i32 %9, %10
	call void @printInt(i32 %11)
	%12 = load i32, i32* %b
	call void @printInt(i32 %12)
	store i32 97, i32* %d
	%13 = load i32, i32* %b
	store i32 %13, i32* %e
	call void @printInt(i32 9)
	call void @printInt(i32 53)
	store i32 8, i32* %f
	%14 = sdiv i32 26, 5
	%15 = add i32 %14, 37
	call void @printInt(i32 %15)
	%16 = load i32, i32* %a
	call void @printInt(i32 %16)
	%17 = load i32, i32* %d
	store i32 %17, i32* %g
	%18 = load i32, i32* %e
	%19 = sub i32 55, %18
	%20 = sub i32 50, %19
	call void @printInt(i32 %20)
	store i32 20, i32* %h
	call void @printInt(i32 85)
	%21 = load i32, i32* %f
	call void @printInt(i32 %21)
	call void @printInt(i32 30)
	%22 = load i32, i32* %c
	call void @printInt(i32 %22)
	call void @printInt(i32 3)
	%23 = load i32, i32* %a
	%24 = add i32 %23, 29
	call void @printInt(i32 %24)
	%25 = load i32, i32* %d
	%26 = sdiv i32 40, %25
	%27 = add i32 16, %26
	%28 = load i32, i32* %g
	%29 = load i32, i32* %f
	%30 = mul i32 %28, %29
	%31 = load i32, i32* %b
	%32 = mul i32 %30, %31
	%33 = mul i32 %32, 4
	%34 = add i32 %27, %33
	%35 = sub i32 %34, 14
	%36 = load i32, i32* %b
	%37 = sdiv i32 %35, %36
	call void @printInt(i32 %37)
	%38 = load i32, i32* %b
	%39 = load i32, i32* %b
	%40 = add i32 %38, %39
	%41 = sub i32 %40, 36
	call void @printInt(i32 %41)
	%42 = load i32, i32* %f
	%43 = load i32, i32* %e
	%44 = load i32, i32* %a
	%45 = add i32 %43, %44
	%46 = sdiv i32 %42, %45
	%47 = load i32, i32* %h
	%48 = mul i32 %46, %47
	call void @printInt(i32 %48)
	%49 = add i32 42, 45
	%50 = load i32, i32* %c
	%51 = sdiv i32 %49, %50
	%52 = load i32, i32* %f
	%53 = load i32, i32* %f
	%54 = add i32 %52, %53
	%55 = mul i32 %51, %54
	%56 = add i32 25, %55
	call void @printInt(i32 %56)
	%57 = add i32 4, 30
	call void @printInt(i32 %57)
	%58 = load i32, i32* %e
	%59 = sdiv i32 35, 16
	%60 = add i32 %59, 20
	%61 = sdiv i32 %58, %60
	call void @printInt(i32 %61)
	call void @printInt(i32 52)
	%62 = load i32, i32* %a
	call void @printInt(i32 %62)
	%63 = load i32, i32* %g
	store i32 %63, i32* %i
	%64 = load i32, i32* %h
	%65 = mul i32 25, %64
	%66 = load i32, i32* %f
	%67 = sub i32 51, %66
	%68 = mul i32 %65, %67
	call void @printInt(i32 %68)
	%69 = mul i32 35, 68
	call void @printInt(i32 %69)
	%70 = load i32, i32* %d
	call void @printInt(i32 %70)
	%71 = load i32, i32* %d
	store i32 %71, i32* %j
	%72 = load i32, i32* %b
	call void @printInt(i32 %72)
	%73 = load i32, i32* %a
	%74 = load i32, i32* %a
	%75 = add i32 %74, 24
	%76 = add i32 %73, %75
	%77 = load i32, i32* %h
	%78 = sub i32 %76, %77
	%79 = add i32 4, %78
	call void @printInt(i32 %79)
	%80 = load i32, i32* %j
	%81 = load i32, i32* %h
	%82 = mul i32 %80, %81
	store i32 %82, i32* %k
	store i32 81, i32* %l
	store i32 10, i32* %m
	call void @printInt(i32 43)
	%83 = load i32, i32* %h
	call void @printInt(i32 %83)
	%84 = load i32, i32* %c
	%85 = load i32, i32* %h
	%86 = sub i32 %84, %85
	call void @printInt(i32 %86)
	%87 = load i32, i32* %m
	%88 = sdiv i32 %87, 11
	call void @printInt(i32 %88)
	%89 = load i32, i32* %a
	%90 = load i32, i32* %b
	%91 = mul i32 6, %90
	%92 = add i32 %91, 21
	%93 = sub i32 %89, %92
	%94 = sdiv i32 %93, 40
	%95 = load i32, i32* %c
	%96 = sub i32 %94, %95
	%97 = add i32 43, %96
	%98 = load i32, i32* %e
	%99 = add i32 %97, %98
	store i32 %99, i32* %n
	%100 = mul i32 17, 63
	store i32 %100, i32* %o
	%101 = load i32, i32* %c
	call void @printInt(i32 %101)
	%102 = mul i32 14, 1
	call void @printInt(i32 %102)
	%103 = load i32, i32* %k
	store i32 %103, i32* %p
	%104 = load i32, i32* %k
	call void @printInt(i32 %104)
	store i32 50, i32* %q
	store i32 15, i32* %r
	%105 = load i32, i32* %n
	store i32 %105, i32* %s
	%106 = load i32, i32* %e
	call void @printInt(i32 %106)
	%107 = load i32, i32* %e
	%108 = mul i32 27, %107
	call void @printInt(i32 %108)
	%109 = load i32, i32* %k
	%110 = load i32, i32* %m
	%111 = sub i32 %110, 59
	%112 = sdiv i32 %111, 3
	%113 = add i32 4, %112
	%114 = add i32 %109, %113
	call void @printInt(i32 %114)
	%115 = load i32, i32* %c
	%116 = sdiv i32 %115, 91
	call void @printInt(i32 %116)
	%117 = load i32, i32* %b
	call void @printInt(i32 %117)
	call void @printInt(i32 33)
	%118 = load i32, i32* %s
	call void @printInt(i32 %118)
	%119 = load i32, i32* %k
	%120 = load i32, i32* %r
	%121 = sub i32 %119, %120
	store i32 %121, i32* %t
	call void @printInt(i32 43)
	store i32 26, i32* %u
	%122 = mul i32 41, 6
	%123 = add i32 38, 30
	%124 = add i32 %122, %123
	store i32 %124, i32* %v
	%125 = load i32, i32* %n
	store i32 %125, i32* %w
	%126 = load i32, i32* %c
	%127 = sub i32 46, 13
	%128 = sub i32 %127, 10
	%129 = sdiv i32 77, %128
	%130 = load i32, i32* %e
	%131 = add i32 38, %130
	%132 = load i32, i32* %w
	%133 = add i32 5, %132
	%134 = sub i32 %131, %133
	%135 = sub i32 %129, %134
	%136 = sdiv i32 %126, %135
	%137 = load i32, i32* %t
	%138 = sdiv i32 %137, 58
	%139 = load i32, i32* %s
	%140 = sdiv i32 %139, 35
	%141 = load i32, i32* %a
	%142 = sub i32 49, %141
	%143 = sub i32 %140, %142
	%144 = add i32 %138, %143
	%145 = load i32, i32* %c
	%146 = load i32, i32* %s
	%147 = sdiv i32 %145, %146
	%148 = mul i32 %144, %147
	%149 = add i32 %148, 48
	%150 = add i32 %136, %149
	call void @printInt(i32 %150)
	call void @printInt(i32 25)
	%151 = load i32, i32* %e
	store i32 %151, i32* %x
	%152 = load i32, i32* %e
	%153 = add i32 %152, 71
	%154 = sdiv i32 %153, 55
	call void @printInt(i32 %154)
	call void @printInt(i32 101)
	call void @printInt(i32 59)
	%155 = load i32, i32* %p
	call void @printInt(i32 %155)
	%156 = load i32, i32* %p
	%157 = sdiv i32 %156, 55
	call void @printInt(i32 %157)
	store i32 23, i32* %y
	call void @printInt(i32 13)
	%158 = load i32, i32* %j
	call void @printInt(i32 %158)
	%159 = load i32, i32* %y
	call void @printInt(i32 %159)
	%160 = load i32, i32* %j
	%161 = mul i32 %160, 10
	%162 = sub i32 28, %161
	store i32 %162, i32* %z
	%163 = load i32, i32* %m
	%164 = load i32, i32* %e
	%165 = sdiv i32 %163, %164
	call void @printInt(i32 %165)
	call void @printInt(i32 59)
	%166 = load i32, i32* %h
	%167 = load i32, i32* %u
	%168 = sdiv i32 62, 40
	%169 = sub i32 %167, %168
	%170 = sub i32 %166, %169
	%171 = add i32 %170, 49
	call void @printInt(i32 %171)
	call void @printInt(i32 50)
	call void @printInt(i32 37)
	ret i32 0
}
