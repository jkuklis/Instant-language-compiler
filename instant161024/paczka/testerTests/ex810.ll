declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%b = alloca i32
	%i = alloca i32
	%h = alloca i32
	%e = alloca i32
	%d = alloca i32
	%t = alloca i32
	%f = alloca i32
	%p = alloca i32
	%l = alloca i32
	%g = alloca i32
	%q = alloca i32
	%n = alloca i32
	%m = alloca i32
	%k = alloca i32
	%j = alloca i32
	%x = alloca i32
	%u = alloca i32
	%o = alloca i32
	%z = alloca i32
	%y = alloca i32
	%w = alloca i32
	%v = alloca i32
	%s = alloca i32
	%r = alloca i32
	%ac = alloca i32
	%ab = alloca i32
	%aa = alloca i32
	store i32 45, i32* %a
	%0 = load i32, i32* %a
	%1 = sub i32 2, %0
	%2 = load i32, i32* %a
	%3 = load i32, i32* %a
	%4 = sub i32 %2, %3
	%5 = load i32, i32* %a
	%6 = mul i32 %5, 10
	%7 = add i32 %4, %6
	%8 = sdiv i32 %1, %7
	call void @printInt(i32 %8)
	call void @printInt(i32 29)
	call void @printInt(i32 30)
	%9 = load i32, i32* %a
	%10 = sdiv i32 64, %9
	call void @printInt(i32 %10)
	call void @printInt(i32 24)
	%11 = load i32, i32* %a
	call void @printInt(i32 %11)
	%12 = load i32, i32* %a
	call void @printInt(i32 %12)
	%13 = load i32, i32* %a
	store i32 %13, i32* %b
	call void @printInt(i32 13)
	%14 = load i32, i32* %b
	%15 = mul i32 19, %14
	call void @printInt(i32 %15)
	%16 = load i32, i32* %a
	call void @printInt(i32 %16)
	call void @printInt(i32 10)
	store i32 50, i32* %c
	%17 = load i32, i32* %c
	call void @printInt(i32 %17)
	%18 = load i32, i32* %a
	%19 = sub i32 %18, 25
	store i32 %19, i32* %d
	%20 = load i32, i32* %a
	%21 = sdiv i32 %20, 35
	call void @printInt(i32 %21)
	%22 = load i32, i32* %c
	call void @printInt(i32 %22)
	call void @printInt(i32 2)
	store i32 11, i32* %e
	%23 = load i32, i32* %b
	%24 = load i32, i32* %b
	%25 = add i32 68, 24
	%26 = mul i32 %24, %25
	%27 = load i32, i32* %d
	%28 = sub i32 %26, %27
	%29 = load i32, i32* %c
	%30 = sdiv i32 %28, %29
	%31 = add i32 %23, %30
	%32 = load i32, i32* %c
	%33 = add i32 %31, %32
	call void @printInt(i32 %33)
	%34 = load i32, i32* %d
	call void @printInt(i32 %34)
	%35 = load i32, i32* %a
	%36 = mul i32 %35, 21
	call void @printInt(i32 %36)
	store i32 55, i32* %f
	%37 = load i32, i32* %c
	%38 = sub i32 25, %37
	%39 = mul i32 %38, 6
	%40 = load i32, i32* %b
	%41 = sdiv i32 %39, %40
	call void @printInt(i32 %41)
	%42 = load i32, i32* %b
	store i32 %42, i32* %g
	%43 = load i32, i32* %g
	%44 = add i32 %43, 19
	%45 = load i32, i32* %e
	%46 = sub i32 %44, %45
	call void @printInt(i32 %46)
	store i32 30, i32* %h
	%47 = load i32, i32* %c
	%48 = load i32, i32* %d
	%49 = load i32, i32* %e
	%50 = load i32, i32* %c
	%51 = sdiv i32 %49, %50
	%52 = mul i32 %48, %51
	%53 = sdiv i32 %52, 47
	%54 = sub i32 %47, %53
	call void @printInt(i32 %54)
	%55 = load i32, i32* %b
	store i32 %55, i32* %i
	%56 = load i32, i32* %d
	%57 = sub i32 %56, 22
	%58 = mul i32 48, %57
	%59 = load i32, i32* %h
	%60 = mul i32 17, %59
	%61 = sub i32 %58, %60
	%62 = load i32, i32* %h
	%63 = add i32 19, %62
	%64 = sdiv i32 94, %63
	%65 = mul i32 %61, %64
	store i32 %65, i32* %j
	%66 = load i32, i32* %i
	%67 = load i32, i32* %a
	%68 = load i32, i32* %j
	%69 = sub i32 2, %68
	%70 = load i32, i32* %h
	%71 = load i32, i32* %i
	%72 = sub i32 72, %71
	%73 = load i32, i32* %e
	%74 = sub i32 %72, %73
	%75 = sdiv i32 %74, 6
	%76 = sdiv i32 %70, %75
	%77 = add i32 %69, %76
	%78 = add i32 %67, %77
	%79 = mul i32 32, 29
	%80 = add i32 %78, %79
	%81 = load i32, i32* %d
	%82 = load i32, i32* %e
	%83 = sdiv i32 %81, %82
	%84 = mul i32 %83, 46
	%85 = sdiv i32 %84, 63
	%86 = add i32 %80, %85
	%87 = sub i32 %66, %86
	call void @printInt(i32 %87)
	%88 = load i32, i32* %i
	call void @printInt(i32 %88)
	%89 = load i32, i32* %a
	call void @printInt(i32 %89)
	%90 = load i32, i32* %h
	call void @printInt(i32 %90)
	store i32 13, i32* %k
	%91 = load i32, i32* %c
	call void @printInt(i32 %91)
	store i32 16, i32* %l
	%92 = load i32, i32* %a
	%93 = add i32 46, %92
	%94 = mul i32 1, 13
	%95 = sdiv i32 %93, %94
	call void @printInt(i32 %95)
	%96 = sub i32 27, 96
	%97 = sub i32 %96, 71
	call void @printInt(i32 %97)
	%98 = load i32, i32* %f
	call void @printInt(i32 %98)
	%99 = load i32, i32* %l
	call void @printInt(i32 %99)
	%100 = load i32, i32* %g
	call void @printInt(i32 %100)
	%101 = load i32, i32* %a
	store i32 %101, i32* %m
	%102 = load i32, i32* %k
	call void @printInt(i32 %102)
	call void @printInt(i32 2)
	%103 = load i32, i32* %l
	%104 = add i32 %103, 25
	%105 = load i32, i32* %m
	%106 = sdiv i32 %104, %105
	%107 = load i32, i32* %h
	%108 = add i32 %106, %107
	call void @printInt(i32 %108)
	call void @printInt(i32 73)
	%109 = load i32, i32* %f
	store i32 %109, i32* %n
	call void @printInt(i32 44)
	store i32 14, i32* %o
	call void @printInt(i32 12)
	%110 = load i32, i32* %a
	store i32 %110, i32* %p
	call void @printInt(i32 11)
	call void @printInt(i32 65)
	call void @printInt(i32 64)
	%111 = load i32, i32* %e
	%112 = mul i32 %111, 34
	call void @printInt(i32 %112)
	call void @printInt(i32 15)
	%113 = load i32, i32* %p
	store i32 %113, i32* %q
	store i32 9, i32* %r
	%114 = add i32 38, 2
	%115 = add i32 56, 21
	%116 = load i32, i32* %d
	%117 = mul i32 %115, %116
	%118 = load i32, i32* %h
	%119 = load i32, i32* %a
	%120 = sub i32 48, %119
	%121 = mul i32 %118, %120
	%122 = sdiv i32 %117, %121
	%123 = mul i32 %114, %122
	%124 = load i32, i32* %m
	%125 = add i32 %124, 106
	%126 = mul i32 58, %125
	%127 = sub i32 %123, %126
	%128 = add i32 %127, 31
	store i32 %128, i32* %s
	%129 = load i32, i32* %i
	call void @printInt(i32 %129)
	call void @printInt(i32 11)
	%130 = load i32, i32* %o
	call void @printInt(i32 %130)
	%131 = load i32, i32* %p
	%132 = load i32, i32* %i
	%133 = sub i32 %131, %132
	%134 = sdiv i32 %133, 39
	call void @printInt(i32 %134)
	%135 = sub i32 26, 12
	call void @printInt(i32 %135)
	store i32 21, i32* %t
	store i32 16, i32* %u
	call void @printInt(i32 50)
	call void @printInt(i32 6)
	call void @printInt(i32 18)
	call void @printInt(i32 33)
	%136 = load i32, i32* %t
	store i32 %136, i32* %v
	%137 = load i32, i32* %t
	call void @printInt(i32 %137)
	%138 = load i32, i32* %n
	%139 = load i32, i32* %q
	%140 = sub i32 %138, %139
	call void @printInt(i32 %140)
	%141 = load i32, i32* %t
	call void @printInt(i32 %141)
	%142 = load i32, i32* %i
	%143 = add i32 59, %142
	%144 = sdiv i32 3, %143
	store i32 %144, i32* %w
	%145 = load i32, i32* %j
	call void @printInt(i32 %145)
	%146 = load i32, i32* %e
	%147 = add i32 %146, 9
	%148 = add i32 15, %147
	store i32 %148, i32* %x
	%149 = sub i32 47, 25
	%150 = add i32 %149, 64
	%151 = load i32, i32* %k
	%152 = add i32 13, %151
	%153 = load i32, i32* %t
	%154 = sub i32 %152, %153
	%155 = load i32, i32* %f
	%156 = sdiv i32 %154, %155
	%157 = mul i32 %150, %156
	%158 = load i32, i32* %f
	%159 = sub i32 %157, %158
	call void @printInt(i32 %159)
	%160 = load i32, i32* %n
	%161 = add i32 %160, 54
	call void @printInt(i32 %161)
	%162 = load i32, i32* %b
	store i32 %162, i32* %y
	%163 = load i32, i32* %l
	%164 = add i32 31, %163
	store i32 %164, i32* %z
	%165 = load i32, i32* %a
	store i32 %165, i32* %aa
	%166 = load i32, i32* %g
	%167 = sub i32 72, %166
	store i32 %167, i32* %ab
	%168 = load i32, i32* %u
	call void @printInt(i32 %168)
	call void @printInt(i32 31)
	call void @printInt(i32 34)
	%169 = load i32, i32* %t
	%170 = load i32, i32* %q
	%171 = sub i32 %169, %170
	call void @printInt(i32 %171)
	%172 = add i32 9, 36
	%173 = sdiv i32 20, %172
	%174 = add i32 54, 17
	%175 = sub i32 25, %174
	%176 = sdiv i32 %173, %175
	call void @printInt(i32 %176)
	%177 = load i32, i32* %b
	%178 = mul i32 %177, 31
	%179 = add i32 %178, 2
	call void @printInt(i32 %179)
	%180 = load i32, i32* %x
	store i32 %180, i32* %ac
	call void @printInt(i32 8)
	%181 = load i32, i32* %p
	call void @printInt(i32 %181)
	ret i32 0
}
