declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%d = alloca i32
	%i = alloca i32
	%g = alloca i32
	%e = alloca i32
	%f = alloca i32
	%h = alloca i32
	%k = alloca i32
	%o = alloca i32
	%j = alloca i32
	%s = alloca i32
	%p = alloca i32
	%q = alloca i32
	%n = alloca i32
	%l = alloca i32
	%w = alloca i32
	%v = alloca i32
	%u = alloca i32
	%t = alloca i32
	%r = alloca i32
	%m = alloca i32
	call void @printInt(i32 12)
	%0 = mul i32 6, 56
	%1 = sub i32 %0, 33
	%2 = add i32 63, 11
	%3 = add i32 %2, 33
	%4 = mul i32 %1, %3
	store i32 %4, i32* %a
	%5 = load i32, i32* %a
	call void @printInt(i32 %5)
	%6 = load i32, i32* %a
	%7 = mul i32 %6, 37
	store i32 %7, i32* %b
	%8 = load i32, i32* %b
	%9 = sub i32 43, %8
	%10 = load i32, i32* %b
	%11 = add i32 %9, %10
	call void @printInt(i32 %11)
	%12 = load i32, i32* %b
	%13 = mul i32 8, %12
	store i32 %13, i32* %c
	%14 = load i32, i32* %b
	%15 = mul i32 65, %14
	call void @printInt(i32 %15)
	call void @printInt(i32 7)
	%16 = load i32, i32* %c
	call void @printInt(i32 %16)
	%17 = load i32, i32* %c
	%18 = add i32 113, %17
	%19 = add i32 2, %18
	call void @printInt(i32 %19)
	%20 = mul i32 27, 3
	%21 = sdiv i32 48, %20
	%22 = load i32, i32* %b
	%23 = sdiv i32 %22, 38
	%24 = mul i32 %21, %23
	call void @printInt(i32 %24)
	call void @printInt(i32 15)
	%25 = load i32, i32* %a
	%26 = sub i32 0, %25
	call void @printInt(i32 %26)
	%27 = load i32, i32* %c
	call void @printInt(i32 %27)
	store i32 29, i32* %d
	call void @printInt(i32 18)
	%28 = load i32, i32* %c
	call void @printInt(i32 %28)
	call void @printInt(i32 62)
	call void @printInt(i32 30)
	%29 = sub i32 50, 15
	call void @printInt(i32 %29)
	%30 = load i32, i32* %c
	call void @printInt(i32 %30)
	%31 = load i32, i32* %b
	%32 = add i32 %31, 43
	call void @printInt(i32 %32)
	%33 = load i32, i32* %b
	%34 = load i32, i32* %a
	%35 = sub i32 %33, %34
	call void @printInt(i32 %35)
	call void @printInt(i32 5)
	call void @printInt(i32 42)
	call void @printInt(i32 39)
	call void @printInt(i32 32)
	call void @printInt(i32 90)
	%36 = load i32, i32* %a
	call void @printInt(i32 %36)
	%37 = load i32, i32* %a
	store i32 %37, i32* %e
	store i32 9, i32* %f
	%38 = load i32, i32* %a
	call void @printInt(i32 %38)
	%39 = load i32, i32* %d
	call void @printInt(i32 %39)
	%40 = load i32, i32* %f
	call void @printInt(i32 %40)
	call void @printInt(i32 13)
	%41 = load i32, i32* %a
	%42 = load i32, i32* %c
	%43 = load i32, i32* %d
	%44 = mul i32 %43, 17
	%45 = sub i32 15, %44
	%46 = load i32, i32* %a
	%47 = sub i32 %45, %46
	%48 = add i32 %42, %47
	%49 = load i32, i32* %b
	%50 = mul i32 %48, %49
	%51 = sdiv i32 %41, %50
	call void @printInt(i32 %51)
	call void @printInt(i32 45)
	store i32 21, i32* %g
	%52 = load i32, i32* %b
	%53 = load i32, i32* %a
	%54 = sub i32 %52, %53
	call void @printInt(i32 %54)
	%55 = load i32, i32* %d
	store i32 %55, i32* %h
	%56 = load i32, i32* %h
	%57 = add i32 12, %56
	%58 = sub i32 %57, 46
	%59 = load i32, i32* %e
	%60 = sdiv i32 %58, %59
	%61 = load i32, i32* %h
	%62 = add i32 %60, %61
	%63 = load i32, i32* %a
	%64 = load i32, i32* %f
	%65 = add i32 13, %64
	%66 = mul i32 %63, %65
	%67 = mul i32 %62, %66
	%68 = load i32, i32* %e
	%69 = sdiv i32 %68, 29
	%70 = mul i32 %67, %69
	%71 = load i32, i32* %h
	%72 = sdiv i32 %70, %71
	call void @printInt(i32 %72)
	%73 = load i32, i32* %e
	store i32 %73, i32* %i
	call void @printInt(i32 43)
	%74 = load i32, i32* %d
	%75 = mul i32 %74, 37
	%76 = load i32, i32* %g
	%77 = sub i32 55, %76
	%78 = load i32, i32* %i
	%79 = sub i32 %77, %78
	%80 = load i32, i32* %d
	%81 = load i32, i32* %b
	%82 = sub i32 %80, %81
	%83 = sdiv i32 %79, %82
	%84 = mul i32 %83, 66
	%85 = sdiv i32 %84, 10
	%86 = load i32, i32* %d
	%87 = mul i32 27, %86
	%88 = sub i32 2, %87
	%89 = add i32 %85, %88
	%90 = load i32, i32* %a
	%91 = mul i32 %89, %90
	%92 = sub i32 25, 59
	%93 = add i32 %91, %92
	%94 = load i32, i32* %g
	%95 = sub i32 %93, %94
	%96 = load i32, i32* %i
	%97 = load i32, i32* %c
	%98 = sub i32 %96, %97
	%99 = load i32, i32* %e
	%100 = mul i32 10, 3
	%101 = mul i32 %99, %100
	%102 = add i32 %101, 7
	%103 = load i32, i32* %g
	%104 = sub i32 %102, %103
	%105 = mul i32 53, 27
	%106 = sdiv i32 %105, 60
	%107 = sdiv i32 %104, %106
	%108 = sub i32 13, 7
	%109 = add i32 %107, %108
	%110 = load i32, i32* %f
	%111 = add i32 %109, %110
	%112 = sub i32 %98, %111
	%113 = add i32 %95, %112
	%114 = sdiv i32 %75, %113
	%115 = mul i32 %114, 95
	call void @printInt(i32 %115)
	store i32 13, i32* %j
	store i32 66, i32* %k
	%116 = load i32, i32* %k
	call void @printInt(i32 %116)
	call void @printInt(i32 25)
	%117 = load i32, i32* %h
	call void @printInt(i32 %117)
	call void @printInt(i32 80)
	call void @printInt(i32 2)
	%118 = load i32, i32* %j
	%119 = load i32, i32* %j
	%120 = sdiv i32 %119, 38
	%121 = add i32 %118, %120
	%122 = load i32, i32* %i
	%123 = add i32 %122, 52
	%124 = add i32 35, %123
	%125 = sdiv i32 %124, 17
	%126 = load i32, i32* %e
	%127 = add i32 3, %126
	%128 = sub i32 %127, 6
	%129 = add i32 %125, %128
	%130 = add i32 %129, 59
	%131 = sub i32 %121, %130
	call void @printInt(i32 %131)
	%132 = load i32, i32* %g
	call void @printInt(i32 %132)
	%133 = load i32, i32* %g
	store i32 %133, i32* %l
	call void @printInt(i32 34)
	store i32 45, i32* %m
	%134 = load i32, i32* %a
	store i32 %134, i32* %n
	%135 = load i32, i32* %k
	%136 = load i32, i32* %k
	%137 = sdiv i32 %135, %136
	%138 = sub i32 42, %137
	%139 = mul i32 %138, 28
	call void @printInt(i32 %139)
	%140 = load i32, i32* %g
	call void @printInt(i32 %140)
	%141 = load i32, i32* %h
	store i32 %141, i32* %o
	%142 = load i32, i32* %h
	%143 = load i32, i32* %k
	%144 = mul i32 %142, %143
	%145 = load i32, i32* %c
	%146 = sdiv i32 %144, %145
	%147 = add i32 %146, 69
	store i32 %147, i32* %p
	call void @printInt(i32 10)
	%148 = load i32, i32* %f
	call void @printInt(i32 %148)
	%149 = load i32, i32* %o
	call void @printInt(i32 %149)
	%150 = load i32, i32* %f
	%151 = load i32, i32* %c
	%152 = add i32 %151, 69
	%153 = add i32 %150, %152
	%154 = sdiv i32 %153, 3
	%155 = mul i32 69, %154
	call void @printInt(i32 %155)
	%156 = load i32, i32* %l
	call void @printInt(i32 %156)
	%157 = load i32, i32* %o
	%158 = load i32, i32* %g
	%159 = mul i32 22, 29
	%160 = sdiv i32 %158, %159
	%161 = add i32 %157, %160
	call void @printInt(i32 %161)
	%162 = load i32, i32* %j
	%163 = load i32, i32* %e
	%164 = mul i32 %163, 36
	%165 = add i32 %162, %164
	%166 = load i32, i32* %o
	%167 = sdiv i32 %165, %166
	%168 = sub i32 47, %167
	call void @printInt(i32 %168)
	call void @printInt(i32 13)
	%169 = load i32, i32* %c
	%170 = mul i32 6, 29
	%171 = load i32, i32* %f
	%172 = mul i32 %170, %171
	%173 = sub i32 %169, %172
	store i32 %173, i32* %q
	%174 = load i32, i32* %i
	%175 = load i32, i32* %q
	%176 = load i32, i32* %a
	%177 = sdiv i32 %175, %176
	%178 = sub i32 43, 52
	%179 = mul i32 5, 8
	%180 = sub i32 35, %179
	%181 = mul i32 59, %180
	%182 = load i32, i32* %i
	%183 = mul i32 %181, %182
	%184 = sub i32 %178, %183
	%185 = mul i32 %177, %184
	%186 = sdiv i32 %174, %185
	call void @printInt(i32 %186)
	%187 = load i32, i32* %n
	call void @printInt(i32 %187)
	%188 = load i32, i32* %o
	call void @printInt(i32 %188)
	%189 = load i32, i32* %e
	call void @printInt(i32 %189)
	%190 = load i32, i32* %n
	call void @printInt(i32 %190)
	%191 = load i32, i32* %p
	%192 = sub i32 %191, 21
	%193 = load i32, i32* %j
	%194 = mul i32 %193, 72
	%195 = sub i32 %192, %194
	call void @printInt(i32 %195)
	%196 = load i32, i32* %b
	call void @printInt(i32 %196)
	call void @printInt(i32 31)
	%197 = load i32, i32* %p
	store i32 %197, i32* %r
	%198 = load i32, i32* %i
	call void @printInt(i32 %198)
	call void @printInt(i32 2)
	%199 = load i32, i32* %c
	store i32 %199, i32* %s
	%200 = load i32, i32* %b
	call void @printInt(i32 %200)
	call void @printInt(i32 30)
	%201 = load i32, i32* %k
	call void @printInt(i32 %201)
	%202 = load i32, i32* %q
	call void @printInt(i32 %202)
	%203 = sub i32 72, 46
	%204 = load i32, i32* %f
	%205 = load i32, i32* %d
	%206 = sdiv i32 %204, %205
	%207 = mul i32 %203, %206
	%208 = load i32, i32* %d
	%209 = mul i32 %207, %208
	call void @printInt(i32 %209)
	%210 = load i32, i32* %s
	%211 = load i32, i32* %s
	%212 = sdiv i32 %210, %211
	%213 = load i32, i32* %a
	%214 = sdiv i32 %212, %213
	call void @printInt(i32 %214)
	%215 = load i32, i32* %d
	call void @printInt(i32 %215)
	%216 = load i32, i32* %i
	store i32 %216, i32* %t
	call void @printInt(i32 23)
	call void @printInt(i32 4)
	call void @printInt(i32 1)
	call void @printInt(i32 73)
	store i32 22, i32* %u
	%217 = load i32, i32* %i
	store i32 %217, i32* %v
	%218 = load i32, i32* %e
	call void @printInt(i32 %218)
	%219 = load i32, i32* %s
	%220 = add i32 %219, 43
	call void @printInt(i32 %220)
	%221 = mul i32 7, 15
	call void @printInt(i32 %221)
	call void @printInt(i32 67)
	%222 = load i32, i32* %p
	call void @printInt(i32 %222)
	%223 = load i32, i32* %g
	call void @printInt(i32 %223)
	store i32 4, i32* %w
	call void @printInt(i32 69)
	ret i32 0
}
