declare void @printInt(i32)
define i32 @main() {
entry:
	%i = alloca i32
	%c = alloca i32
	%d = alloca i32
	%a = alloca i32
	%e = alloca i32
	%b = alloca i32
	%n = alloca i32
	%j = alloca i32
	%h = alloca i32
	%p = alloca i32
	%g = alloca i32
	%x = alloca i32
	%u = alloca i32
	%k = alloca i32
	%f = alloca i32
	%w = alloca i32
	%q = alloca i32
	%y = alloca i32
	%t = alloca i32
	%s = alloca i32
	%r = alloca i32
	%m = alloca i32
	%l = alloca i32
	%ab = alloca i32
	%v = alloca i32
	%o = alloca i32
	%al = alloca i32
	%aj = alloca i32
	%ah = alloca i32
	%af = alloca i32
	%ac = alloca i32
	%aa = alloca i32
	%z = alloca i32
	%aq = alloca i32
	%ap = alloca i32
	%ao = alloca i32
	%an = alloca i32
	%am = alloca i32
	%ak = alloca i32
	%ai = alloca i32
	%ag = alloca i32
	%ae = alloca i32
	%ad = alloca i32
	%0 = mul i32 1, 17
	%1 = add i32 %0, 42
	%2 = sdiv i32 %1, 59
	%3 = add i32 15, %2
	%4 = sdiv i32 %3, 15
	call void @printInt(i32 %4)
	call void @printInt(i32 35)
	call void @printInt(i32 21)
	call void @printInt(i32 97)
	%5 = add i32 22, 23
	store i32 %5, i32* %a
	store i32 68, i32* %b
	store i32 10, i32* %c
	%6 = load i32, i32* %a
	store i32 %6, i32* %d
	%7 = load i32, i32* %c
	%8 = mul i32 15, %7
	call void @printInt(i32 %8)
	call void @printInt(i32 10)
	store i32 24, i32* %e
	%9 = load i32, i32* %b
	%10 = load i32, i32* %d
	%11 = add i32 %9, %10
	store i32 %11, i32* %f
	%12 = load i32, i32* %c
	call void @printInt(i32 %12)
	%13 = load i32, i32* %d
	%14 = sub i32 7, %13
	%15 = mul i32 10, %14
	%16 = load i32, i32* %b
	%17 = add i32 12, %16
	%18 = mul i32 %15, %17
	%19 = sdiv i32 47, %18
	call void @printInt(i32 %19)
	call void @printInt(i32 32)
	%20 = load i32, i32* %a
	call void @printInt(i32 %20)
	%21 = load i32, i32* %d
	store i32 %21, i32* %g
	call void @printInt(i32 20)
	%22 = load i32, i32* %f
	call void @printInt(i32 %22)
	call void @printInt(i32 10)
	%23 = load i32, i32* %e
	%24 = add i32 8, %23
	call void @printInt(i32 %24)
	%25 = load i32, i32* %b
	store i32 %25, i32* %h
	%26 = add i32 5, 49
	store i32 %26, i32* %i
	%27 = load i32, i32* %i
	store i32 %27, i32* %j
	store i32 12, i32* %k
	%28 = load i32, i32* %h
	%29 = sub i32 %28, 14
	%30 = add i32 %29, 8
	%31 = add i32 %30, 33
	%32 = load i32, i32* %j
	%33 = sub i32 %31, %32
	call void @printInt(i32 %33)
	%34 = add i32 29, 4
	%35 = mul i32 15, %34
	%36 = load i32, i32* %h
	%37 = sub i32 %35, %36
	call void @printInt(i32 %37)
	%38 = load i32, i32* %i
	call void @printInt(i32 %38)
	%39 = load i32, i32* %f
	call void @printInt(i32 %39)
	%40 = load i32, i32* %e
	%41 = add i32 51, %40
	%42 = add i32 58, %41
	call void @printInt(i32 %42)
	%43 = load i32, i32* %i
	%44 = add i32 %43, 62
	%45 = sub i32 15, %44
	%46 = sdiv i32 %45, 57
	call void @printInt(i32 %46)
	%47 = load i32, i32* %i
	call void @printInt(i32 %47)
	%48 = load i32, i32* %j
	call void @printInt(i32 %48)
	%49 = load i32, i32* %j
	%50 = sub i32 11, %49
	call void @printInt(i32 %50)
	%51 = load i32, i32* %d
	store i32 %51, i32* %l
	%52 = load i32, i32* %c
	store i32 %52, i32* %m
	store i32 5, i32* %n
	%53 = load i32, i32* %j
	%54 = load i32, i32* %e
	%55 = sub i32 %53, %54
	%56 = add i32 75, %55
	%57 = sub i32 11, %56
	%58 = load i32, i32* %n
	%59 = mul i32 %57, %58
	call void @printInt(i32 %59)
	call void @printInt(i32 10)
	call void @printInt(i32 15)
	%60 = load i32, i32* %c
	%61 = add i32 %60, 32
	%62 = load i32, i32* %i
	%63 = sdiv i32 %61, %62
	call void @printInt(i32 %63)
	%64 = load i32, i32* %n
	%65 = add i32 38, %64
	%66 = load i32, i32* %g
	%67 = load i32, i32* %l
	%68 = add i32 %66, %67
	%69 = mul i32 %65, %68
	%70 = sdiv i32 5, 24
	%71 = sub i32 %69, %70
	%72 = sdiv i32 %71, 33
	%73 = load i32, i32* %b
	%74 = sub i32 %72, %73
	%75 = mul i32 %74, 61
	%76 = add i32 %75, 8
	%77 = mul i32 %76, 8
	store i32 %77, i32* %o
	%78 = load i32, i32* %m
	%79 = load i32, i32* %g
	%80 = mul i32 %78, %79
	%81 = load i32, i32* %e
	%82 = mul i32 %80, %81
	%83 = sub i32 %82, 0
	call void @printInt(i32 %83)
	%84 = sdiv i32 39, 19
	%85 = mul i32 15, %84
	call void @printInt(i32 %85)
	call void @printInt(i32 22)
	%86 = load i32, i32* %h
	store i32 %86, i32* %p
	call void @printInt(i32 1)
	%87 = load i32, i32* %i
	%88 = load i32, i32* %b
	%89 = sub i32 18, %88
	%90 = load i32, i32* %n
	%91 = load i32, i32* %b
	%92 = mul i32 %90, %91
	%93 = mul i32 %89, %92
	%94 = mul i32 %87, %93
	call void @printInt(i32 %94)
	%95 = load i32, i32* %p
	%96 = sdiv i32 %95, 29
	%97 = sub i32 11, %96
	%98 = mul i32 51, %97
	call void @printInt(i32 %98)
	%99 = load i32, i32* %k
	%100 = mul i32 %99, 69
	call void @printInt(i32 %100)
	%101 = load i32, i32* %i
	%102 = load i32, i32* %p
	%103 = mul i32 %102, 6
	%104 = mul i32 %101, %103
	%105 = load i32, i32* %e
	%106 = sub i32 %104, %105
	store i32 %106, i32* %q
	store i32 14, i32* %r
	call void @printInt(i32 56)
	%107 = load i32, i32* %d
	%108 = load i32, i32* %f
	%109 = load i32, i32* %p
	%110 = add i32 %108, %109
	%111 = sub i32 %107, %110
	store i32 %111, i32* %s
	%112 = load i32, i32* %q
	%113 = sub i32 %112, 16
	%114 = sdiv i32 %113, 32
	store i32 %114, i32* %t
	%115 = load i32, i32* %o
	call void @printInt(i32 %115)
	call void @printInt(i32 22)
	%116 = load i32, i32* %a
	store i32 %116, i32* %u
	call void @printInt(i32 8)
	%117 = load i32, i32* %d
	%118 = sub i32 %117, 39
	store i32 %118, i32* %v
	call void @printInt(i32 25)
	%119 = load i32, i32* %s
	call void @printInt(i32 %119)
	%120 = load i32, i32* %e
	call void @printInt(i32 %120)
	%121 = load i32, i32* %a
	call void @printInt(i32 %121)
	%122 = load i32, i32* %v
	call void @printInt(i32 %122)
	%123 = load i32, i32* %g
	call void @printInt(i32 %123)
	%124 = mul i32 66, 27
	%125 = mul i32 16, %124
	call void @printInt(i32 %125)
	%126 = load i32, i32* %k
	call void @printInt(i32 %126)
	%127 = add i32 22, 9
	store i32 %127, i32* %w
	%128 = load i32, i32* %w
	%129 = load i32, i32* %u
	%130 = add i32 %129, 24
	%131 = sub i32 %130, 14
	%132 = sdiv i32 %128, %131
	call void @printInt(i32 %132)
	call void @printInt(i32 33)
	call void @printInt(i32 40)
	%133 = load i32, i32* %e
	%134 = sub i32 %133, 18
	%135 = mul i32 %134, 4
	%136 = sdiv i32 7, %135
	store i32 %136, i32* %x
	%137 = load i32, i32* %i
	call void @printInt(i32 %137)
	call void @printInt(i32 40)
	%138 = add i32 13, 15
	store i32 %138, i32* %y
	call void @printInt(i32 21)
	%139 = load i32, i32* %a
	call void @printInt(i32 %139)
	%140 = load i32, i32* %s
	%141 = add i32 47, %140
	%142 = sub i32 8, %141
	%143 = mul i32 64, %142
	call void @printInt(i32 %143)
	%144 = load i32, i32* %x
	%145 = load i32, i32* %c
	%146 = sub i32 %144, %145
	call void @printInt(i32 %146)
	call void @printInt(i32 19)
	%147 = load i32, i32* %h
	%148 = load i32, i32* %i
	%149 = sub i32 %147, %148
	%150 = load i32, i32* %y
	%151 = load i32, i32* %a
	%152 = load i32, i32* %c
	%153 = add i32 %152, 35
	%154 = mul i32 %151, %153
	%155 = sub i32 %150, %154
	%156 = add i32 5, %155
	%157 = mul i32 39, %156
	%158 = sdiv i32 114, %157
	%159 = mul i32 %158, 38
	%160 = load i32, i32* %q
	%161 = sub i32 %159, %160
	%162 = add i32 %149, %161
	%163 = add i32 %162, 5
	store i32 %163, i32* %z
	store i32 44, i32* %aa
	%164 = load i32, i32* %aa
	call void @printInt(i32 %164)
	store i32 60, i32* %ab
	store i32 41, i32* %ac
	%165 = load i32, i32* %ab
	call void @printInt(i32 %165)
	call void @printInt(i32 41)
	call void @printInt(i32 3)
	%166 = load i32, i32* %b
	%167 = sdiv i32 81, 41
	%168 = sub i32 %166, %167
	call void @printInt(i32 %168)
	call void @printInt(i32 42)
	call void @printInt(i32 41)
	%169 = load i32, i32* %x
	store i32 %169, i32* %ad
	%170 = load i32, i32* %ac
	%171 = load i32, i32* %p
	%172 = mul i32 %170, %171
	call void @printInt(i32 %172)
	%173 = load i32, i32* %m
	store i32 %173, i32* %ae
	call void @printInt(i32 1)
	store i32 22, i32* %af
	store i32 16, i32* %ag
	%174 = load i32, i32* %f
	%175 = load i32, i32* %d
	%176 = mul i32 13, %175
	%177 = sdiv i32 %174, %176
	call void @printInt(i32 %177)
	%178 = load i32, i32* %g
	call void @printInt(i32 %178)
	%179 = load i32, i32* %n
	call void @printInt(i32 %179)
	%180 = load i32, i32* %ab
	%181 = sdiv i32 1, %180
	call void @printInt(i32 %181)
	call void @printInt(i32 8)
	%182 = load i32, i32* %y
	call void @printInt(i32 %182)
	%183 = load i32, i32* %g
	call void @printInt(i32 %183)
	call void @printInt(i32 32)
	%184 = load i32, i32* %t
	%185 = sub i32 11, %184
	%186 = load i32, i32* %x
	%187 = add i32 %185, %186
	call void @printInt(i32 %187)
	%188 = load i32, i32* %u
	call void @printInt(i32 %188)
	%189 = load i32, i32* %w
	%190 = load i32, i32* %w
	%191 = sdiv i32 %189, %190
	call void @printInt(i32 %191)
	%192 = load i32, i32* %p
	call void @printInt(i32 %192)
	call void @printInt(i32 12)
	call void @printInt(i32 29)
	%193 = load i32, i32* %r
	call void @printInt(i32 %193)
	%194 = sdiv i32 48, 6
	store i32 %194, i32* %ah
	%195 = sub i32 41, 50
	store i32 %195, i32* %ai
	%196 = load i32, i32* %j
	store i32 %196, i32* %aj
	%197 = load i32, i32* %j
	call void @printInt(i32 %197)
	%198 = load i32, i32* %k
	call void @printInt(i32 %198)
	%199 = load i32, i32* %q
	%200 = load i32, i32* %af
	%201 = mul i32 %199, %200
	call void @printInt(i32 %201)
	%202 = load i32, i32* %ah
	%203 = sub i32 %202, 4
	call void @printInt(i32 %203)
	%204 = load i32, i32* %c
	%205 = sub i32 39, 71
	%206 = add i32 %204, %205
	%207 = sub i32 %206, 7
	call void @printInt(i32 %207)
	call void @printInt(i32 58)
	call void @printInt(i32 18)
	%208 = load i32, i32* %h
	store i32 %208, i32* %ak
	%209 = load i32, i32* %r
	%210 = sdiv i32 37, %209
	%211 = sdiv i32 %210, 102
	call void @printInt(i32 %211)
	call void @printInt(i32 8)
	%212 = sub i32 13, 21
	store i32 %212, i32* %al
	call void @printInt(i32 71)
	%213 = sdiv i32 62, 45
	store i32 %213, i32* %am
	%214 = load i32, i32* %t
	call void @printInt(i32 %214)
	%215 = load i32, i32* %k
	call void @printInt(i32 %215)
	%216 = load i32, i32* %a
	call void @printInt(i32 %216)
	%217 = load i32, i32* %d
	call void @printInt(i32 %217)
	%218 = load i32, i32* %x
	store i32 %218, i32* %an
	call void @printInt(i32 29)
	call void @printInt(i32 37)
	%219 = load i32, i32* %aj
	%220 = load i32, i32* %u
	%221 = load i32, i32* %u
	%222 = add i32 %220, %221
	%223 = mul i32 %222, 12
	%224 = sdiv i32 %219, %223
	call void @printInt(i32 %224)
	%225 = load i32, i32* %n
	call void @printInt(i32 %225)
	call void @printInt(i32 71)
	%226 = load i32, i32* %al
	%227 = load i32, i32* %n
	%228 = sdiv i32 %226, %227
	call void @printInt(i32 %228)
	%229 = load i32, i32* %a
	store i32 %229, i32* %ao
	%230 = load i32, i32* %l
	call void @printInt(i32 %230)
	%231 = load i32, i32* %c
	%232 = mul i32 31, %231
	store i32 %232, i32* %ap
	%233 = load i32, i32* %h
	store i32 %233, i32* %aq
	%234 = load i32, i32* %c
	call void @printInt(i32 %234)
	ret i32 0
}
