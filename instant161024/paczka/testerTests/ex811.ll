declare void @printInt(i32)
define i32 @main() {
entry:
	%c = alloca i32
	%a = alloca i32
	%p = alloca i32
	%h = alloca i32
	%f = alloca i32
	%n = alloca i32
	%u = alloca i32
	%o = alloca i32
	%g = alloca i32
	%b = alloca i32
	%t = alloca i32
	%i = alloca i32
	%s = alloca i32
	%l = alloca i32
	%j = alloca i32
	%ad = alloca i32
	%z = alloca i32
	%w = alloca i32
	%r = alloca i32
	%e = alloca i32
	%d = alloca i32
	%v = alloca i32
	%q = alloca i32
	%m = alloca i32
	%k = alloca i32
	%af = alloca i32
	%ac = alloca i32
	%aa = alloca i32
	%y = alloca i32
	%x = alloca i32
	%ah = alloca i32
	%ag = alloca i32
	%ae = alloca i32
	%ab = alloca i32
	call void @printInt(i32 13)
	call void @printInt(i32 16)
	store i32 20, i32* %a
	%0 = load i32, i32* %a
	call void @printInt(i32 %0)
	%1 = load i32, i32* %a
	%2 = mul i32 70, %1
	call void @printInt(i32 %2)
	%3 = load i32, i32* %a
	%4 = load i32, i32* %a
	%5 = sdiv i32 %3, %4
	call void @printInt(i32 %5)
	call void @printInt(i32 37)
	store i32 18, i32* %b
	store i32 12, i32* %c
	%6 = load i32, i32* %b
	call void @printInt(i32 %6)
	call void @printInt(i32 35)
	%7 = load i32, i32* %a
	call void @printInt(i32 %7)
	%8 = load i32, i32* %a
	%9 = load i32, i32* %a
	%10 = add i32 %8, %9
	call void @printInt(i32 %10)
	%11 = load i32, i32* %a
	call void @printInt(i32 %11)
	%12 = load i32, i32* %c
	store i32 %12, i32* %d
	%13 = load i32, i32* %b
	%14 = add i32 %13, 4
	call void @printInt(i32 %14)
	%15 = load i32, i32* %d
	%16 = sub i32 2, %15
	call void @printInt(i32 %16)
	%17 = load i32, i32* %c
	%18 = mul i32 4, %17
	call void @printInt(i32 %18)
	%19 = load i32, i32* %b
	%20 = load i32, i32* %c
	%21 = mul i32 %19, %20
	%22 = add i32 %21, 60
	%23 = mul i32 24, %22
	%24 = load i32, i32* %c
	%25 = sdiv i32 %23, %24
	call void @printInt(i32 %25)
	%26 = load i32, i32* %c
	store i32 %26, i32* %e
	call void @printInt(i32 35)
	store i32 43, i32* %f
	store i32 33, i32* %g
	%27 = load i32, i32* %g
	call void @printInt(i32 %27)
	%28 = load i32, i32* %c
	%29 = mul i32 %28, 8
	%30 = sub i32 0, %29
	call void @printInt(i32 %30)
	%31 = load i32, i32* %f
	call void @printInt(i32 %31)
	%32 = add i32 49, 9
	call void @printInt(i32 %32)
	%33 = load i32, i32* %g
	store i32 %33, i32* %h
	%34 = load i32, i32* %h
	store i32 %34, i32* %i
	call void @printInt(i32 5)
	call void @printInt(i32 94)
	%35 = load i32, i32* %h
	call void @printInt(i32 %35)
	%36 = load i32, i32* %h
	%37 = load i32, i32* %b
	%38 = mul i32 %36, %37
	%39 = sdiv i32 %38, 21
	%40 = mul i32 %39, 99
	%41 = load i32, i32* %h
	%42 = sdiv i32 50, 30
	%43 = add i32 %41, %42
	%44 = sub i32 52, %43
	%45 = sdiv i32 %40, %44
	call void @printInt(i32 %45)
	%46 = load i32, i32* %h
	store i32 %46, i32* %j
	%47 = load i32, i32* %c
	call void @printInt(i32 %47)
	%48 = sdiv i32 24, 7
	%49 = mul i32 40, %48
	%50 = sub i32 %49, 9
	%51 = sdiv i32 %50, 23
	call void @printInt(i32 %51)
	store i32 13, i32* %k
	call void @printInt(i32 36)
	%52 = load i32, i32* %f
	%53 = load i32, i32* %h
	%54 = mul i32 %52, %53
	%55 = load i32, i32* %c
	%56 = add i32 40, %55
	%57 = add i32 %54, %56
	%58 = mul i32 %57, 50
	%59 = load i32, i32* %a
	%60 = mul i32 %58, %59
	%61 = sdiv i32 4, %60
	call void @printInt(i32 %61)
	store i32 38, i32* %l
	store i32 5, i32* %m
	call void @printInt(i32 11)
	call void @printInt(i32 47)
	%62 = load i32, i32* %e
	%63 = sdiv i32 %62, 37
	store i32 %63, i32* %n
	%64 = load i32, i32* %f
	call void @printInt(i32 %64)
	call void @printInt(i32 8)
	%65 = load i32, i32* %a
	call void @printInt(i32 %65)
	%66 = load i32, i32* %b
	call void @printInt(i32 %66)
	call void @printInt(i32 24)
	call void @printInt(i32 70)
	%67 = load i32, i32* %l
	%68 = mul i32 21, %67
	call void @printInt(i32 %68)
	%69 = load i32, i32* %g
	%70 = add i32 %69, 65
	%71 = load i32, i32* %f
	%72 = load i32, i32* %l
	%73 = add i32 %71, %72
	%74 = mul i32 %70, %73
	%75 = mul i32 41, %74
	%76 = mul i32 81, %75
	call void @printInt(i32 %76)
	call void @printInt(i32 45)
	%77 = load i32, i32* %h
	call void @printInt(i32 %77)
	%78 = sub i32 44, 74
	store i32 %78, i32* %o
	%79 = load i32, i32* %o
	%80 = sub i32 23, %79
	store i32 %80, i32* %p
	call void @printInt(i32 27)
	store i32 53, i32* %q
	%81 = load i32, i32* %a
	call void @printInt(i32 %81)
	%82 = load i32, i32* %o
	call void @printInt(i32 %82)
	%83 = load i32, i32* %o
	call void @printInt(i32 %83)
	call void @printInt(i32 35)
	store i32 23, i32* %r
	%84 = load i32, i32* %g
	call void @printInt(i32 %84)
	store i32 2, i32* %s
	call void @printInt(i32 11)
	%85 = sdiv i32 3, 69
	call void @printInt(i32 %85)
	%86 = load i32, i32* %n
	%87 = load i32, i32* %m
	%88 = sub i32 %86, %87
	store i32 %88, i32* %t
	%89 = load i32, i32* %c
	store i32 %89, i32* %u
	call void @printInt(i32 2)
	%90 = load i32, i32* %j
	%91 = load i32, i32* %n
	%92 = add i32 %91, 25
	%93 = load i32, i32* %d
	%94 = sub i32 %93, 80
	%95 = sub i32 %92, %94
	%96 = mul i32 1, %95
	%97 = sub i32 41, %96
	%98 = sub i32 %90, %97
	call void @printInt(i32 %98)
	call void @printInt(i32 19)
	store i32 34, i32* %v
	%99 = load i32, i32* %i
	call void @printInt(i32 %99)
	%100 = load i32, i32* %k
	%101 = mul i32 56, 100
	%102 = sub i32 %101, 47
	%103 = mul i32 %102, 3
	%104 = load i32, i32* %v
	%105 = sdiv i32 %104, 24
	%106 = sub i32 7, %105
	%107 = mul i32 %106, 61
	%108 = load i32, i32* %t
	%109 = mul i32 %107, %108
	%110 = sub i32 %109, 13
	%111 = add i32 %103, %110
	%112 = mul i32 26, %111
	%113 = add i32 %100, %112
	call void @printInt(i32 %113)
	%114 = load i32, i32* %p
	%115 = sdiv i32 1, %114
	%116 = add i32 %115, 33
	%117 = load i32, i32* %f
	%118 = sub i32 %116, %117
	call void @printInt(i32 %118)
	%119 = add i32 58, 75
	%120 = load i32, i32* %n
	%121 = sdiv i32 %120, 18
	%122 = add i32 %119, %121
	store i32 %122, i32* %w
	%123 = load i32, i32* %u
	call void @printInt(i32 %123)
	call void @printInt(i32 14)
	%124 = load i32, i32* %i
	%125 = load i32, i32* %r
	%126 = sdiv i32 %124, %125
	store i32 %126, i32* %x
	%127 = add i32 7, 15
	call void @printInt(i32 %127)
	call void @printInt(i32 1)
	call void @printInt(i32 47)
	%128 = load i32, i32* %c
	%129 = sub i32 53, 15
	%130 = sub i32 %128, %129
	%131 = add i32 13, %130
	call void @printInt(i32 %131)
	%132 = load i32, i32* %j
	store i32 %132, i32* %y
	%133 = load i32, i32* %w
	call void @printInt(i32 %133)
	%134 = load i32, i32* %p
	call void @printInt(i32 %134)
	%135 = load i32, i32* %i
	call void @printInt(i32 %135)
	%136 = load i32, i32* %p
	%137 = sdiv i32 %136, 53
	call void @printInt(i32 %137)
	%138 = load i32, i32* %q
	%139 = add i32 83, %138
	%140 = mul i32 31, %139
	%141 = add i32 25, 56
	%142 = load i32, i32* %t
	%143 = add i32 %141, %142
	%144 = sdiv i32 26, 21
	%145 = mul i32 %143, %144
	%146 = sub i32 10, %145
	%147 = add i32 %140, %146
	%148 = sdiv i32 10, %147
	call void @printInt(i32 %148)
	%149 = load i32, i32* %o
	%150 = add i32 %149, 17
	%151 = add i32 80, %150
	call void @printInt(i32 %151)
	call void @printInt(i32 24)
	%152 = load i32, i32* %f
	%153 = add i32 25, %152
	%154 = load i32, i32* %l
	%155 = mul i32 %153, %154
	call void @printInt(i32 %155)
	store i32 23, i32* %z
	call void @printInt(i32 2)
	store i32 48, i32* %aa
	%156 = load i32, i32* %p
	call void @printInt(i32 %156)
	store i32 13, i32* %ab
	%157 = load i32, i32* %u
	%158 = add i32 %157, 46
	%159 = sub i32 19, %158
	call void @printInt(i32 %159)
	%160 = load i32, i32* %t
	store i32 %160, i32* %ac
	%161 = load i32, i32* %r
	store i32 %161, i32* %ad
	%162 = load i32, i32* %ad
	%163 = load i32, i32* %s
	%164 = add i32 %162, %163
	%165 = mul i32 54, %164
	%166 = load i32, i32* %f
	%167 = load i32, i32* %u
	%168 = load i32, i32* %j
	%169 = mul i32 %167, %168
	%170 = add i32 %169, 64
	%171 = load i32, i32* %u
	%172 = sub i32 %170, %171
	%173 = load i32, i32* %g
	%174 = add i32 %172, %173
	%175 = sdiv i32 %166, %174
	%176 = sub i32 %165, %175
	%177 = sdiv i32 30, %176
	%178 = add i32 %177, 15
	call void @printInt(i32 %178)
	%179 = load i32, i32* %s
	%180 = sub i32 %179, 37
	call void @printInt(i32 %180)
	%181 = load i32, i32* %ad
	call void @printInt(i32 %181)
	%182 = load i32, i32* %s
	%183 = sub i32 %182, 19
	%184 = sub i32 %183, 63
	call void @printInt(i32 %184)
	%185 = load i32, i32* %z
	%186 = load i32, i32* %n
	%187 = add i32 %185, %186
	%188 = add i32 67, %187
	store i32 %188, i32* %ae
	call void @printInt(i32 14)
	%189 = load i32, i32* %n
	call void @printInt(i32 %189)
	%190 = load i32, i32* %p
	call void @printInt(i32 %190)
	call void @printInt(i32 32)
	call void @printInt(i32 15)
	%191 = load i32, i32* %e
	%192 = load i32, i32* %z
	%193 = sdiv i32 %192, 93
	%194 = add i32 %191, %193
	%195 = load i32, i32* %o
	%196 = sdiv i32 %194, %195
	%197 = load i32, i32* %c
	%198 = add i32 %196, %197
	call void @printInt(i32 %198)
	%199 = load i32, i32* %p
	store i32 %199, i32* %af
	call void @printInt(i32 47)
	%200 = load i32, i32* %w
	store i32 %200, i32* %ag
	%201 = load i32, i32* %p
	%202 = load i32, i32* %n
	%203 = mul i32 %201, %202
	%204 = load i32, i32* %t
	%205 = mul i32 %204, 14
	%206 = sub i32 %205, 23
	%207 = load i32, i32* %ac
	%208 = add i32 %207, 18
	%209 = sub i32 %206, %208
	%210 = add i32 %209, 36
	%211 = mul i32 %203, %210
	call void @printInt(i32 %211)
	%212 = load i32, i32* %i
	%213 = sub i32 62, %212
	store i32 %213, i32* %ah
	call void @printInt(i32 36)
	%214 = load i32, i32* %ad
	call void @printInt(i32 %214)
	%215 = load i32, i32* %u
	call void @printInt(i32 %215)
	%216 = load i32, i32* %af
	%217 = mul i32 31, %216
	call void @printInt(i32 %217)
	%218 = load i32, i32* %aa
	call void @printInt(i32 %218)
	call void @printInt(i32 26)
	ret i32 0
}
