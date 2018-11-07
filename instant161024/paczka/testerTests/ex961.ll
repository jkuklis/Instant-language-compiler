declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%c = alloca i32
	%b = alloca i32
	%d = alloca i32
	%f = alloca i32
	%e = alloca i32
	%h = alloca i32
	%g = alloca i32
	%i = alloca i32
	call void @printInt(i32 3)
	%0 = add i32 23, 40
	%1 = mul i32 35, 17
	%2 = add i32 30, %1
	%3 = mul i32 %0, %2
	call void @printInt(i32 %3)
	call void @printInt(i32 9)
	call void @printInt(i32 18)
	call void @printInt(i32 31)
	%4 = sub i32 49, 23
	%5 = add i32 18, %4
	%6 = add i32 33, %5
	store i32 %6, i32* %a
	call void @printInt(i32 47)
	%7 = load i32, i32* %a
	store i32 %7, i32* %b
	call void @printInt(i32 34)
	%8 = load i32, i32* %b
	%9 = sub i32 %8, 16
	%10 = mul i32 12, %9
	%11 = mul i32 24, %10
	%12 = load i32, i32* %b
	%13 = sub i32 %11, %12
	%14 = load i32, i32* %a
	%15 = load i32, i32* %a
	%16 = load i32, i32* %a
	%17 = add i32 %15, %16
	%18 = sub i32 4, %17
	%19 = load i32, i32* %a
	%20 = sdiv i32 %18, %19
	%21 = mul i32 %14, %20
	%22 = add i32 34, 39
	%23 = add i32 %21, %22
	%24 = sdiv i32 %13, %23
	call void @printInt(i32 %24)
	%25 = load i32, i32* %b
	%26 = load i32, i32* %a
	%27 = mul i32 %26, 37
	%28 = mul i32 %27, 4
	%29 = sub i32 %25, %28
	%30 = sub i32 14, %29
	store i32 %30, i32* %c
	%31 = load i32, i32* %a
	%32 = load i32, i32* %a
	%33 = sub i32 %31, %32
	%34 = mul i32 38, %33
	%35 = load i32, i32* %b
	%36 = mul i32 %34, %35
	%37 = load i32, i32* %c
	%38 = sub i32 %36, %37
	%39 = load i32, i32* %c
	%40 = mul i32 %38, %39
	call void @printInt(i32 %40)
	call void @printInt(i32 48)
	%41 = load i32, i32* %a
	%42 = load i32, i32* %b
	%43 = add i32 %41, %42
	%44 = load i32, i32* %c
	%45 = sdiv i32 %43, %44
	%46 = sdiv i32 %45, 48
	%47 = load i32, i32* %b
	%48 = mul i32 %46, %47
	%49 = mul i32 23, %48
	call void @printInt(i32 %49)
	%50 = load i32, i32* %a
	call void @printInt(i32 %50)
	%51 = load i32, i32* %a
	store i32 %51, i32* %d
	call void @printInt(i32 12)
	%52 = load i32, i32* %d
	%53 = sub i32 %52, 37
	call void @printInt(i32 %53)
	%54 = load i32, i32* %d
	call void @printInt(i32 %54)
	call void @printInt(i32 54)
	%55 = load i32, i32* %d
	%56 = sdiv i32 4, %55
	call void @printInt(i32 %56)
	%57 = load i32, i32* %d
	store i32 %57, i32* %e
	%58 = load i32, i32* %e
	call void @printInt(i32 %58)
	call void @printInt(i32 8)
	%59 = mul i32 32, 16
	%60 = sub i32 %59, 52
	%61 = load i32, i32* %b
	%62 = sub i32 %60, %61
	%63 = load i32, i32* %c
	%64 = sub i32 %63, 20
	%65 = add i32 18, %64
	%66 = load i32, i32* %b
	%67 = sub i32 %65, %66
	%68 = sdiv i32 %67, 9
	%69 = add i32 45, %68
	%70 = sub i32 %62, %69
	%71 = load i32, i32* %a
	%72 = sub i32 %70, %71
	call void @printInt(i32 %72)
	%73 = load i32, i32* %e
	call void @printInt(i32 %73)
	%74 = load i32, i32* %b
	%75 = load i32, i32* %b
	%76 = sub i32 %74, %75
	%77 = add i32 50, %76
	call void @printInt(i32 %77)
	call void @printInt(i32 43)
	%78 = load i32, i32* %d
	%79 = sub i32 5, %78
	%80 = mul i32 %79, 8
	%81 = sdiv i32 %80, 14
	call void @printInt(i32 %81)
	%82 = load i32, i32* %b
	%83 = sub i32 %82, 41
	call void @printInt(i32 %83)
	%84 = load i32, i32* %c
	store i32 %84, i32* %f
	%85 = load i32, i32* %b
	call void @printInt(i32 %85)
	%86 = load i32, i32* %a
	%87 = load i32, i32* %d
	%88 = sdiv i32 %86, %87
	call void @printInt(i32 %88)
	call void @printInt(i32 6)
	%89 = load i32, i32* %f
	call void @printInt(i32 %89)
	%90 = load i32, i32* %d
	call void @printInt(i32 %90)
	%91 = load i32, i32* %c
	call void @printInt(i32 %91)
	%92 = load i32, i32* %d
	call void @printInt(i32 %92)
	call void @printInt(i32 76)
	%93 = load i32, i32* %e
	%94 = sub i32 47, %93
	%95 = load i32, i32* %b
	%96 = sub i32 4, 79
	%97 = add i32 %96, 63
	%98 = load i32, i32* %f
	%99 = mul i32 %97, %98
	%100 = load i32, i32* %d
	%101 = load i32, i32* %a
	%102 = load i32, i32* %d
	%103 = sub i32 %101, %102
	%104 = load i32, i32* %f
	%105 = mul i32 %104, 9
	%106 = sub i32 %103, %105
	%107 = add i32 98, %106
	%108 = sdiv i32 %107, 14
	%109 = mul i32 %100, %108
	%110 = mul i32 %99, %109
	%111 = load i32, i32* %a
	%112 = sdiv i32 35, %111
	%113 = sub i32 %110, %112
	%114 = sub i32 %95, %113
	%115 = sdiv i32 %94, %114
	call void @printInt(i32 %115)
	call void @printInt(i32 5)
	store i32 39, i32* %g
	%116 = load i32, i32* %c
	%117 = load i32, i32* %e
	%118 = load i32, i32* %d
	%119 = load i32, i32* %a
	%120 = sub i32 39, %119
	%121 = sdiv i32 %118, %120
	%122 = add i32 12, %121
	%123 = mul i32 %117, %122
	%124 = sdiv i32 %116, %123
	%125 = mul i32 53, %124
	%126 = add i32 32, %125
	call void @printInt(i32 %126)
	%127 = load i32, i32* %g
	call void @printInt(i32 %127)
	%128 = load i32, i32* %f
	%129 = sdiv i32 %128, 74
	%130 = load i32, i32* %g
	%131 = sub i32 2, 2
	%132 = mul i32 35, %131
	%133 = mul i32 %130, %132
	%134 = sdiv i32 %133, 23
	%135 = load i32, i32* %d
	%136 = load i32, i32* %e
	%137 = sub i32 %135, %136
	%138 = load i32, i32* %c
	%139 = sub i32 %138, 23
	%140 = sub i32 %137, %139
	%141 = sub i32 11, 3
	%142 = mul i32 %140, %141
	%143 = add i32 %134, %142
	%144 = sdiv i32 %143, 8
	%145 = add i32 %129, %144
	call void @printInt(i32 %145)
	call void @printInt(i32 58)
	%146 = load i32, i32* %a
	%147 = load i32, i32* %a
	%148 = add i32 %146, %147
	%149 = mul i32 15, %148
	call void @printInt(i32 %149)
	call void @printInt(i32 0)
	%150 = load i32, i32* %e
	call void @printInt(i32 %150)
	%151 = load i32, i32* %c
	%152 = load i32, i32* %b
	%153 = sdiv i32 %151, %152
	%154 = load i32, i32* %c
	%155 = sdiv i32 %153, %154
	%156 = load i32, i32* %a
	%157 = mul i32 %155, %156
	%158 = load i32, i32* %c
	%159 = mul i32 %157, %158
	%160 = sub i32 %159, 11
	call void @printInt(i32 %160)
	%161 = add i32 6, 30
	%162 = sub i32 14, %161
	call void @printInt(i32 %162)
	call void @printInt(i32 85)
	%163 = load i32, i32* %c
	%164 = load i32, i32* %f
	%165 = sub i32 %163, %164
	call void @printInt(i32 %165)
	%166 = load i32, i32* %f
	call void @printInt(i32 %166)
	%167 = load i32, i32* %f
	store i32 %167, i32* %h
	%168 = load i32, i32* %e
	%169 = load i32, i32* %c
	%170 = mul i32 %168, %169
	%171 = load i32, i32* %f
	%172 = sub i32 %170, %171
	call void @printInt(i32 %172)
	%173 = load i32, i32* %h
	%174 = mul i32 68, %173
	call void @printInt(i32 %174)
	store i32 69, i32* %i
	call void @printInt(i32 31)
	%175 = load i32, i32* %f
	%176 = load i32, i32* %d
	%177 = load i32, i32* %h
	%178 = sdiv i32 %177, 19
	%179 = sdiv i32 %176, %178
	%180 = add i32 %175, %179
	%181 = load i32, i32* %a
	%182 = mul i32 %180, %181
	call void @printInt(i32 %182)
	%183 = load i32, i32* %f
	call void @printInt(i32 %183)
	%184 = load i32, i32* %c
	call void @printInt(i32 %184)
	%185 = load i32, i32* %h
	call void @printInt(i32 %185)
	%186 = load i32, i32* %f
	call void @printInt(i32 %186)
	%187 = load i32, i32* %c
	%188 = mul i32 %187, 83
	%189 = sdiv i32 %188, 36
	%190 = add i32 %189, 57
	call void @printInt(i32 %190)
	call void @printInt(i32 67)
	call void @printInt(i32 74)
	%191 = load i32, i32* %i
	%192 = sdiv i32 %191, 3
	call void @printInt(i32 %192)
	%193 = load i32, i32* %h
	call void @printInt(i32 %193)
	%194 = load i32, i32* %c
	call void @printInt(i32 %194)
	ret i32 0
}
