declare void @printInt(i32)
define i32 @main() {
entry:
	%e = alloca i32
	%h = alloca i32
	%b = alloca i32
	%a = alloca i32
	%l = alloca i32
	%i = alloca i32
	%n = alloca i32
	%d = alloca i32
	%c = alloca i32
	%v = alloca i32
	%j = alloca i32
	%g = alloca i32
	%f = alloca i32
	%r = alloca i32
	%k = alloca i32
	%y = alloca i32
	%w = alloca i32
	%p = alloca i32
	%m = alloca i32
	%z = alloca i32
	%x = alloca i32
	%u = alloca i32
	%t = alloca i32
	%s = alloca i32
	%q = alloca i32
	%o = alloca i32
	%aa = alloca i32
	store i32 10, i32* %a
	call void @printInt(i32 50)
	%0 = load i32, i32* %a
	call void @printInt(i32 %0)
	call void @printInt(i32 10)
	%1 = sub i32 5, 9
	%2 = add i32 5, %1
	store i32 %2, i32* %b
	%3 = sub i32 14, 6
	call void @printInt(i32 %3)
	store i32 21, i32* %c
	%4 = load i32, i32* %c
	store i32 %4, i32* %d
	call void @printInt(i32 15)
	call void @printInt(i32 33)
	%5 = load i32, i32* %d
	call void @printInt(i32 %5)
	store i32 29, i32* %e
	%6 = add i32 4, 24
	call void @printInt(i32 %6)
	%7 = load i32, i32* %a
	call void @printInt(i32 %7)
	%8 = load i32, i32* %c
	%9 = load i32, i32* %e
	%10 = sdiv i32 %8, %9
	call void @printInt(i32 %10)
	call void @printInt(i32 34)
	%11 = load i32, i32* %b
	%12 = add i32 %11, 22
	%13 = load i32, i32* %b
	%14 = add i32 %12, %13
	%15 = sub i32 21, %14
	call void @printInt(i32 %15)
	%16 = load i32, i32* %c
	%17 = add i32 47, %16
	store i32 %17, i32* %f
	call void @printInt(i32 14)
	call void @printInt(i32 1)
	%18 = load i32, i32* %e
	%19 = sdiv i32 %18, 55
	%20 = sub i32 52, %19
	call void @printInt(i32 %20)
	%21 = load i32, i32* %e
	%22 = load i32, i32* %d
	%23 = sdiv i32 %21, %22
	%24 = load i32, i32* %e
	%25 = sdiv i32 %23, %24
	call void @printInt(i32 %25)
	%26 = load i32, i32* %f
	%27 = load i32, i32* %a
	%28 = mul i32 %26, %27
	%29 = sub i32 %28, 16
	store i32 %29, i32* %g
	call void @printInt(i32 47)
	call void @printInt(i32 8)
	store i32 58, i32* %h
	%30 = load i32, i32* %g
	%31 = load i32, i32* %h
	%32 = sdiv i32 %30, %31
	call void @printInt(i32 %32)
	call void @printInt(i32 2)
	%33 = load i32, i32* %h
	%34 = load i32, i32* %d
	%35 = load i32, i32* %b
	%36 = sdiv i32 %34, %35
	%37 = load i32, i32* %e
	%38 = load i32, i32* %b
	%39 = load i32, i32* %b
	%40 = add i32 %38, %39
	%41 = add i32 %37, %40
	%42 = sdiv i32 %36, %41
	%43 = mul i32 0, %42
	%44 = sdiv i32 %43, 109
	%45 = add i32 %33, %44
	call void @printInt(i32 %45)
	call void @printInt(i32 3)
	%46 = load i32, i32* %e
	%47 = load i32, i32* %f
	%48 = mul i32 %46, %47
	store i32 %48, i32* %i
	call void @printInt(i32 88)
	call void @printInt(i32 66)
	%49 = load i32, i32* %b
	%50 = load i32, i32* %a
	%51 = sdiv i32 %49, %50
	%52 = load i32, i32* %c
	%53 = sdiv i32 %51, %52
	call void @printInt(i32 %53)
	call void @printInt(i32 47)
	store i32 91, i32* %j
	call void @printInt(i32 29)
	store i32 2, i32* %k
	%54 = load i32, i32* %e
	%55 = add i32 %54, 57
	store i32 %55, i32* %l
	call void @printInt(i32 48)
	%56 = load i32, i32* %a
	%57 = add i32 30, 56
	%58 = sdiv i32 %56, %57
	call void @printInt(i32 %58)
	%59 = load i32, i32* %h
	call void @printInt(i32 %59)
	%60 = load i32, i32* %h
	store i32 %60, i32* %m
	%61 = load i32, i32* %h
	%62 = load i32, i32* %e
	%63 = load i32, i32* %g
	%64 = mul i32 %62, %63
	%65 = load i32, i32* %h
	%66 = mul i32 %64, %65
	%67 = load i32, i32* %b
	%68 = add i32 %66, %67
	%69 = add i32 13, 14
	%70 = load i32, i32* %d
	%71 = sub i32 %69, %70
	%72 = mul i32 %68, %71
	%73 = mul i32 57, %72
	%74 = add i32 %61, %73
	%75 = load i32, i32* %h
	%76 = sdiv i32 %74, %75
	call void @printInt(i32 %76)
	%77 = mul i32 14, 17
	%78 = mul i32 18, %77
	call void @printInt(i32 %78)
	store i32 48, i32* %n
	%79 = load i32, i32* %l
	call void @printInt(i32 %79)
	%80 = load i32, i32* %j
	call void @printInt(i32 %80)
	%81 = load i32, i32* %l
	%82 = sub i32 %81, 67
	call void @printInt(i32 %82)
	%83 = load i32, i32* %i
	call void @printInt(i32 %83)
	%84 = load i32, i32* %j
	call void @printInt(i32 %84)
	%85 = load i32, i32* %l
	%86 = sdiv i32 %85, 27
	call void @printInt(i32 %86)
	%87 = load i32, i32* %l
	%88 = load i32, i32* %l
	%89 = load i32, i32* %i
	%90 = load i32, i32* %n
	%91 = sdiv i32 %89, %90
	%92 = sdiv i32 %88, %91
	%93 = sub i32 %87, %92
	call void @printInt(i32 %93)
	%94 = sdiv i32 48, 23
	%95 = load i32, i32* %e
	%96 = mul i32 %94, %95
	store i32 %96, i32* %o
	%97 = load i32, i32* %i
	call void @printInt(i32 %97)
	%98 = load i32, i32* %a
	call void @printInt(i32 %98)
	%99 = load i32, i32* %i
	%100 = sdiv i32 87, %99
	%101 = mul i32 %100, 26
	store i32 %101, i32* %p
	%102 = load i32, i32* %n
	store i32 %102, i32* %q
	%103 = load i32, i32* %e
	call void @printInt(i32 %103)
	%104 = load i32, i32* %b
	%105 = load i32, i32* %h
	%106 = mul i32 %105, 25
	%107 = mul i32 %104, %106
	%108 = sdiv i32 %107, 12
	store i32 %108, i32* %r
	%109 = load i32, i32* %d
	%110 = sub i32 28, %109
	call void @printInt(i32 %110)
	call void @printInt(i32 41)
	%111 = load i32, i32* %n
	call void @printInt(i32 %111)
	%112 = load i32, i32* %e
	%113 = load i32, i32* %g
	%114 = mul i32 %112, %113
	%115 = add i32 %114, 2
	store i32 %115, i32* %s
	call void @printInt(i32 21)
	%116 = load i32, i32* %i
	call void @printInt(i32 %116)
	store i32 43, i32* %t
	%117 = load i32, i32* %a
	store i32 %117, i32* %u
	call void @printInt(i32 12)
	%118 = add i32 35, 19
	call void @printInt(i32 %118)
	%119 = load i32, i32* %f
	store i32 %119, i32* %v
	%120 = load i32, i32* %f
	%121 = load i32, i32* %p
	%122 = add i32 18, %121
	%123 = add i32 %120, %122
	call void @printInt(i32 %123)
	%124 = load i32, i32* %l
	call void @printInt(i32 %124)
	%125 = load i32, i32* %r
	call void @printInt(i32 %125)
	%126 = load i32, i32* %b
	call void @printInt(i32 %126)
	%127 = load i32, i32* %v
	%128 = add i32 5, %127
	%129 = add i32 %128, 46
	call void @printInt(i32 %129)
	%130 = load i32, i32* %v
	%131 = load i32, i32* %k
	%132 = sdiv i32 %130, %131
	%133 = load i32, i32* %n
	%134 = sdiv i32 %132, %133
	call void @printInt(i32 %134)
	%135 = load i32, i32* %v
	%136 = add i32 %135, 20
	call void @printInt(i32 %136)
	%137 = load i32, i32* %c
	%138 = add i32 49, %137
	store i32 %138, i32* %w
	call void @printInt(i32 26)
	call void @printInt(i32 27)
	call void @printInt(i32 32)
	%139 = load i32, i32* %m
	%140 = add i32 10, 5
	%141 = sdiv i32 %139, %140
	%142 = load i32, i32* %a
	%143 = add i32 %141, %142
	%144 = sdiv i32 %143, 8
	call void @printInt(i32 %144)
	%145 = load i32, i32* %j
	%146 = load i32, i32* %k
	%147 = sdiv i32 %146, 39
	%148 = add i32 %145, %147
	%149 = sdiv i32 7, %148
	store i32 %149, i32* %x
	%150 = load i32, i32* %n
	call void @printInt(i32 %150)
	%151 = load i32, i32* %g
	%152 = sdiv i32 %151, 65
	%153 = mul i32 %152, 3
	call void @printInt(i32 %153)
	%154 = load i32, i32* %a
	%155 = sub i32 %154, 11
	%156 = load i32, i32* %r
	%157 = sub i32 %155, %156
	store i32 %157, i32* %y
	call void @printInt(i32 0)
	call void @printInt(i32 13)
	%158 = load i32, i32* %i
	%159 = mul i32 %158, 71
	%160 = load i32, i32* %j
	%161 = add i32 %159, %160
	call void @printInt(i32 %161)
	%162 = sdiv i32 11, 20
	call void @printInt(i32 %162)
	%163 = load i32, i32* %v
	%164 = sdiv i32 27, %163
	%165 = mul i32 %164, 74
	%166 = sub i32 %165, 12
	call void @printInt(i32 %166)
	%167 = sdiv i32 4, 51
	call void @printInt(i32 %167)
	store i32 31, i32* %z
	%168 = sub i32 3, 14
	%169 = add i32 5, %168
	%170 = load i32, i32* %y
	%171 = sdiv i32 %169, %170
	store i32 %171, i32* %aa
	%172 = load i32, i32* %h
	call void @printInt(i32 %172)
	%173 = mul i32 2, 30
	%174 = load i32, i32* %w
	%175 = add i32 %173, %174
	call void @printInt(i32 %175)
	ret i32 0
}
