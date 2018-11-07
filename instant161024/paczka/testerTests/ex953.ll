declare void @printInt(i32)
define i32 @main() {
entry:
	%d = alloca i32
	%b = alloca i32
	%a = alloca i32
	%e = alloca i32
	%c = alloca i32
	%i = alloca i32
	%g = alloca i32
	%f = alloca i32
	%l = alloca i32
	%k = alloca i32
	%j = alloca i32
	%h = alloca i32
	call void @printInt(i32 50)
	%0 = sub i32 13, 64
	%1 = sdiv i32 49, %0
	%2 = sub i32 23, 21
	%3 = sub i32 21, %2
	%4 = add i32 18, %3
	%5 = sdiv i32 %4, 20
	%6 = sub i32 45, %5
	%7 = sdiv i32 59, 6
	%8 = add i32 8, 17
	%9 = mul i32 55, %8
	%10 = sub i32 %9, 39
	%11 = sdiv i32 33, 13
	%12 = sub i32 37, 7
	%13 = mul i32 %11, %12
	%14 = sub i32 2, %13
	%15 = mul i32 %10, %14
	%16 = sdiv i32 35, 13
	%17 = sdiv i32 42, %16
	%18 = add i32 %17, 5
	%19 = sub i32 44, 10
	%20 = sub i32 52, %19
	%21 = sdiv i32 %18, %20
	%22 = sdiv i32 %21, 6
	%23 = sub i32 %22, 12
	%24 = add i32 55, %23
	%25 = sub i32 10, %24
	%26 = add i32 %25, 43
	%27 = add i32 %26, 44
	%28 = mul i32 %15, %27
	%29 = add i32 %28, 55
	%30 = mul i32 24, %29
	%31 = sub i32 %7, %30
	%32 = mul i32 27, 4
	%33 = sub i32 34, %32
	%34 = add i32 %33, 29
	%35 = sub i32 %34, 73
	%36 = mul i32 %35, 18
	%37 = sub i32 %31, %36
	%38 = mul i32 12, 34
	%39 = sdiv i32 54, 10
	%40 = add i32 %39, 89
	%41 = sdiv i32 %38, %40
	%42 = sdiv i32 %37, %41
	%43 = sdiv i32 %6, %42
	%44 = sub i32 9, 7
	%45 = mul i32 16, 31
	%46 = mul i32 %45, 15
	%47 = add i32 60, %46
	%48 = sub i32 %44, %47
	%49 = sdiv i32 %48, 110
	%50 = sdiv i32 %49, 28
	%51 = mul i32 %50, 27
	%52 = add i32 77, %51
	%53 = sub i32 %52, 14
	%54 = sdiv i32 63, %53
	%55 = sub i32 24, 6
	%56 = mul i32 4, %55
	%57 = add i32 %56, 39
	%58 = add i32 %57, 8
	%59 = add i32 %58, 13
	%60 = add i32 %59, 12
	%61 = sub i32 %54, %60
	%62 = mul i32 19, 11
	%63 = sub i32 %61, %62
	%64 = add i32 12, 25
	%65 = sub i32 7, %64
	%66 = sub i32 %65, 14
	%67 = sub i32 21, 39
	%68 = add i32 %66, %67
	%69 = sub i32 35, 10
	%70 = sub i32 %69, 70
	%71 = add i32 %68, %70
	%72 = mul i32 49, %71
	%73 = sub i32 %72, 40
	%74 = add i32 %63, %73
	%75 = sub i32 35, 46
	%76 = sdiv i32 %74, %75
	%77 = sdiv i32 3, %76
	%78 = sub i32 85, %77
	%79 = mul i32 %78, 48
	%80 = sdiv i32 %43, %79
	%81 = sub i32 %1, %80
	call void @printInt(i32 %81)
	store i32 100, i32* %a
	store i32 4, i32* %b
	call void @printInt(i32 56)
	call void @printInt(i32 3)
	call void @printInt(i32 39)
	call void @printInt(i32 41)
	%82 = load i32, i32* %a
	call void @printInt(i32 %82)
	%83 = load i32, i32* %b
	store i32 %83, i32* %c
	%84 = load i32, i32* %c
	call void @printInt(i32 %84)
	call void @printInt(i32 13)
	store i32 18, i32* %d
	%85 = load i32, i32* %c
	call void @printInt(i32 %85)
	call void @printInt(i32 15)
	%86 = load i32, i32* %c
	%87 = sub i32 32, %86
	call void @printInt(i32 %87)
	%88 = load i32, i32* %b
	call void @printInt(i32 %88)
	%89 = load i32, i32* %d
	%90 = load i32, i32* %d
	%91 = sub i32 %89, %90
	store i32 %91, i32* %e
	%92 = load i32, i32* %b
	%93 = load i32, i32* %e
	%94 = mul i32 %92, %93
	%95 = add i32 15, 104
	%96 = mul i32 %95, 1
	%97 = mul i32 %94, %96
	call void @printInt(i32 %97)
	%98 = load i32, i32* %d
	call void @printInt(i32 %98)
	%99 = load i32, i32* %a
	call void @printInt(i32 %99)
	call void @printInt(i32 54)
	%100 = load i32, i32* %c
	call void @printInt(i32 %100)
	call void @printInt(i32 24)
	%101 = load i32, i32* %b
	%102 = load i32, i32* %a
	%103 = mul i32 %101, %102
	%104 = sub i32 55, %103
	%105 = add i32 46, %104
	store i32 %105, i32* %f
	call void @printInt(i32 34)
	call void @printInt(i32 14)
	%106 = load i32, i32* %b
	%107 = load i32, i32* %d
	%108 = sub i32 %107, 27
	%109 = sdiv i32 %108, 60
	%110 = mul i32 %106, %109
	%111 = load i32, i32* %e
	%112 = sub i32 %110, %111
	call void @printInt(i32 %112)
	%113 = load i32, i32* %d
	call void @printInt(i32 %113)
	call void @printInt(i32 17)
	%114 = load i32, i32* %a
	call void @printInt(i32 %114)
	store i32 7, i32* %g
	%115 = load i32, i32* %d
	%116 = sub i32 %115, 20
	%117 = load i32, i32* %b
	%118 = mul i32 %116, %117
	%119 = mul i32 %118, 33
	call void @printInt(i32 %119)
	%120 = sub i32 31, 41
	call void @printInt(i32 %120)
	%121 = load i32, i32* %f
	%122 = add i32 71, %121
	call void @printInt(i32 %122)
	%123 = load i32, i32* %a
	store i32 %123, i32* %h
	call void @printInt(i32 22)
	%124 = load i32, i32* %d
	%125 = load i32, i32* %g
	%126 = load i32, i32* %d
	%127 = mul i32 %125, %126
	%128 = add i32 %124, %127
	store i32 %128, i32* %i
	%129 = load i32, i32* %e
	call void @printInt(i32 %129)
	%130 = load i32, i32* %e
	call void @printInt(i32 %130)
	%131 = load i32, i32* %a
	call void @printInt(i32 %131)
	%132 = load i32, i32* %e
	call void @printInt(i32 %132)
	store i32 138, i32* %j
	store i32 24, i32* %k
	%133 = sub i32 18, 1
	%134 = mul i32 6, %133
	store i32 %134, i32* %l
	call void @printInt(i32 48)
	%135 = load i32, i32* %i
	%136 = load i32, i32* %a
	%137 = add i32 %135, %136
	%138 = load i32, i32* %b
	%139 = sdiv i32 %137, %138
	%140 = add i32 20, %139
	%141 = load i32, i32* %d
	%142 = sdiv i32 %140, %141
	%143 = sub i32 13, 48
	%144 = sdiv i32 %142, %143
	%145 = load i32, i32* %b
	%146 = sdiv i32 %144, %145
	call void @printInt(i32 %146)
	call void @printInt(i32 7)
	ret i32 0
}
