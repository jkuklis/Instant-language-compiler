declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%d = alloca i32
	%c = alloca i32
	%e = alloca i32
	%b = alloca i32
	%k = alloca i32
	%h = alloca i32
	%j = alloca i32
	%g = alloca i32
	%f = alloca i32
	%n = alloca i32
	%m = alloca i32
	%l = alloca i32
	%i = alloca i32
	%0 = sdiv i32 62, 17
	call void @printInt(i32 %0)
	call void @printInt(i32 78)
	%1 = sdiv i32 78, 36
	%2 = sub i32 41, %1
	%3 = mul i32 28, 38
	%4 = sdiv i32 %2, %3
	%5 = add i32 34, %4
	%6 = sdiv i32 %5, 1
	%7 = sub i32 %6, 0
	%8 = add i32 %7, 61
	%9 = sdiv i32 18, %8
	%10 = add i32 51, %9
	store i32 %10, i32* %a
	%11 = sdiv i32 57, 40
	%12 = add i32 42, %11
	%13 = add i32 %12, 51
	call void @printInt(i32 %13)
	call void @printInt(i32 65)
	call void @printInt(i32 43)
	call void @printInt(i32 7)
	%14 = load i32, i32* %a
	call void @printInt(i32 %14)
	call void @printInt(i32 27)
	%15 = load i32, i32* %a
	%16 = sub i32 52, %15
	%17 = load i32, i32* %a
	%18 = sub i32 %16, %17
	call void @printInt(i32 %18)
	%19 = sdiv i32 58, 95
	call void @printInt(i32 %19)
	store i32 35, i32* %b
	call void @printInt(i32 31)
	%20 = load i32, i32* %b
	call void @printInt(i32 %20)
	%21 = load i32, i32* %a
	store i32 %21, i32* %c
	%22 = load i32, i32* %a
	call void @printInt(i32 %22)
	call void @printInt(i32 79)
	%23 = sub i32 9, 52
	call void @printInt(i32 %23)
	call void @printInt(i32 59)
	call void @printInt(i32 58)
	call void @printInt(i32 21)
	%24 = load i32, i32* %b
	call void @printInt(i32 %24)
	%25 = load i32, i32* %a
	%26 = load i32, i32* %c
	%27 = mul i32 %25, %26
	%28 = sdiv i32 15, %27
	call void @printInt(i32 %28)
	%29 = load i32, i32* %b
	%30 = load i32, i32* %c
	%31 = sub i32 %30, 14
	%32 = load i32, i32* %c
	%33 = add i32 %31, %32
	%34 = load i32, i32* %c
	%35 = sdiv i32 51, %34
	%36 = sub i32 %33, %35
	%37 = mul i32 9, %36
	%38 = sdiv i32 %29, %37
	%39 = sub i32 33, %38
	call void @printInt(i32 %39)
	%40 = load i32, i32* %c
	call void @printInt(i32 %40)
	call void @printInt(i32 43)
	%41 = load i32, i32* %a
	store i32 %41, i32* %d
	%42 = sdiv i32 38, 12
	call void @printInt(i32 %42)
	%43 = load i32, i32* %a
	call void @printInt(i32 %43)
	%44 = load i32, i32* %d
	call void @printInt(i32 %44)
	%45 = load i32, i32* %c
	store i32 %45, i32* %e
	call void @printInt(i32 14)
	call void @printInt(i32 47)
	%46 = load i32, i32* %e
	store i32 %46, i32* %f
	%47 = load i32, i32* %e
	%48 = mul i32 51, %47
	%49 = sub i32 15, %48
	%50 = load i32, i32* %b
	%51 = sub i32 114, %50
	%52 = sub i32 %51, 34
	%53 = load i32, i32* %a
	%54 = sub i32 %52, %53
	%55 = load i32, i32* %d
	%56 = sdiv i32 %55, 28
	%57 = load i32, i32* %e
	%58 = sdiv i32 %57, 107
	%59 = mul i32 60, %58
	%60 = add i32 %56, %59
	%61 = sub i32 %54, %60
	%62 = load i32, i32* %f
	%63 = mul i32 %61, %62
	%64 = sdiv i32 %63, 16
	%65 = sub i32 %49, %64
	%66 = load i32, i32* %d
	%67 = load i32, i32* %d
	%68 = sub i32 39, %67
	%69 = load i32, i32* %d
	%70 = add i32 %68, %69
	%71 = add i32 %66, %70
	%72 = add i32 %71, 9
	%73 = sdiv i32 %65, %72
	call void @printInt(i32 %73)
	%74 = load i32, i32* %a
	call void @printInt(i32 %74)
	%75 = load i32, i32* %d
	call void @printInt(i32 %75)
	%76 = load i32, i32* %a
	store i32 %76, i32* %g
	store i32 17, i32* %h
	%77 = load i32, i32* %d
	store i32 %77, i32* %i
	%78 = load i32, i32* %h
	call void @printInt(i32 %78)
	%79 = load i32, i32* %e
	store i32 %79, i32* %j
	%80 = load i32, i32* %b
	call void @printInt(i32 %80)
	%81 = load i32, i32* %e
	call void @printInt(i32 %81)
	%82 = load i32, i32* %d
	call void @printInt(i32 %82)
	%83 = load i32, i32* %c
	store i32 %83, i32* %k
	call void @printInt(i32 7)
	call void @printInt(i32 15)
	%84 = load i32, i32* %f
	%85 = sdiv i32 %84, 38
	%86 = mul i32 7, %85
	call void @printInt(i32 %86)
	%87 = load i32, i32* %k
	call void @printInt(i32 %87)
	%88 = load i32, i32* %h
	%89 = sdiv i32 17, %88
	call void @printInt(i32 %89)
	%90 = load i32, i32* %j
	%91 = load i32, i32* %e
	%92 = sub i32 %91, 40
	%93 = sdiv i32 %90, %92
	%94 = sub i32 35, %93
	%95 = add i32 28, %94
	%96 = load i32, i32* %e
	%97 = add i32 %95, %96
	%98 = load i32, i32* %a
	%99 = add i32 %98, 67
	%100 = load i32, i32* %c
	%101 = sdiv i32 %99, %100
	%102 = load i32, i32* %g
	%103 = sdiv i32 %102, 22
	%104 = sdiv i32 9, %103
	%105 = load i32, i32* %g
	%106 = sub i32 %105, 19
	%107 = add i32 %104, %106
	%108 = sub i32 %101, %107
	%109 = load i32, i32* %j
	%110 = add i32 34, %109
	%111 = add i32 %108, %110
	%112 = load i32, i32* %d
	%113 = sub i32 11, %112
	%114 = sub i32 %111, %113
	%115 = mul i32 %97, %114
	call void @printInt(i32 %115)
	%116 = mul i32 30, 42
	%117 = sdiv i32 %116, 41
	store i32 %117, i32* %l
	call void @printInt(i32 22)
	%118 = load i32, i32* %k
	call void @printInt(i32 %118)
	%119 = load i32, i32* %d
	call void @printInt(i32 %119)
	%120 = load i32, i32* %a
	%121 = load i32, i32* %h
	%122 = load i32, i32* %b
	%123 = sub i32 %121, %122
	%124 = load i32, i32* %b
	%125 = mul i32 17, %124
	%126 = sdiv i32 %123, %125
	%127 = mul i32 %120, %126
	%128 = load i32, i32* %k
	%129 = add i32 %127, %128
	call void @printInt(i32 %129)
	%130 = sub i32 38, 27
	call void @printInt(i32 %130)
	call void @printInt(i32 21)
	call void @printInt(i32 49)
	store i32 12, i32* %m
	store i32 34, i32* %n
	%131 = load i32, i32* %a
	call void @printInt(i32 %131)
	ret i32 0
}
