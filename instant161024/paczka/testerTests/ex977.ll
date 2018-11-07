declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%d = alloca i32
	%g = alloca i32
	%e = alloca i32
	%i = alloca i32
	%h = alloca i32
	%f = alloca i32
	%n = alloca i32
	%m = alloca i32
	%l = alloca i32
	%k = alloca i32
	%j = alloca i32
	call void @printInt(i32 18)
	%0 = sub i32 32, 33
	call void @printInt(i32 %0)
	call void @printInt(i32 4)
	store i32 31, i32* %a
	call void @printInt(i32 103)
	call void @printInt(i32 8)
	%1 = load i32, i32* %a
	store i32 %1, i32* %b
	%2 = load i32, i32* %b
	store i32 %2, i32* %c
	%3 = load i32, i32* %b
	call void @printInt(i32 %3)
	%4 = load i32, i32* %b
	%5 = add i32 31, %4
	%6 = sdiv i32 %5, 46
	%7 = load i32, i32* %b
	%8 = sdiv i32 %7, 41
	%9 = load i32, i32* %c
	%10 = add i32 %8, %9
	%11 = add i32 %10, 39
	%12 = sub i32 %6, %11
	%13 = load i32, i32* %c
	%14 = load i32, i32* %b
	%15 = sdiv i32 %13, %14
	%16 = mul i32 %12, %15
	%17 = sub i32 20, %16
	%18 = load i32, i32* %a
	%19 = sdiv i32 %17, %18
	call void @printInt(i32 %19)
	store i32 87, i32* %d
	%20 = load i32, i32* %b
	%21 = mul i32 %20, 8
	call void @printInt(i32 %21)
	%22 = load i32, i32* %a
	call void @printInt(i32 %22)
	%23 = load i32, i32* %a
	call void @printInt(i32 %23)
	%24 = load i32, i32* %a
	call void @printInt(i32 %24)
	%25 = load i32, i32* %b
	%26 = load i32, i32* %c
	%27 = load i32, i32* %c
	%28 = sdiv i32 29, %27
	%29 = sdiv i32 %28, 20
	%30 = sub i32 25, %29
	%31 = sub i32 44, %30
	%32 = sub i32 %26, %31
	%33 = mul i32 %25, %32
	%34 = add i32 %33, 42
	call void @printInt(i32 %34)
	call void @printInt(i32 4)
	%35 = load i32, i32* %a
	%36 = load i32, i32* %a
	%37 = sub i32 %35, %36
	%38 = add i32 %37, 55
	%39 = load i32, i32* %b
	%40 = load i32, i32* %d
	%41 = mul i32 %39, %40
	%42 = sub i32 %38, %41
	%43 = sdiv i32 %42, 50
	%44 = load i32, i32* %a
	%45 = add i32 %43, %44
	%46 = load i32, i32* %c
	%47 = mul i32 32, %46
	%48 = add i32 %45, %47
	call void @printInt(i32 %48)
	store i32 5, i32* %e
	%49 = load i32, i32* %b
	call void @printInt(i32 %49)
	call void @printInt(i32 47)
	%50 = sub i32 18, 23
	store i32 %50, i32* %f
	%51 = mul i32 26, 13
	call void @printInt(i32 %51)
	%52 = load i32, i32* %a
	%53 = load i32, i32* %a
	%54 = add i32 %53, 19
	%55 = add i32 %52, %54
	call void @printInt(i32 %55)
	%56 = load i32, i32* %d
	%57 = sdiv i32 39, %56
	%58 = load i32, i32* %c
	%59 = mul i32 %57, %58
	%60 = sub i32 57, %59
	%61 = load i32, i32* %c
	%62 = mul i32 %60, %61
	store i32 %62, i32* %g
	%63 = load i32, i32* %g
	call void @printInt(i32 %63)
	call void @printInt(i32 35)
	%64 = load i32, i32* %d
	%65 = load i32, i32* %a
	%66 = add i32 %64, %65
	%67 = load i32, i32* %f
	%68 = sdiv i32 %66, %67
	call void @printInt(i32 %68)
	call void @printInt(i32 53)
	%69 = load i32, i32* %g
	%70 = mul i32 %69, 66
	%71 = load i32, i32* %d
	%72 = load i32, i32* %e
	%73 = mul i32 %71, %72
	%74 = load i32, i32* %c
	%75 = load i32, i32* %a
	%76 = mul i32 %74, %75
	%77 = sdiv i32 %76, 17
	%78 = mul i32 %77, 1
	%79 = load i32, i32* %d
	%80 = load i32, i32* %c
	%81 = load i32, i32* %e
	%82 = load i32, i32* %b
	%83 = mul i32 %81, %82
	%84 = sub i32 %80, %83
	%85 = load i32, i32* %e
	%86 = add i32 7, %85
	%87 = load i32, i32* %a
	%88 = sdiv i32 %87, 9
	%89 = sub i32 %88, 15
	%90 = sub i32 %86, %89
	%91 = sub i32 %84, %90
	%92 = mul i32 %79, %91
	%93 = mul i32 %78, %92
	%94 = sdiv i32 %73, %93
	%95 = load i32, i32* %a
	%96 = sub i32 45, %95
	%97 = mul i32 %94, %96
	%98 = mul i32 %70, %97
	store i32 %98, i32* %h
	call void @printInt(i32 16)
	call void @printInt(i32 15)
	%99 = load i32, i32* %a
	store i32 %99, i32* %i
	%100 = load i32, i32* %i
	store i32 %100, i32* %j
	call void @printInt(i32 55)
	call void @printInt(i32 18)
	call void @printInt(i32 5)
	call void @printInt(i32 44)
	%101 = load i32, i32* %h
	%102 = load i32, i32* %b
	%103 = add i32 %101, %102
	%104 = mul i32 0, %103
	%105 = load i32, i32* %h
	%106 = add i32 %104, %105
	%107 = add i32 %106, 36
	call void @printInt(i32 %107)
	call void @printInt(i32 20)
	call void @printInt(i32 22)
	%108 = load i32, i32* %g
	store i32 %108, i32* %k
	store i32 37, i32* %l
	call void @printInt(i32 19)
	%109 = load i32, i32* %a
	store i32 %109, i32* %m
	call void @printInt(i32 29)
	%110 = load i32, i32* %i
	call void @printInt(i32 %110)
	%111 = load i32, i32* %m
	%112 = load i32, i32* %f
	%113 = sdiv i32 %111, %112
	store i32 %113, i32* %n
	%114 = load i32, i32* %n
	call void @printInt(i32 %114)
	ret i32 0
}
