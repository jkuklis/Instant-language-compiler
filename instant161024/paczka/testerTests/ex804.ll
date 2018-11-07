declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%d = alloca i32
	%g = alloca i32
	%c = alloca i32
	%e = alloca i32
	%b = alloca i32
	%h = alloca i32
	%i = alloca i32
	%f = alloca i32
	%m = alloca i32
	%l = alloca i32
	%k = alloca i32
	%j = alloca i32
	call void @printInt(i32 77)
	%0 = add i32 12, 14
	%1 = mul i32 42, 33
	%2 = sub i32 37, %1
	%3 = add i32 %2, 16
	%4 = sub i32 %0, %3
	call void @printInt(i32 %4)
	%5 = add i32 9, 7
	call void @printInt(i32 %5)
	call void @printInt(i32 7)
	call void @printInt(i32 106)
	%6 = add i32 47, 50
	%7 = sub i32 41, %6
	call void @printInt(i32 %7)
	call void @printInt(i32 92)
	call void @printInt(i32 5)
	%8 = sdiv i32 67, 26
	store i32 %8, i32* %a
	%9 = load i32, i32* %a
	%10 = load i32, i32* %a
	%11 = sdiv i32 63, %10
	%12 = load i32, i32* %a
	%13 = mul i32 8, %12
	%14 = sdiv i32 %11, %13
	%15 = sdiv i32 %9, %14
	%16 = sdiv i32 %15, 8
	%17 = load i32, i32* %a
	%18 = sdiv i32 %16, %17
	%19 = sub i32 %18, 19
	call void @printInt(i32 %19)
	call void @printInt(i32 71)
	%20 = load i32, i32* %a
	store i32 %20, i32* %b
	%21 = load i32, i32* %a
	call void @printInt(i32 %21)
	%22 = load i32, i32* %b
	store i32 %22, i32* %c
	store i32 48, i32* %d
	%23 = load i32, i32* %d
	%24 = mul i32 %23, 13
	call void @printInt(i32 %24)
	%25 = load i32, i32* %a
	store i32 %25, i32* %e
	%26 = load i32, i32* %c
	%27 = add i32 %26, 35
	call void @printInt(i32 %27)
	%28 = load i32, i32* %a
	%29 = sub i32 4, %28
	call void @printInt(i32 %29)
	call void @printInt(i32 28)
	%30 = load i32, i32* %e
	%31 = mul i32 15, %30
	call void @printInt(i32 %31)
	%32 = load i32, i32* %d
	%33 = mul i32 %32, 67
	call void @printInt(i32 %33)
	%34 = sdiv i32 47, 59
	store i32 %34, i32* %f
	%35 = load i32, i32* %b
	store i32 %35, i32* %g
	call void @printInt(i32 26)
	%36 = load i32, i32* %d
	%37 = load i32, i32* %a
	%38 = mul i32 %37, 93
	%39 = sdiv i32 %36, %38
	%40 = sdiv i32 %39, 22
	call void @printInt(i32 %40)
	%41 = load i32, i32* %a
	%42 = load i32, i32* %g
	%43 = sub i32 %41, %42
	%44 = mul i32 54, %43
	%45 = mul i32 19, 49
	%46 = sub i32 %44, %45
	%47 = add i32 14, %46
	%48 = load i32, i32* %d
	%49 = mul i32 %47, %48
	call void @printInt(i32 %49)
	%50 = load i32, i32* %c
	call void @printInt(i32 %50)
	%51 = load i32, i32* %a
	store i32 %51, i32* %h
	call void @printInt(i32 1)
	call void @printInt(i32 72)
	%52 = load i32, i32* %b
	%53 = load i32, i32* %d
	%54 = mul i32 %52, %53
	%55 = load i32, i32* %g
	%56 = add i32 %54, %55
	call void @printInt(i32 %56)
	%57 = load i32, i32* %a
	call void @printInt(i32 %57)
	%58 = load i32, i32* %h
	store i32 %58, i32* %i
	%59 = load i32, i32* %g
	%60 = add i32 %59, 11
	%61 = load i32, i32* %f
	%62 = sdiv i32 %61, 48
	%63 = load i32, i32* %c
	%64 = mul i32 47, %63
	%65 = sub i32 31, %64
	%66 = add i32 %62, %65
	%67 = add i32 %60, %66
	store i32 %67, i32* %j
	%68 = load i32, i32* %e
	%69 = load i32, i32* %e
	%70 = load i32, i32* %g
	%71 = sub i32 67, %70
	%72 = sub i32 45, %71
	%73 = sdiv i32 %69, %72
	%74 = add i32 %68, %73
	call void @printInt(i32 %74)
	store i32 48, i32* %k
	store i32 1, i32* %l
	%75 = mul i32 10, 31
	%76 = sdiv i32 29, %75
	call void @printInt(i32 %76)
	call void @printInt(i32 48)
	store i32 14, i32* %m
	%77 = sub i32 38, 19
	%78 = sub i32 5, 80
	%79 = add i32 %77, %78
	call void @printInt(i32 %79)
	%80 = load i32, i32* %i
	%81 = add i32 %80, 42
	%82 = mul i32 19, %81
	%83 = load i32, i32* %c
	%84 = add i32 %82, %83
	%85 = add i32 6, %84
	call void @printInt(i32 %85)
	%86 = load i32, i32* %h
	%87 = add i32 56, %86
	call void @printInt(i32 %87)
	ret i32 0
}
