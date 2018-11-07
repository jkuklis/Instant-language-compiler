declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%d = alloca i32
	%c = alloca i32
	%b = alloca i32
	%e = alloca i32
	%f = alloca i32
	%h = alloca i32
	%g = alloca i32
	call void @printInt(i32 90)
	%0 = sub i32 35, 59
	%1 = sub i32 39, %0
	%2 = mul i32 11, 15
	%3 = mul i32 %1, %2
	%4 = sub i32 18, 64
	%5 = add i32 %4, 32
	%6 = mul i32 %3, %5
	store i32 %6, i32* %a
	call void @printInt(i32 3)
	%7 = load i32, i32* %a
	call void @printInt(i32 %7)
	%8 = load i32, i32* %a
	%9 = load i32, i32* %a
	%10 = sdiv i32 %8, %9
	%11 = load i32, i32* %a
	%12 = load i32, i32* %a
	%13 = mul i32 %11, %12
	%14 = add i32 %10, %13
	%15 = load i32, i32* %a
	%16 = sub i32 %14, %15
	call void @printInt(i32 %16)
	call void @printInt(i32 11)
	%17 = load i32, i32* %a
	%18 = mul i32 %17, 16
	store i32 %18, i32* %b
	%19 = load i32, i32* %b
	store i32 %19, i32* %c
	%20 = load i32, i32* %b
	%21 = load i32, i32* %c
	%22 = sub i32 32, 14
	%23 = add i32 %21, %22
	%24 = add i32 %20, %23
	%25 = sub i32 39, %24
	call void @printInt(i32 %25)
	%26 = load i32, i32* %a
	store i32 %26, i32* %d
	%27 = load i32, i32* %b
	%28 = load i32, i32* %c
	%29 = mul i32 %27, %28
	call void @printInt(i32 %29)
	%30 = load i32, i32* %c
	call void @printInt(i32 %30)
	%31 = load i32, i32* %c
	store i32 %31, i32* %e
	%32 = sdiv i32 28, 20
	%33 = load i32, i32* %e
	%34 = mul i32 29, %33
	%35 = load i32, i32* %d
	%36 = sdiv i32 35, 19
	%37 = add i32 %35, %36
	%38 = load i32, i32* %a
	%39 = add i32 %38, 69
	%40 = load i32, i32* %c
	%41 = add i32 %39, %40
	%42 = load i32, i32* %d
	%43 = sub i32 %41, %42
	%44 = sdiv i32 %37, %43
	%45 = load i32, i32* %a
	%46 = sub i32 %44, %45
	%47 = sdiv i32 %34, %46
	%48 = load i32, i32* %d
	%49 = load i32, i32* %d
	%50 = add i32 %48, %49
	%51 = sub i32 %47, %50
	%52 = add i32 %51, 36
	%53 = mul i32 %32, %52
	%54 = load i32, i32* %e
	%55 = mul i32 %53, %54
	call void @printInt(i32 %55)
	%56 = load i32, i32* %c
	%57 = load i32, i32* %c
	%58 = load i32, i32* %b
	%59 = mul i32 12, 87
	%60 = add i32 %58, %59
	%61 = add i32 %57, %60
	%62 = sub i32 22, 74
	%63 = add i32 %61, %62
	%64 = sdiv i32 %56, %63
	%65 = mul i32 %64, 8
	%66 = sub i32 2, %65
	call void @printInt(i32 %66)
	%67 = sub i32 9, 15
	call void @printInt(i32 %67)
	call void @printInt(i32 18)
	call void @printInt(i32 14)
	call void @printInt(i32 27)
	call void @printInt(i32 35)
	%68 = load i32, i32* %a
	call void @printInt(i32 %68)
	%69 = load i32, i32* %d
	%70 = mul i32 %69, 48
	%71 = load i32, i32* %a
	%72 = load i32, i32* %b
	%73 = load i32, i32* %d
	%74 = add i32 %73, 56
	%75 = mul i32 %72, %74
	%76 = mul i32 %71, %75
	%77 = add i32 %70, %76
	%78 = load i32, i32* %d
	%79 = sub i32 %77, %78
	%80 = add i32 66, %79
	store i32 %80, i32* %f
	%81 = load i32, i32* %b
	call void @printInt(i32 %81)
	call void @printInt(i32 8)
	call void @printInt(i32 48)
	call void @printInt(i32 83)
	%82 = load i32, i32* %d
	store i32 %82, i32* %g
	%83 = load i32, i32* %f
	call void @printInt(i32 %83)
	call void @printInt(i32 42)
	store i32 35, i32* %h
	call void @printInt(i32 33)
	%84 = load i32, i32* %b
	call void @printInt(i32 %84)
	%85 = load i32, i32* %d
	call void @printInt(i32 %85)
	%86 = load i32, i32* %d
	%87 = load i32, i32* %c
	%88 = sdiv i32 %87, 90
	%89 = mul i32 %86, %88
	%90 = mul i32 4, %89
	%91 = load i32, i32* %a
	%92 = sub i32 %90, %91
	%93 = sdiv i32 %92, 8
	call void @printInt(i32 %93)
	call void @printInt(i32 98)
	ret i32 0
}
