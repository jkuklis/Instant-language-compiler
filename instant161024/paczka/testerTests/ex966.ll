declare void @printInt(i32)
define i32 @main() {
entry:
	%e = alloca i32
	%h = alloca i32
	%c = alloca i32
	%b = alloca i32
	%d = alloca i32
	%a = alloca i32
	%j = alloca i32
	%g = alloca i32
	%i = alloca i32
	%f = alloca i32
	%l = alloca i32
	%k = alloca i32
	%0 = add i32 24, 56
	%1 = sub i32 %0, 13
	call void @printInt(i32 %1)
	%2 = mul i32 25, 5
	%3 = mul i32 %2, 28
	%4 = mul i32 40, %3
	%5 = add i32 12, %4
	%6 = add i32 32, %5
	%7 = add i32 %6, 58
	store i32 %7, i32* %a
	%8 = sdiv i32 46, 15
	store i32 %8, i32* %b
	%9 = load i32, i32* %a
	%10 = sdiv i32 %9, 16
	%11 = sdiv i32 77, %10
	store i32 %11, i32* %c
	%12 = load i32, i32* %b
	call void @printInt(i32 %12)
	%13 = load i32, i32* %a
	%14 = load i32, i32* %c
	%15 = sub i32 %13, %14
	call void @printInt(i32 %15)
	%16 = sdiv i32 17, 46
	call void @printInt(i32 %16)
	call void @printInt(i32 12)
	store i32 19, i32* %d
	%17 = load i32, i32* %c
	call void @printInt(i32 %17)
	%18 = load i32, i32* %b
	%19 = sdiv i32 %18, 40
	call void @printInt(i32 %19)
	store i32 0, i32* %e
	call void @printInt(i32 72)
	%20 = load i32, i32* %e
	call void @printInt(i32 %20)
	%21 = load i32, i32* %d
	call void @printInt(i32 %21)
	%22 = load i32, i32* %e
	%23 = load i32, i32* %e
	%24 = mul i32 7, %23
	%25 = mul i32 %24, 34
	%26 = mul i32 %22, %25
	%27 = sub i32 25, %26
	%28 = load i32, i32* %c
	%29 = mul i32 %27, %28
	call void @printInt(i32 %29)
	store i32 35, i32* %f
	%30 = load i32, i32* %c
	call void @printInt(i32 %30)
	%31 = load i32, i32* %b
	call void @printInt(i32 %31)
	call void @printInt(i32 20)
	%32 = load i32, i32* %d
	call void @printInt(i32 %32)
	%33 = sdiv i32 28, 40
	call void @printInt(i32 %33)
	%34 = load i32, i32* %c
	store i32 %34, i32* %g
	%35 = load i32, i32* %e
	call void @printInt(i32 %35)
	%36 = load i32, i32* %e
	store i32 %36, i32* %h
	%37 = load i32, i32* %h
	call void @printInt(i32 %37)
	%38 = load i32, i32* %e
	call void @printInt(i32 %38)
	%39 = load i32, i32* %e
	call void @printInt(i32 %39)
	%40 = load i32, i32* %c
	%41 = add i32 %40, 12
	%42 = sdiv i32 99, 22
	%43 = add i32 %41, %42
	call void @printInt(i32 %43)
	call void @printInt(i32 4)
	%44 = load i32, i32* %g
	%45 = load i32, i32* %h
	%46 = sub i32 %45, 41
	%47 = load i32, i32* %a
	%48 = load i32, i32* %h
	%49 = sub i32 %47, %48
	%50 = add i32 %49, 14
	%51 = sdiv i32 %46, %50
	%52 = load i32, i32* %f
	%53 = load i32, i32* %b
	%54 = mul i32 %52, %53
	%55 = sub i32 %51, %54
	%56 = mul i32 %44, %55
	%57 = sub i32 53, %56
	call void @printInt(i32 %57)
	store i32 61, i32* %i
	call void @printInt(i32 41)
	%58 = load i32, i32* %i
	%59 = mul i32 62, %58
	%60 = sdiv i32 1, %59
	call void @printInt(i32 %60)
	%61 = load i32, i32* %b
	call void @printInt(i32 %61)
	%62 = load i32, i32* %b
	call void @printInt(i32 %62)
	%63 = load i32, i32* %e
	call void @printInt(i32 %63)
	%64 = load i32, i32* %d
	store i32 %64, i32* %j
	%65 = load i32, i32* %j
	call void @printInt(i32 %65)
	call void @printInt(i32 5)
	call void @printInt(i32 79)
	%66 = sdiv i32 82, 13
	%67 = load i32, i32* %h
	%68 = mul i32 %66, %67
	call void @printInt(i32 %68)
	%69 = load i32, i32* %c
	%70 = sub i32 21, 58
	%71 = sub i32 20, %70
	%72 = mul i32 %69, %71
	%73 = load i32, i32* %j
	%74 = sub i32 %72, %73
	store i32 %74, i32* %k
	%75 = load i32, i32* %b
	call void @printInt(i32 %75)
	%76 = load i32, i32* %h
	%77 = load i32, i32* %g
	%78 = load i32, i32* %h
	%79 = add i32 %77, %78
	%80 = sub i32 %76, %79
	store i32 %80, i32* %l
	call void @printInt(i32 83)
	%81 = load i32, i32* %h
	call void @printInt(i32 %81)
	ret i32 0
}
