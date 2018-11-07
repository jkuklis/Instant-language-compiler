declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%c = alloca i32
	%f = alloca i32
	%d = alloca i32
	%j = alloca i32
	%i = alloca i32
	%h = alloca i32
	%g = alloca i32
	%e = alloca i32
	%0 = add i32 49, 11
	%1 = sub i32 %0, 40
	%2 = mul i32 33, 69
	%3 = sdiv i32 %2, 34
	%4 = mul i32 41, %3
	%5 = add i32 28, 35
	%6 = mul i32 22, %5
	%7 = add i32 %4, %6
	%8 = sdiv i32 %1, %7
	%9 = add i32 65, %8
	%10 = sub i32 %9, 26
	call void @printInt(i32 %10)
	store i32 7, i32* %a
	%11 = add i32 46, 48
	call void @printInt(i32 %11)
	store i32 20, i32* %b
	%12 = load i32, i32* %a
	call void @printInt(i32 %12)
	%13 = sub i32 42, 41
	%14 = load i32, i32* %b
	%15 = mul i32 90, 20
	%16 = sdiv i32 %14, %15
	%17 = load i32, i32* %a
	%18 = sdiv i32 %16, %17
	%19 = sub i32 %18, 7
	%20 = mul i32 %19, 48
	%21 = load i32, i32* %a
	%22 = sdiv i32 %20, %21
	%23 = mul i32 %13, %22
	%24 = load i32, i32* %b
	%25 = load i32, i32* %b
	%26 = load i32, i32* %b
	%27 = sub i32 %25, %26
	%28 = add i32 %24, %27
	%29 = load i32, i32* %b
	%30 = sub i32 %28, %29
	%31 = load i32, i32* %a
	%32 = load i32, i32* %a
	%33 = load i32, i32* %a
	%34 = load i32, i32* %a
	%35 = sdiv i32 21, %34
	%36 = sdiv i32 %33, %35
	%37 = sdiv i32 %32, %36
	%38 = sub i32 %31, %37
	%39 = sub i32 50, %38
	%40 = add i32 %30, %39
	%41 = add i32 %23, %40
	%42 = load i32, i32* %b
	%43 = sdiv i32 25, %42
	%44 = add i32 %41, %43
	%45 = load i32, i32* %a
	%46 = load i32, i32* %a
	%47 = mul i32 59, 23
	%48 = load i32, i32* %b
	%49 = sub i32 20, %48
	%50 = sub i32 %47, %49
	%51 = add i32 %46, %50
	%52 = sub i32 %45, %51
	%53 = load i32, i32* %b
	%54 = sub i32 2, %53
	%55 = add i32 %52, %54
	%56 = sub i32 %44, %55
	call void @printInt(i32 %56)
	%57 = load i32, i32* %a
	%58 = load i32, i32* %a
	%59 = add i32 %57, %58
	store i32 %59, i32* %c
	%60 = load i32, i32* %c
	%61 = sdiv i32 3, %60
	%62 = load i32, i32* %a
	%63 = add i32 %61, %62
	call void @printInt(i32 %63)
	%64 = load i32, i32* %a
	%65 = sdiv i32 21, %64
	store i32 %65, i32* %d
	%66 = load i32, i32* %a
	%67 = load i32, i32* %a
	%68 = sdiv i32 %66, %67
	%69 = add i32 20, %68
	%70 = add i32 %69, 14
	call void @printInt(i32 %70)
	%71 = load i32, i32* %d
	%72 = sdiv i32 43, %71
	call void @printInt(i32 %72)
	%73 = load i32, i32* %b
	store i32 %73, i32* %e
	%74 = load i32, i32* %c
	store i32 %74, i32* %f
	call void @printInt(i32 4)
	%75 = load i32, i32* %a
	%76 = sub i32 %75, 15
	store i32 %76, i32* %g
	call void @printInt(i32 7)
	store i32 31, i32* %h
	%77 = load i32, i32* %a
	store i32 %77, i32* %i
	call void @printInt(i32 67)
	%78 = load i32, i32* %f
	store i32 %78, i32* %j
	call void @printInt(i32 10)
	%79 = load i32, i32* %c
	call void @printInt(i32 %79)
	ret i32 0
}
