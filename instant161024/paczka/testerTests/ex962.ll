declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%f = alloca i32
	%d = alloca i32
	%e = alloca i32
	%c = alloca i32
	%j = alloca i32
	%i = alloca i32
	%k = alloca i32
	%h = alloca i32
	%g = alloca i32
	%0 = sub i32 30, 15
	%1 = mul i32 %0, 32
	%2 = mul i32 %1, 7
	%3 = add i32 28, %2
	%4 = sdiv i32 55, %3
	%5 = mul i32 55, 43
	%6 = sub i32 %4, %5
	%7 = add i32 29, %6
	call void @printInt(i32 %7)
	call void @printInt(i32 41)
	call void @printInt(i32 40)
	call void @printInt(i32 37)
	%8 = sdiv i32 8, 4
	%9 = add i32 53, %8
	store i32 %9, i32* %a
	%10 = load i32, i32* %a
	%11 = load i32, i32* %a
	%12 = sub i32 %11, 0
	%13 = add i32 %10, %12
	call void @printInt(i32 %13)
	call void @printInt(i32 85)
	call void @printInt(i32 15)
	%14 = load i32, i32* %a
	call void @printInt(i32 %14)
	%15 = load i32, i32* %a
	store i32 %15, i32* %b
	%16 = load i32, i32* %a
	%17 = mul i32 43, %16
	call void @printInt(i32 %17)
	call void @printInt(i32 6)
	%18 = sub i32 64, 25
	store i32 %18, i32* %c
	%19 = load i32, i32* %c
	store i32 %19, i32* %d
	%20 = load i32, i32* %a
	%21 = load i32, i32* %b
	%22 = sdiv i32 %20, %21
	%23 = mul i32 21, %22
	call void @printInt(i32 %23)
	%24 = load i32, i32* %d
	%25 = sub i32 %24, 2
	%26 = sub i32 24, %25
	%27 = mul i32 27, %26
	%28 = mul i32 %27, 31
	store i32 %28, i32* %e
	%29 = load i32, i32* %c
	call void @printInt(i32 %29)
	%30 = sdiv i32 39, 10
	%31 = sdiv i32 24, %30
	call void @printInt(i32 %31)
	%32 = load i32, i32* %c
	%33 = sdiv i32 %32, 43
	call void @printInt(i32 %33)
	%34 = load i32, i32* %b
	call void @printInt(i32 %34)
	%35 = load i32, i32* %a
	store i32 %35, i32* %f
	%36 = load i32, i32* %f
	call void @printInt(i32 %36)
	%37 = load i32, i32* %b
	%38 = sub i32 %37, 33
	%39 = mul i32 85, %38
	%40 = add i32 %39, 55
	call void @printInt(i32 %40)
	%41 = add i32 5, 17
	%42 = load i32, i32* %f
	%43 = sub i32 %41, %42
	%44 = load i32, i32* %a
	%45 = load i32, i32* %d
	%46 = add i32 %44, %45
	%47 = mul i32 %43, %46
	call void @printInt(i32 %47)
	%48 = add i32 32, 20
	call void @printInt(i32 %48)
	store i32 9, i32* %g
	%49 = load i32, i32* %e
	call void @printInt(i32 %49)
	%50 = load i32, i32* %f
	%51 = mul i32 %50, 45
	call void @printInt(i32 %51)
	%52 = load i32, i32* %d
	call void @printInt(i32 %52)
	%53 = load i32, i32* %b
	call void @printInt(i32 %53)
	%54 = load i32, i32* %e
	store i32 %54, i32* %h
	%55 = sdiv i32 27, 53
	store i32 %55, i32* %i
	%56 = mul i32 42, 31
	%57 = load i32, i32* %f
	%58 = add i32 %56, %57
	%59 = load i32, i32* %b
	%60 = sdiv i32 %58, %59
	%61 = sdiv i32 10, %60
	call void @printInt(i32 %61)
	%62 = load i32, i32* %i
	%63 = load i32, i32* %f
	%64 = mul i32 %62, %63
	%65 = load i32, i32* %d
	%66 = sdiv i32 %64, %65
	%67 = sub i32 %66, 65
	%68 = load i32, i32* %e
	%69 = sub i32 %67, %68
	call void @printInt(i32 %69)
	store i32 15, i32* %j
	call void @printInt(i32 6)
	%70 = add i32 65, 51
	call void @printInt(i32 %70)
	%71 = sdiv i32 101, 16
	call void @printInt(i32 %71)
	%72 = load i32, i32* %j
	store i32 %72, i32* %k
	call void @printInt(i32 63)
	%73 = load i32, i32* %b
	call void @printInt(i32 %73)
	ret i32 0
}
