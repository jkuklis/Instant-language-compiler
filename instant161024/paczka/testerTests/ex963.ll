declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	%i = alloca i32
	%g = alloca i32
	%c = alloca i32
	%e = alloca i32
	%h = alloca i32
	%d = alloca i32
	%l = alloca i32
	%j = alloca i32
	%f = alloca i32
	%k = alloca i32
	call void @printInt(i32 7)
	store i32 26, i32* %a
	%0 = load i32, i32* %a
	store i32 %0, i32* %b
	%1 = load i32, i32* %b
	%2 = mul i32 7, %1
	%3 = load i32, i32* %a
	%4 = load i32, i32* %a
	%5 = sub i32 36, %4
	%6 = mul i32 14, %5
	%7 = sub i32 %3, %6
	%8 = add i32 %7, 25
	%9 = sub i32 %2, %8
	%10 = mul i32 5, 21
	%11 = sub i32 41, %10
	%12 = add i32 %9, %11
	call void @printInt(i32 %12)
	%13 = load i32, i32* %b
	call void @printInt(i32 %13)
	%14 = load i32, i32* %a
	%15 = load i32, i32* %b
	%16 = sdiv i32 %14, %15
	%17 = sub i32 %16, 33
	store i32 %17, i32* %c
	%18 = load i32, i32* %a
	store i32 %18, i32* %d
	%19 = load i32, i32* %b
	call void @printInt(i32 %19)
	%20 = load i32, i32* %a
	store i32 %20, i32* %e
	store i32 45, i32* %f
	%21 = load i32, i32* %b
	%22 = add i32 %21, 37
	%23 = mul i32 %22, 1
	call void @printInt(i32 %23)
	call void @printInt(i32 5)
	%24 = load i32, i32* %c
	%25 = sdiv i32 58, %24
	call void @printInt(i32 %25)
	call void @printInt(i32 62)
	%26 = load i32, i32* %c
	%27 = sub i32 %26, 49
	%28 = load i32, i32* %d
	%29 = sub i32 %27, %28
	%30 = sub i32 36, %29
	%31 = sub i32 %30, 29
	store i32 %31, i32* %g
	%32 = load i32, i32* %e
	call void @printInt(i32 %32)
	call void @printInt(i32 27)
	call void @printInt(i32 8)
	%33 = load i32, i32* %c
	call void @printInt(i32 %33)
	call void @printInt(i32 20)
	call void @printInt(i32 73)
	store i32 25, i32* %h
	call void @printInt(i32 55)
	%34 = load i32, i32* %a
	call void @printInt(i32 %34)
	%35 = load i32, i32* %h
	call void @printInt(i32 %35)
	call void @printInt(i32 8)
	%36 = load i32, i32* %e
	%37 = add i32 %36, 17
	%38 = load i32, i32* %a
	%39 = mul i32 %38, 15
	%40 = sub i32 11, 5
	%41 = load i32, i32* %g
	%42 = add i32 %40, %41
	%43 = add i32 %39, %42
	%44 = mul i32 %43, 3
	%45 = mul i32 %37, %44
	call void @printInt(i32 %45)
	%46 = load i32, i32* %g
	%47 = load i32, i32* %h
	%48 = sdiv i32 %46, %47
	call void @printInt(i32 %48)
	%49 = load i32, i32* %b
	call void @printInt(i32 %49)
	call void @printInt(i32 8)
	%50 = load i32, i32* %g
	call void @printInt(i32 %50)
	call void @printInt(i32 81)
	%51 = load i32, i32* %d
	call void @printInt(i32 %51)
	call void @printInt(i32 18)
	%52 = load i32, i32* %c
	%53 = mul i32 10, %52
	%54 = sdiv i32 54, %53
	store i32 %54, i32* %i
	%55 = load i32, i32* %b
	call void @printInt(i32 %55)
	call void @printInt(i32 75)
	%56 = load i32, i32* %i
	call void @printInt(i32 %56)
	store i32 10, i32* %j
	call void @printInt(i32 29)
	call void @printInt(i32 75)
	%57 = load i32, i32* %i
	call void @printInt(i32 %57)
	%58 = load i32, i32* %i
	store i32 %58, i32* %k
	call void @printInt(i32 6)
	%59 = load i32, i32* %a
	%60 = load i32, i32* %a
	%61 = sub i32 %60, 59
	%62 = load i32, i32* %a
	%63 = load i32, i32* %b
	%64 = sub i32 %62, %63
	%65 = sub i32 %61, %64
	%66 = sdiv i32 %59, %65
	call void @printInt(i32 %66)
	%67 = load i32, i32* %e
	%68 = load i32, i32* %g
	%69 = sdiv i32 %67, %68
	%70 = load i32, i32* %i
	%71 = sub i32 %69, %70
	call void @printInt(i32 %71)
	call void @printInt(i32 14)
	%72 = load i32, i32* %j
	%73 = sub i32 %72, 2
	call void @printInt(i32 %73)
	%74 = load i32, i32* %f
	store i32 %74, i32* %l
	%75 = mul i32 23, 68
	call void @printInt(i32 %75)
	call void @printInt(i32 38)
	call void @printInt(i32 3)
	%76 = load i32, i32* %l
	%77 = mul i32 69, %76
	call void @printInt(i32 %77)
	ret i32 0
}
