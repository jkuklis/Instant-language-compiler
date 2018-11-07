declare void @printInt(i32)
define i32 @main() {
entry:
	%h = alloca i32
	%g = alloca i32
	%f = alloca i32
	%e = alloca i32
	%d = alloca i32
	%c = alloca i32
	%b = alloca i32
	%a = alloca i32
	%n = alloca i32
	%m = alloca i32
	%l = alloca i32
	%k = alloca i32
	%j = alloca i32
	%i = alloca i32
	store i32 0, i32* %a
	store i32 1, i32* %b
	store i32 0, i32* %c
	store i32 1, i32* %d
	store i32 0, i32* %e
	store i32 1, i32* %f
	store i32 0, i32* %g
	store i32 1, i32* %h
	%0 = load i32, i32* %a
	%1 = load i32, i32* %b
	%2 = mul i32 %0, %1
	%3 = load i32, i32* %c
	%4 = load i32, i32* %d
	%5 = mul i32 %3, %4
	%6 = load i32, i32* %e
	%7 = load i32, i32* %f
	%8 = load i32, i32* %g
	%9 = load i32, i32* %h
	%10 = add i32 %8, %9
	%11 = add i32 %7, %10
	%12 = add i32 %6, %11
	%13 = add i32 %5, %12
	%14 = add i32 %2, %13
	call void @printInt(i32 %14)
	store i32 1, i32* %a
	store i32 2, i32* %b
	store i32 1, i32* %c
	store i32 2, i32* %d
	store i32 1, i32* %e
	store i32 2, i32* %f
	store i32 1, i32* %g
	store i32 2, i32* %h
	store i32 1, i32* %i
	store i32 2, i32* %j
	store i32 1, i32* %k
	store i32 2, i32* %l
	store i32 1, i32* %m
	store i32 2, i32* %n
	%15 = load i32, i32* %a
	%16 = mul i32 2, %15
	%17 = load i32, i32* %b
	%18 = sdiv i32 %17, 2
	%19 = load i32, i32* %c
	%20 = load i32, i32* %d
	%21 = load i32, i32* %e
	%22 = load i32, i32* %f
	%23 = load i32, i32* %g
	%24 = load i32, i32* %h
	%25 = load i32, i32* %i
	%26 = load i32, i32* %j
	%27 = sdiv i32 %26, 2
	%28 = load i32, i32* %k
	%29 = load i32, i32* %l
	%30 = load i32, i32* %m
	%31 = load i32, i32* %n
	%32 = add i32 %30, %31
	%33 = add i32 %29, %32
	%34 = add i32 %28, %33
	%35 = add i32 %27, %34
	%36 = add i32 %25, %35
	%37 = add i32 %24, %36
	%38 = add i32 %23, %37
	%39 = add i32 %22, %38
	%40 = add i32 %21, %39
	%41 = add i32 %20, %40
	%42 = add i32 %19, %41
	%43 = add i32 %18, %42
	%44 = add i32 %16, %43
	%45 = sdiv i32 %44, 10
	call void @printInt(i32 %45)
	ret i32 0
}
