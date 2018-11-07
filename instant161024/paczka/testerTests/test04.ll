declare void @printInt(i32)
define i32 @main() {
entry:
	%a = alloca i32
	%b = alloca i32
	store i32 1, i32* %a
	store i32 2, i32* %b
	%0 = load i32, i32* %b
	%1 = load i32, i32* %a
	%2 = load i32, i32* %a
	%3 = load i32, i32* %a
	%4 = load i32, i32* %a
	%5 = load i32, i32* %a
	%6 = load i32, i32* %a
	%7 = load i32, i32* %a
	%8 = load i32, i32* %a
	%9 = load i32, i32* %a
	%10 = load i32, i32* %a
	%11 = load i32, i32* %b
	%12 = load i32, i32* %a
	%13 = load i32, i32* %a
	%14 = load i32, i32* %a
	%15 = load i32, i32* %a
	%16 = load i32, i32* %a
	%17 = load i32, i32* %a
	%18 = load i32, i32* %a
	%19 = load i32, i32* %a
	%20 = load i32, i32* %a
	%21 = load i32, i32* %a
	%22 = load i32, i32* %a
	%23 = load i32, i32* %a
	%24 = load i32, i32* %a
	%25 = load i32, i32* %a
	%26 = load i32, i32* %a
	%27 = load i32, i32* %a
	%28 = load i32, i32* %a
	%29 = load i32, i32* %a
	%30 = load i32, i32* %a
	%31 = load i32, i32* %b
	%32 = add i32 %30, %31
	%33 = add i32 1, %32
	%34 = add i32 %29, %33
	%35 = add i32 %28, %34
	%36 = add i32 1, %35
	%37 = add i32 %27, %36
	%38 = add i32 %26, %37
	%39 = add i32 1, %38
	%40 = add i32 %25, %39
	%41 = add i32 %24, %40
	%42 = add i32 %23, %41
	%43 = add i32 %22, %42
	%44 = add i32 1, %43
	%45 = add i32 %21, %44
	%46 = add i32 %20, %45
	%47 = add i32 %19, %46
	%48 = add i32 %18, %47
	%49 = add i32 %17, %48
	%50 = add i32 %16, %49
	%51 = add i32 %15, %50
	%52 = add i32 %14, %51
	%53 = add i32 %13, %52
	%54 = add i32 %12, %53
	%55 = add i32 1, %54
	%56 = add i32 %11, %55
	%57 = add i32 %10, %56
	%58 = add i32 %9, %57
	%59 = add i32 %8, %58
	%60 = add i32 1, %59
	%61 = add i32 %7, %60
	%62 = add i32 %6, %61
	%63 = add i32 %5, %62
	%64 = add i32 %4, %63
	%65 = add i32 %3, %64
	%66 = add i32 1, %65
	%67 = add i32 %2, %66
	%68 = add i32 %1, %67
	%69 = add i32 %0, %68
	call void @printInt(i32 %69)
	ret i32 0
}
