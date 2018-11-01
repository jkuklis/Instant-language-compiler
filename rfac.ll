declare void @printInt(i32)
define i32 @main() {
entry: 
	%i1=call i32 @fact(i32 5)
	call void @printInt(i32 %i1)
	ret i32 0
}

define i32 @fact(i32 %n) {
entry:
	%c0 = icmp eq i32 %n, 0
	br i1 %c0, label %L0, label %L1
L0:
	ret i32 1
L1:
	%i1 = sub i32 %n, 1
	%i2 = call i32 @fact(i32 %i1)
	%i3 = mul i32 %n, %i2
	ret i32 %i3 
}