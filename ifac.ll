declare void @printInt(i32)
define i32 @main() {
entry: 
	%i1=call i32 @fact(i32 5)
	call void @printInt(i32 %i1)
	ret i32 0
}

; r = 1
; i = n
; while (i > 1):
;   r = r * i
;   i = i -1
;;

; L1:
; i.1 = phi entry n, L2 i.2
; r.1 = phi 1,r.2
; if i.1 <= 1 goto L3
; L2: r.2 = mul r.1, i.1
; i.2 = sub i.1, 1
; goto L1
; L3: return r.1
define i32 @fact(i32 %n) {
entry: br label %L1 
L1: 
	%i.1 = phi i32 [%n, %entry], [%i.2, %L2]
	%r.1 = phi i32 [1, %entry], [%r.2, %L2]
	%c0 = icmp sle i32 %i.1, 1
	br i1 %c0, label %L3, label %L2
L2:
	%r.2 = mul i32 %r.1, %i.1
	%i.2 = sub i32 %i.1, 1
	br label %L1
L3:
	ret i32 %r.1
}