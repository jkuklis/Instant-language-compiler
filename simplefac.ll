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
; return r
;;

define i32 @fact(i32 %n) {
entry: 
; local variables:
        %loc_r = alloca i32
	%loc_i = alloca i32
; r = 1
	store i32 1, i32* %loc_r
; i = n
        store i32 %n, i32* %loc_i
	br label %L1
; while i > 1:
L1:
 	%tmp_i1 = load i32* %loc_i
	%c0 = icmp sle i32 %tmp_i1, 1
	br i1 %c0, label %L3, label %L2
; loop body
L2:
; r = r * i
        %tmp_i2 = load i32* %loc_r
	%tmp_i3 = load i32* %loc_i
	%tmp_i4 = mul i32 %tmp_i2, %tmp_i3
	store i32 %tmp_i4, i32* %loc_r 
; i = i-1
	%tmp_i5 = load i32* %loc_i
	%tmp_i6 = sub i32 %tmp_i5, 1
	store i32 %tmp_i6, i32* %loc_i
	br label %L1
L3:
	%tmp_i8 = load i32* %loc_r
	ret i32 %tmp_i8
}
