; ModuleID = 'sumto.c'

define i32 @sumto(i32 %n) nounwind readnone ssp {
entry:
  %0 = icmp sgt i32 %n, 0
  br i1 %0, label %bb.nph, label %"5"

bb.nph:                                           ; preds = %entry
  %tmp4 = add i32 %n, -2
  %tmp2 = add i32 %n, -1
  %tmp5 = zext i32 %tmp4 to i33
  %tmp3 = zext i32 %tmp2 to i33
  %tmp6 = mul i33 %tmp3, %tmp5
  %tmp7 = lshr i33 %tmp6, 1
  %tmp8 = trunc i33 %tmp7 to i32
  %tmp = shl i32 %n, 1
  %tmp9 = add i32 %tmp, %tmp8
  %tmp10 = add i32 %tmp9, -1
  ret i32 %tmp10

"5":                                              ; preds = %entry
  ret i32 0
}
