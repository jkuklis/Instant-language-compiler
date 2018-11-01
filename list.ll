%struct.list = type { [16 x i8], i32, %struct.list* }

define i32 @foo(%struct.list* %a) {
  %1 = getelementptr %struct.list* %a, i32 7, i32 1
  %2 = load i32* %1
  ret i32 %2
}
