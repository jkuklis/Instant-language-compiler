@.str = private constant [7 x i8] c"Hello,\00"
@.str1 = private constant [8 x i8] c" world\0A\00"

define i32 @main() {
  %s1 = bitcast [7 x i8]* @.str to i8*
  %s2 = bitcast [8 x i8]* @.str1 to i8*
  %s3 = call i8* @concat(i8* %s1, i8* %s2)
  %_ = call i32 @puts(i8* %s3)
  ret i32 0
}

declare i32 @puts(i8*)

declare i8* @concat(i8*, i8*)
