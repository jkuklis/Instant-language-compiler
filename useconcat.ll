; ModuleID = 'useconcat.c'

@.str = private unnamed_addr constant [7 x i8] c"Hello,\00", align 1
@.str1 = private unnamed_addr constant [8 x i8] c" world\0A\00", align 1

define i32 @main() nounwind uwtable {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval
  %call = call i8* @concat(i8* getelementptr inbounds ([7 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([8 x i8]* @.str1, i32 0, i32 0))
  %call1 = call i32 @puts(i8* %call)
  ret i32 0
}

declare i32 @puts(i8*)

declare i8* @concat(i8*, i8*)
