; ModuleID = 'concat.c'

define i8* @concat(i8* %s1, i8* %s2) {
  %1 = call i32 @strlen(i8* %s1)
  %2 = call i32 @strlen(i8* %s2)
  %3 = add i32 %1, 1
  %4 = add i32 %3, %2
  %5 = call i8* @malloc(i32 %4)
  %6 = call i8* @strcpy(i8* %5, i8* %s1)
  %7 = call i8* @strcat(i8* %6, i8* %s2)
  ret i8* %7
}

declare noalias i8* @malloc(i32) nounwind
declare i32 @strlen(i8* nocapture) nounwind readonly
declare i8* @strcat(i8*, i8* nocapture) nounwind
declare i8* @strcpy(i8*, i8* nocapture) nounwind
