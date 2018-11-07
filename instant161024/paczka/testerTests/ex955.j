.class public ex955
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 43
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	bipush 17
	imul
	bipush 42
	iadd
	bipush 59
	idiv
	bipush 15
	iadd
	bipush 15
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 97
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	bipush 23
	iadd
	istore_3
	bipush 68
	istore 5
	bipush 10
	istore_1
	iload_3
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	iload_1
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	bipush 24
	istore 4
	iload 5
	iload_2
	iadd
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	iload_2
	isub
	bipush 10
	imul
	bipush 12
	iload 5
	iadd
	imul
	bipush 47
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	iload 4
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	istore 8
	iconst_5
	bipush 49
	iadd
	istore_0
	iload_0
	istore 7
	bipush 12
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	bipush 14
	isub
	bipush 8
	iadd
	bipush 33
	iadd
	iload 7
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	iconst_4
	iadd
	bipush 15
	imul
	iload 8
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 51
	iload 4
	iadd
	bipush 58
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 62
	iadd
	bipush 15
	swap
	isub
	bipush 57
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	iload 7
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 22
	iload_1
	istore 21
	iconst_5
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	iload 4
	isub
	bipush 75
	iadd
	bipush 11
	swap
	isub
	iload 6
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 32
	iadd
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 38
	iload 6
	iadd
	iload 10
	iload 22
	iadd
	imul
	iconst_5
	bipush 24
	idiv
	isub
	bipush 33
	idiv
	iload 5
	isub
	bipush 61
	imul
	bipush 8
	iadd
	bipush 8
	imul
	istore 25
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 21
	iload 10
	imul
	iload 4
	imul
	iconst_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	bipush 19
	idiv
	bipush 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	iload 5
	isub
	iload 6
	iload 5
	imul
	imul
	iload_0
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	bipush 29
	idiv
	bipush 11
	swap
	isub
	bipush 51
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	bipush 69
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload 9
	bipush 6
	imul
	iload_0
	imul
	iload 4
	isub
	istore 16
	bipush 14
	istore 20
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 56
	invokevirtual java/io/PrintStream/println(I)V
	iload 14
	iload 9
	iadd
	iload_2
	swap
	isub
	istore 19
	iload 16
	bipush 16
	isub
	bipush 32
	idiv
	istore 18
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 25
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	bipush 39
	isub
	istore 24
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 19
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 24
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 66
	bipush 27
	imul
	bipush 16
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	bipush 9
	iadd
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	bipush 24
	iadd
	bipush 14
	isub
	iload 15
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	bipush 18
	isub
	iconst_4
	imul
	bipush 7
	swap
	idiv
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	bipush 15
	iadd
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	iload 19
	iadd
	bipush 8
	swap
	isub
	bipush 64
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	iload_1
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 19
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	iload_0
	isub
	iload_1
	bipush 35
	iadd
	iload_3
	imul
	iload 17
	swap
	isub
	iconst_5
	iadd
	bipush 39
	imul
	bipush 114
	swap
	idiv
	bipush 38
	imul
	iload 16
	isub
	iadd
	iconst_5
	iadd
	istore 32
	bipush 44
	istore 31
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 31
	invokevirtual java/io/PrintStream/println(I)V
	bipush 60
	istore 23
	bipush 41
	istore 30
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 23
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 81
	bipush 41
	idiv
	iload 5
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	iload 11
	istore 42
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 30
	iload 9
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload 21
	istore 41
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	istore 29
	bipush 16
	istore 40
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	iload_2
	imul
	iload 14
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	iload 23
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	iload 18
	isub
	iload 11
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	iload 15
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 20
	invokevirtual java/io/PrintStream/println(I)V
	bipush 48
	bipush 6
	idiv
	istore 28
	bipush 41
	bipush 50
	isub
	istore 39
	iload 7
	istore 27
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	iload 29
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 28
	iconst_4
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	bipush 71
	isub
	iload_1
	iadd
	bipush 7
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 58
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	istore 38
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	iload 20
	idiv
	bipush 102
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	bipush 21
	isub
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 71
	invokevirtual java/io/PrintStream/println(I)V
	bipush 62
	bipush 45
	idiv
	istore 37
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	iload 11
	istore 36
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	iload 12
	iadd
	bipush 12
	imul
	iload 27
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 71
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 26
	iload 6
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 35
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 22
	invokevirtual java/io/PrintStream/println(I)V
	bipush 31
	iload_1
	imul
	istore 34
	iload 8
	istore 33
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
