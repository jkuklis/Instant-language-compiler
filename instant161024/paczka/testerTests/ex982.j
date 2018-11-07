.class public ex982
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 23
.limit stack 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	bipush 6
	bipush 56
	imul
	bipush 33
	isub
	bipush 63
	bipush 11
	iadd
	bipush 33
	iadd
	imul
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 37
	imul
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	iload_1
	isub
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 8
	iload_1
	imul
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 65
	iload_1
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 113
	iload_2
	iadd
	iconst_2
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	iconst_3
	imul
	bipush 48
	swap
	idiv
	iload_1
	bipush 38
	idiv
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_0
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 29
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 62
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	bipush 15
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 43
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 90
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 6
	bipush 9
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	bipush 17
	imul
	bipush 15
	swap
	isub
	iload_0
	isub
	iload_2
	iadd
	iload_1
	imul
	iload_0
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 45
	invokevirtual java/io/PrintStream/println(I)V
	bipush 21
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	iload 8
	iadd
	bipush 46
	isub
	iload 6
	idiv
	iload 8
	iadd
	bipush 13
	iload 7
	iadd
	iload_0
	imul
	imul
	iload 6
	bipush 29
	idiv
	imul
	iload 8
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 6
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 55
	iload 5
	isub
	iload 4
	isub
	iload_3
	iload_1
	isub
	idiv
	bipush 66
	imul
	bipush 10
	idiv
	bipush 27
	iload_3
	imul
	iconst_2
	swap
	isub
	iadd
	iload_0
	imul
	bipush 25
	bipush 59
	isub
	iadd
	iload 5
	isub
	bipush 10
	iconst_3
	imul
	iload 6
	imul
	bipush 7
	iadd
	iload 5
	isub
	bipush 53
	bipush 27
	imul
	bipush 60
	idiv
	idiv
	bipush 13
	bipush 7
	isub
	iadd
	iload 7
	iadd
	iload 4
	iload_2
	isub
	swap
	isub
	iadd
	iload_3
	bipush 37
	imul
	swap
	idiv
	bipush 95
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	istore 11
	bipush 66
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	bipush 52
	iadd
	bipush 35
	iadd
	bipush 17
	idiv
	iconst_3
	iload 6
	iadd
	bipush 6
	isub
	iadd
	bipush 59
	iadd
	iload 11
	bipush 38
	idiv
	iload 11
	iadd
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	bipush 45
	istore 22
	iload_0
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	iload 9
	idiv
	bipush 42
	swap
	isub
	bipush 28
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	istore 10
	iload 8
	iload 9
	imul
	iload_2
	idiv
	bipush 69
	iadd
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 69
	iadd
	iload 7
	iadd
	iconst_3
	idiv
	bipush 69
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	bipush 29
	imul
	iload 5
	swap
	idiv
	iload 10
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	bipush 36
	imul
	iload 11
	iadd
	iload 10
	idiv
	bipush 47
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	bipush 6
	bipush 29
	imul
	iload 7
	imul
	iload_2
	swap
	isub
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	bipush 52
	isub
	iconst_5
	bipush 8
	imul
	bipush 35
	swap
	isub
	bipush 59
	imul
	iload 4
	imul
	isub
	iload 14
	iload_0
	idiv
	imul
	iload 4
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	bipush 21
	isub
	iload 11
	bipush 72
	imul
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	iload 13
	istore 21
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 72
	bipush 46
	isub
	iload 7
	iload_3
	idiv
	imul
	iload_3
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	iload 12
	idiv
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore 20
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 73
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	istore 19
	iload 4
	istore 18
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	bipush 43
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	bipush 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 67
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	iconst_4
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 69
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
