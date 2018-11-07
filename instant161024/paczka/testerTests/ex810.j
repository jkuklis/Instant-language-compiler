.class public ex810
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 29
.limit stack 4
	bipush 45
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	iload_0
	bipush 10
	imul
	iadd
	iconst_2
	iload_0
	isub
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 24
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 19
	iload_2
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	bipush 50
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 25
	isub
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 35
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 11
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 68
	bipush 24
	iadd
	iload_2
	imul
	iload 6
	isub
	iload_1
	idiv
	iload_2
	iadd
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 21
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 55
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	iload_1
	isub
	bipush 6
	imul
	iload_2
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	bipush 19
	iadd
	iload 5
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 30
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iload_1
	idiv
	iload 6
	imul
	bipush 47
	idiv
	iload_1
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore_3
	iload 6
	bipush 22
	isub
	bipush 48
	imul
	bipush 17
	iload 4
	imul
	isub
	bipush 19
	iload 4
	iadd
	bipush 94
	swap
	idiv
	imul
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	iload 16
	isub
	bipush 72
	iload_3
	isub
	iload 5
	isub
	bipush 6
	idiv
	iload 4
	swap
	idiv
	iadd
	iload_0
	iadd
	bipush 32
	bipush 29
	imul
	iadd
	iload 6
	iload 5
	idiv
	bipush 46
	imul
	bipush 63
	idiv
	iadd
	iload_3
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 16
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	iload_0
	iadd
	iconst_1
	bipush 13
	imul
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	bipush 96
	isub
	bipush 71
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	bipush 25
	iadd
	iload 14
	idiv
	iload 4
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 73
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	istore 19
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 65
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	bipush 34
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	iload 9
	istore 12
	bipush 9
	istore 25
	bipush 56
	bipush 21
	iadd
	iload 6
	imul
	bipush 48
	iload_0
	isub
	iload 4
	imul
	idiv
	bipush 38
	iconst_2
	iadd
	imul
	iload 14
	bipush 106
	iadd
	bipush 58
	imul
	isub
	bipush 31
	iadd
	istore 24
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 19
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	iload_3
	isub
	bipush 39
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 26
	bipush 12
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 21
	istore 7
	bipush 16
	istore 18
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	iload 7
	istore 23
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	iload 12
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	bipush 59
	iload_3
	iadd
	iconst_3
	swap
	idiv
	istore 22
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	bipush 9
	iadd
	bipush 15
	iadd
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	bipush 25
	isub
	bipush 64
	iadd
	bipush 13
	iload 15
	iadd
	iload 7
	isub
	iload 8
	idiv
	imul
	iload 8
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	bipush 54
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 21
	bipush 31
	iload 10
	iadd
	istore 20
	iload_0
	istore 28
	bipush 72
	iload 11
	isub
	istore 27
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	iload 12
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	bipush 36
	iadd
	bipush 20
	swap
	idiv
	bipush 54
	bipush 17
	iadd
	bipush 25
	swap
	isub
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 31
	imul
	iconst_2
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload 17
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
