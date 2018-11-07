.class public ex970
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 27
.limit stack 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 19
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	bipush 6
	bipush 46
	imul
	bipush 46
	bipush 47
	imul
	bipush 16
	iadd
	bipush 53
	iadd
	bipush 40
	swap
	isub
	bipush 47
	isub
	imul
	bipush 56
	bipush 34
	isub
	bipush 20
	iadd
	bipush 47
	imul
	bipush 54
	iadd
	swap
	idiv
	bipush 23
	bipush 38
	idiv
	isub
	bipush 32
	isub
	bipush 18
	bipush 78
	imul
	bipush 23
	idiv
	bipush 18
	isub
	bipush 85
	bipush 47
	imul
	bipush 10
	iadd
	bipush 49
	imul
	imul
	iconst_5
	imul
	bipush 27
	imul
	isub
	bipush 28
	imul
	bipush 6
	bipush 53
	idiv
	bipush 18
	isub
	isub
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 70
	idiv
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	bipush 19
	iload_0
	iadd
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 26
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	bipush 65
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	bipush 19
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 38
	istore 20
	bipush 28
	istore_3
	iload 4
	istore 19
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	iload 8
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 24
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	iconst_2
	isub
	iload_2
	imul
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 25
	iload_1
	imul
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 81
	iload_3
	iadd
	bipush 26
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	bipush 91
	idiv
	iload_1
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	bipush 69
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	iload 7
	iadd
	istore 6
	bipush 37
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	bipush 74
	iadd
	iload 20
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	bipush 40
	idiv
	iload 10
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload 7
	istore 9
	bipush 22
	iload 6
	imul
	iload_1
	imul
	bipush 22
	isub
	bipush 17
	imul
	iload_0
	iadd
	iload 6
	imul
	iload 5
	iload 6
	imul
	bipush 92
	swap
	isub
	iadd
	iconst_4
	isub
	iload 9
	iadd
	iload 5
	swap
	idiv
	bipush 44
	bipush 56
	isub
	swap
	isub
	bipush 13
	imul
	bipush 13
	swap
	isub
	bipush 63
	idiv
	iload 9
	iadd
	istore 18
	iload 5
	istore 25
	iload 8
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 43
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	iload 18
	istore 12
	iload 10
	bipush 48
	idiv
	bipush 91
	swap
	idiv
	bipush 39
	imul
	iload 12
	iload 10
	iadd
	iload_0
	swap
	idiv
	iload 7
	swap
	isub
	iadd
	bipush 24
	iload_3
	imul
	isub
	istore 24
	bipush 12
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	istore 15
	iload 7
	bipush 43
	idiv
	iload 9
	iload_2
	idiv
	iload 16
	iadd
	bipush 22
	swap
	isub
	iload_2
	swap
	isub
	iload 12
	iadd
	isub
	bipush 8
	isub
	istore 23
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 17
	invokevirtual java/io/PrintStream/println(I)V
	bipush 6
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 19
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	bipush 37
	istore 22
	iload 19
	istore 21
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	bipush 43
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 12
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 77
	bipush 46
	iadd
	bipush 67
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 63
	bipush 36
	isub
	iload_2
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 47
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	iload_1
	idiv
	iload 13
	imul
	bipush 22
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
