.class public ex958
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 15
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	bipush 25
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 56
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 51
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 52
	invokevirtual java/io/PrintStream/println(I)V
	bipush 27
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iconst_4
	imul
	iload_0
	idiv
	iload_0
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iload_1
	isub
	istore_3
	iload_0
	bipush 18
	imul
	iload_1
	swap
	idiv
	bipush 7
	imul
	iload_0
	imul
	istore 4
	iload_1
	bipush 16
	imul
	iload_3
	imul
	bipush 27
	isub
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	iload 4
	iadd
	bipush 34
	swap
	isub
	iload_3
	swap
	isub
	bipush 45
	imul
	iload_1
	swap
	idiv
	bipush 72
	imul
	bipush 67
	swap
	isub
	iload_1
	swap
	isub
	bipush 6
	iload_3
	iadd
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	bipush 87
	istore 7
	iconst_4
	iload 4
	isub
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	bipush 43
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 7
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	bipush 55
	iadd
	iload_3
	iadd
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	bipush 44
	idiv
	iload 6
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	invokevirtual java/io/PrintStream/println(I)V
	bipush 28
	istore 5
	iload 8
	bipush 13
	isub
	iload_2
	swap
	isub
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	iload_2
	imul
	bipush 57
	iload 8
	iadd
	isub
	iload_0
	imul
	iload 4
	bipush 41
	isub
	imul
	iload_1
	swap
	isub
	iload_2
	isub
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	bipush 44
	isub
	bipush 48
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	bipush 9
	isub
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 57
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	istore 11
	iload_1
	bipush 12
	iadd
	bipush 29
	iload 4
	iadd
	iload_0
	isub
	bipush 15
	imul
	idiv
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 75
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
