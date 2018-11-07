.class public ex962
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 11
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	bipush 15
	isub
	bipush 32
	imul
	bipush 7
	imul
	bipush 28
	iadd
	bipush 55
	swap
	idiv
	bipush 55
	bipush 43
	imul
	isub
	bipush 29
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	invokevirtual java/io/PrintStream/println(I)V
	bipush 8
	iconst_4
	idiv
	bipush 53
	iadd
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iconst_0
	isub
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 85
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	iload_0
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	bipush 64
	bipush 25
	isub
	istore 5
	iload 5
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_1
	idiv
	bipush 21
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	iconst_2
	isub
	bipush 24
	swap
	isub
	bipush 27
	imul
	bipush 31
	imul
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	bipush 10
	idiv
	bipush 24
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	bipush 43
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 33
	isub
	bipush 85
	imul
	bipush 55
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	bipush 17
	iadd
	iload_2
	isub
	iload_0
	iload_3
	iadd
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	bipush 20
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 9
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 45
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore 9
	bipush 27
	bipush 53
	idiv
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	bipush 31
	imul
	iload_2
	iadd
	iload_1
	idiv
	bipush 10
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	iload_2
	imul
	iload_3
	idiv
	bipush 65
	isub
	iload 4
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 15
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 65
	bipush 51
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 101
	bipush 16
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 6
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 63
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
