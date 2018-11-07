.class public ex992
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 14
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	bipush 33
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 16
	bipush 29
	imul
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iconst_3
	istore_1
	iload_0
	istore_2
	iload_1
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 25
	imul
	iload_1
	imul
	bipush 32
	iadd
	iload_2
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	bipush 16
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 42
	istore 8
	iload_0
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 30
	bipush 28
	imul
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	bipush 32
	iadd
	iload_3
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	bipush 55
	iadd
	iload 8
	iadd
	iload_2
	iload_1
	imul
	imul
	iload_1
	swap
	isub
	bipush 7
	iload_0
	iadd
	bipush 22
	isub
	bipush 54
	iadd
	imul
	iload 6
	bipush 13
	isub
	iadd
	iload_1
	bipush 8
	iadd
	iload_0
	swap
	isub
	bipush 19
	iadd
	bipush 64
	imul
	bipush 14
	swap
	isub
	iadd
	bipush 61
	iadd
	iload_3
	isub
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 26
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	iload 7
	isub
	istore 9
	iload_0
	bipush 15
	isub
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	iload 9
	imul
	iload_0
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	bipush 55
	isub
	istore 12
	bipush 80
	bipush 18
	idiv
	iload_0
	iadd
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	iload 4
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 46
	istore 10
	return
.end method
