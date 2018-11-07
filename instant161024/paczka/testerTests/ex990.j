.class public ex990
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 9
.limit stack 4
	bipush 23
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	iload_0
	imul
	bipush 29
	bipush 46
	iadd
	bipush 62
	swap
	idiv
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	iadd
	iconst_1
	idiv
	bipush 38
	iadd
	bipush 18
	bipush 21
	isub
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	bipush 19
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	iload_3
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 99
	bipush 10
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	bipush 64
	idiv
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 77
	bipush 28
	isub
	bipush 29
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iload 5
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	bipush 12
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 92
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	bipush 65
	imul
	iconst_2
	isub
	bipush 27
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 40
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 8
	bipush 27
	istore 7
	iload_1
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	iload 4
	imul
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
