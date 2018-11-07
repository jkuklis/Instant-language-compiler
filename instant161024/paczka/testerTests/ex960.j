.class public ex960
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 8
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	bipush 42
	idiv
	bipush 22
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 17
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	bipush 11
	imul
	iload_1
	imul
	bipush 12
	isub
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 82
	invokevirtual java/io/PrintStream/println(I)V
	bipush 42
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	invokevirtual java/io/PrintStream/println(I)V
	bipush 27
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 7
	iadd
	iconst_4
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	iload_3
	idiv
	iload_0
	iadd
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 52
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	bipush 18
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 23
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 43
	iadd
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	bipush 30
	bipush 32
	idiv
	iload_3
	bipush 48
	imul
	imul
	istore 4
	iload 4
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	iload 4
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	bipush 10
	isub
	iload_1
	swap
	idiv
	iload 5
	iadd
	iload_2
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	sipush 140
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 61
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload 5
	iadd
	iload_0
	idiv
	bipush 111
	iload_0
	idiv
	imul
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	iload_2
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	bipush 38
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	iload_0
	idiv
	bipush 37
	imul
	iconst_5
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 70
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
