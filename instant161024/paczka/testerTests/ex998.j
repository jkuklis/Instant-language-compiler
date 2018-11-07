.class public ex998
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
	bipush 22
	bipush 25
	idiv
	bipush 61
	swap
	isub
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 18
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 33
	isub
	iload_0
	swap
	isub
	bipush 21
	imul
	iload_0
	idiv
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload 6
	istore 5
	bipush 9
	bipush 19
	idiv
	istore 4
	bipush 32
	istore_2
	bipush 23
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	bipush 8
	idiv
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 78
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 67
	iload_2
	isub
	iload_1
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 6
	idiv
	bipush 44
	imul
	iload_3
	bipush 39
	iadd
	iadd
	bipush 38
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	bipush 41
	istore 7
	return
.end method
