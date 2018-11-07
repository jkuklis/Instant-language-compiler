.class public ex996
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 10
.limit stack 4
	bipush 10
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 24
	iload_0
	imul
	bipush 53
	bipush 14
	isub
	bipush 23
	iadd
	iadd
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	bipush 39
	idiv
	iload_2
	isub
	bipush 41
	swap
	isub
	bipush 25
	iadd
	iload_0
	iload_2
	isub
	imul
	iload_0
	iload_0
	idiv
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	bipush 14
	idiv
	bipush 46
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 47
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iconst_5
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 15
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 48
	bipush 17
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 33
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	bipush 100
	istore 8
	iload 4
	bipush 42
	isub
	istore 7
	bipush 61
	iload_0
	imul
	iload_2
	imul
	bipush 69
	iadd
	bipush 16
	swap
	idiv
	iload_0
	imul
	iload_0
	iadd
	iload_1
	imul
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	iload_1
	isub
	iconst_4
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 69
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	iload_2
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 11
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
