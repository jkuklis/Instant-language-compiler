.class public ex986
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 7
.limit stack 5
	iconst_3
	bipush 6
	imul
	bipush 27
	iadd
	bipush 43
	bipush 58
	iadd
	bipush 80
	swap
	isub
	iconst_4
	swap
	idiv
	bipush 12
	iadd
	idiv
	bipush 24
	bipush 33
	isub
	bipush 34
	isub
	swap
	isub
	bipush 86
	bipush 38
	iadd
	bipush 25
	bipush 29
	isub
	iadd
	bipush 69
	iconst_0
	isub
	iconst_2
	swap
	idiv
	imul
	bipush 95
	bipush 14
	imul
	idiv
	isub
	bipush 10
	isub
	bipush 35
	bipush 27
	idiv
	bipush 20
	iadd
	imul
	bipush 96
	iadd
	iconst_2
	isub
	bipush 24
	bipush 12
	iadd
	imul
	iconst_5
	imul
	bipush 17
	isub
	bipush 25
	idiv
	istore_3
	iconst_5
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 7
	bipush 33
	idiv
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 52
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 56
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 19
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_1
	isub
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
