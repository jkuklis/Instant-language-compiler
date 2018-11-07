.class public ex954
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 4
.limit stack 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 65
	invokevirtual java/io/PrintStream/println(I)V
	bipush 20
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 17
	bipush 12
	imul
	iload_0
	imul
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	bipush 10
	idiv
	bipush 86
	bipush 53
	imul
	iload_0
	isub
	idiv
	bipush 11
	iload_0
	imul
	iconst_3
	swap
	idiv
	bipush 28
	iload_0
	isub
	bipush 23
	idiv
	iload_0
	iadd
	bipush 24
	imul
	bipush 34
	iadd
	iload_0
	swap
	isub
	iload_0
	iadd
	isub
	bipush 59
	isub
	iload_0
	bipush 44
	isub
	iadd
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	bipush 21
	imul
	iload_1
	isub
	iconst_5
	iload_1
	imul
	iload_1
	swap
	idiv
	isub
	iload_1
	imul
	bipush 9
	iconst_3
	isub
	iload_1
	swap
	isub
	swap
	isub
	bipush 35
	swap
	idiv
	iload_0
	iconst_5
	idiv
	bipush 15
	imul
	isub
	istore_2
	bipush 56
	iload_1
	isub
	bipush 14
	swap
	isub
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 126
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 62
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
