.class public ex964
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 4
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 60
	sipush 148
	imul
	bipush 8
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 18
	bipush 58
	idiv
	bipush 15
	bipush 77
	idiv
	iadd
	bipush 18
	bipush 41
	imul
	isub
	bipush 52
	bipush 56
	isub
	bipush 70
	swap
	isub
	bipush 34
	iadd
	iconst_1
	idiv
	bipush 21
	imul
	iadd
	bipush 45
	iadd
	istore_0
	iload_0
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 60
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	iload_0
	iadd
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 60
	bipush 43
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	bipush 46
	isub
	bipush 45
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_1
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore_3
	return
.end method
