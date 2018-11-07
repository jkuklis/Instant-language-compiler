.class public ex968
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 3
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	bipush 37
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 54
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 90
	bipush 52
	idiv
	bipush 31
	bipush 20
	iadd
	isub
	bipush 35
	swap
	isub
	iconst_3
	imul
	bipush 42
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 40
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	imul
	iload_0
	isub
	bipush 33
	imul
	bipush 18
	iadd
	iload_0
	imul
	iload_0
	iadd
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	bipush 9
	imul
	iconst_5
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 18
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	iload_0
	imul
	istore_2
	return
.end method
