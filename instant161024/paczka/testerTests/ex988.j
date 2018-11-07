.class public ex988
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	bipush 12
	isub
	bipush 8
	imul
	bipush 52
	bipush 20
	idiv
	isub
	bipush 65
	iadd
	bipush 41
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	bipush 74
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	bipush 23
	bipush 15
	iadd
	bipush 13
	idiv
	bipush 72
	iadd
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 52
	iadd
	iload_0
	iadd
	iload_0
	imul
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 36
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
