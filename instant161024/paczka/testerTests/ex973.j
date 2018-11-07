.class public ex973
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 5
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 89
	bipush 27
	imul
	bipush 33
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	bipush 57
	isub
	iconst_5
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	iconst_2
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 11
	istore_0
	sipush 130
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	iload_1
	isub
	bipush 27
	iload_1
	iadd
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 72
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 16
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	bipush 18
	bipush 31
	isub
	istore_3
	return
.end method
