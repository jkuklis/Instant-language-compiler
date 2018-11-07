.class public ex997
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 2
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 24
	bipush 19
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	bipush 21
	idiv
	bipush 29
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 37
	bipush 59
	iadd
	bipush 18
	bipush 60
	isub
	imul
	istore_1
	bipush 43
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 100
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
