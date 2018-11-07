.class public ex991
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
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	bipush 24
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 46
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	bipush 100
	isub
	bipush 32
	iload_0
	isub
	isub
	bipush 56
	swap
	isub
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 17
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
