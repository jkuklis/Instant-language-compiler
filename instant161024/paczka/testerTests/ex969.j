.class public ex969
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1
.limit stack 3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	bipush 11
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iconst_2
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
