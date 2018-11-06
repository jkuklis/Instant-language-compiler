.class public test.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 2.limit stack 3	iconst_3
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	iconst_4
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iconst_5
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iconst_3
	iadd
	istore_0
	iconst_5
	iconst_2
	idiv
	iconst_3
	iconst_1
	isub
	iadd
	bipush 6
	iconst_3
	imul
	iadd
	iload_0
	iadd
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iconst_3
	iadd
	istore_1
	return
.end method
