.class public ex978
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 6
.limit stack 3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	bipush 47
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iconst_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 58
	bipush 23
	iadd
	bipush 20
	isub
	iconst_1
	imul
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	bipush 46
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 6
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 60
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_1
	iadd
	bipush 29
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 12
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	bipush 48
	iconst_2
	isub
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	bipush 37
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	bipush 24
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload_2
	istore 4
	return
.end method
