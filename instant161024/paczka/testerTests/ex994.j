.class public ex994
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 8
.limit stack 3
	bipush 44
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 35
	idiv
	iload_1
	idiv
	bipush 54
	iadd
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 23
	istore_2
	bipush 31
	bipush 52
	iadd
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	bipush 53
	istore 7
	iload_0
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	iload_0
	imul
	bipush 36
	isub
	iload_2
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	iload_2
	idiv
	bipush 46
	imul
	bipush 35
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 108
	istore 6
	iload_0
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
