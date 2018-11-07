.class public ex999
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 10
.limit stack 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	bipush 69
	imul
	bipush 34
	idiv
	bipush 41
	imul
	bipush 28
	bipush 35
	iadd
	bipush 22
	imul
	iadd
	bipush 49
	bipush 11
	iadd
	bipush 40
	isub
	swap
	idiv
	bipush 65
	iadd
	bipush 26
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 7
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	bipush 48
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 20
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	bipush 41
	isub
	bipush 90
	bipush 20
	imul
	iload_1
	swap
	idiv
	iload_0
	idiv
	bipush 7
	isub
	bipush 48
	imul
	iload_0
	idiv
	imul
	iload_1
	iload_1
	isub
	iload_1
	iadd
	iload_1
	isub
	bipush 21
	iload_0
	idiv
	iload_0
	swap
	idiv
	iload_0
	swap
	idiv
	iload_0
	swap
	isub
	bipush 50
	swap
	isub
	iadd
	iadd
	bipush 25
	iload_1
	idiv
	iadd
	bipush 59
	bipush 23
	imul
	bipush 20
	iload_1
	isub
	isub
	iload_0
	iadd
	iload_0
	swap
	isub
	iconst_2
	iload_1
	isub
	iadd
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iload_0
	iadd
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	iload_2
	idiv
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 21
	iload_0
	idiv
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	idiv
	bipush 20
	iadd
	bipush 14
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	iload 4
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 9
	iload_2
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 15
	isub
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	bipush 31
	istore 7
	iload_0
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 67
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
