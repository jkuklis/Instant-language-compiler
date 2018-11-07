.class public ex971
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 16
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 24
	bipush 23
	idiv
	bipush 20
	swap
	isub
	bipush 34
	swap
	isub
	bipush 33
	swap
	isub
	bipush 41
	isub
	iconst_1
	imul
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 66
	iload_0
	iadd
	iconst_5
	imul
	bipush 42
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	istore_2
	iload_0
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 77
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	iload_1
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 36
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	bipush 30
	isub
	iload_2
	iadd
	bipush 57
	bipush 15
	imul
	isub
	bipush 56
	swap
	idiv
	iload 4
	bipush 42
	isub
	iload 4
	isub
	bipush 62
	iadd
	iload_2
	swap
	idiv
	imul
	iload_2
	isub
	bipush 57
	bipush 21
	iadd
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	iload 4
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 59
	iload_1
	imul
	bipush 26
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	bipush 44
	iadd
	bipush 6
	iadd
	iload_3
	iload_2
	iadd
	idiv
	bipush 95
	swap
	idiv
	iload_3
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 64
	imul
	iload_1
	swap
	idiv
	bipush 27
	iadd
	bipush 7
	iadd
	bipush 11
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 43
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	bipush 77
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload_0
	iadd
	iload_3
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	iload 4
	iadd
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	bipush 34
	imul
	iload_0
	bipush 15
	imul
	isub
	iload 8
	imul
	iload_2
	iconst_3
	imul
	imul
	bipush 13
	imul
	bipush 72
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 61
	iload 4
	iadd
	iload 4
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	bipush 45
	idiv
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 29
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 55
	iload 7
	isub
	bipush 107
	iload_3
	idiv
	iload 6
	isub
	isub
	bipush 11
	iload_1
	isub
	iload 5
	imul
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 58
	invokevirtual java/io/PrintStream/println(I)V
	bipush 77
	iload 5
	iadd
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	bipush 10
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 48
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	iload 5
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	invokevirtual java/io/PrintStream/println(I)V
	bipush 60
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iconst_1
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	iconst_2
	iadd
	iload_1
	idiv
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	iconst_2
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 55
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	bipush 14
	isub
	iload 8
	idiv
	iload_3
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload_1
	iadd
	bipush 65
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	bipush 45
	istore 13
	iload 5
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	bipush 49
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload 9
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	bipush 38
	imul
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
