.class public ex957
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 27
.limit stack 4
	bipush 10
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	iconst_5
	bipush 9
	isub
	iconst_5
	iadd
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	bipush 6
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 21
	istore 8
	iload 8
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	bipush 29
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	bipush 24
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 22
	iadd
	iload_2
	iadd
	bipush 21
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 47
	iload 8
	iadd
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 55
	idiv
	bipush 52
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload 7
	idiv
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 12
	iload_3
	imul
	bipush 16
	isub
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	bipush 58
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	iload_2
	idiv
	iload_2
	iload_2
	iadd
	iload_0
	iadd
	idiv
	iconst_0
	imul
	bipush 109
	idiv
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iload 12
	imul
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 88
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 66
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	iload_3
	idiv
	iload 8
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	invokevirtual java/io/PrintStream/println(I)V
	bipush 91
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	iconst_2
	istore 14
	iload_0
	bipush 57
	iadd
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 48
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	bipush 56
	iadd
	iload_3
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 18
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload 11
	imul
	iload_1
	imul
	iload_2
	iadd
	bipush 13
	bipush 14
	iadd
	iload 7
	isub
	imul
	bipush 57
	imul
	iload_1
	iadd
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	bipush 17
	imul
	bipush 18
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 48
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	bipush 67
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	bipush 27
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iload 6
	idiv
	iload 4
	swap
	idiv
	iload 4
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 48
	bipush 23
	idiv
	iload_0
	imul
	istore 25
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 87
	iload 5
	idiv
	bipush 26
	imul
	istore 17
	iload 6
	istore 24
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	bipush 25
	imul
	iload_2
	imul
	bipush 12
	idiv
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 28
	iload 7
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 41
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iload 11
	imul
	iconst_2
	iadd
	istore 23
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	bipush 43
	istore 22
	iload_3
	istore 21
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	bipush 19
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload 12
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	iload 17
	iadd
	iload 12
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	iload 9
	iadd
	bipush 46
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	iload 14
	idiv
	iload 6
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	bipush 20
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 49
	iload 8
	iadd
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 26
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	iconst_5
	iadd
	iload 18
	swap
	idiv
	iload_3
	iadd
	bipush 8
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 14
	bipush 39
	idiv
	iload 10
	iadd
	bipush 7
	swap
	idiv
	istore 20
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 11
	bipush 65
	idiv
	iconst_3
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	bipush 11
	isub
	iload 13
	isub
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	bipush 71
	imul
	iload 10
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	bipush 20
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	iload 9
	idiv
	bipush 74
	imul
	bipush 12
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	bipush 51
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 31
	istore 19
	iconst_3
	bipush 14
	isub
	iconst_5
	iadd
	iload 15
	idiv
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	bipush 30
	imul
	iload 16
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
