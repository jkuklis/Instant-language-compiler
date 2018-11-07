.class public ex961
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 9
.limit stack 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	bipush 40
	iadd
	bipush 35
	bipush 17
	imul
	bipush 30
	iadd
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	bipush 49
	bipush 23
	isub
	bipush 18
	iadd
	bipush 33
	iadd
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	iadd
	iconst_4
	swap
	isub
	iload_0
	idiv
	iload_0
	imul
	bipush 34
	bipush 39
	iadd
	iadd
	iload_2
	bipush 16
	isub
	bipush 12
	imul
	bipush 24
	imul
	iload_2
	isub
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	bipush 37
	imul
	iconst_4
	imul
	iload_2
	swap
	isub
	bipush 14
	swap
	isub
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	bipush 38
	imul
	iload_2
	imul
	iload_1
	isub
	iload_1
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 48
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_2
	iadd
	iload_1
	idiv
	bipush 48
	idiv
	iload_2
	imul
	bipush 23
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	bipush 37
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 54
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	iload_3
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	bipush 16
	imul
	bipush 52
	isub
	iload_2
	isub
	iload_1
	bipush 20
	isub
	bipush 18
	iadd
	iload_2
	isub
	bipush 9
	idiv
	bipush 45
	iadd
	isub
	iload_0
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	iload_2
	isub
	bipush 50
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 43
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	iload_3
	isub
	bipush 8
	imul
	bipush 14
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	bipush 41
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_3
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 76
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_3
	isub
	iload 4
	bipush 9
	imul
	isub
	bipush 98
	iadd
	bipush 14
	idiv
	iload_3
	imul
	iconst_4
	bipush 79
	isub
	bipush 63
	iadd
	iload 4
	imul
	imul
	bipush 35
	iload_0
	idiv
	isub
	iload_2
	swap
	isub
	bipush 47
	iload 5
	isub
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	bipush 39
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	iload_0
	isub
	iload_3
	swap
	idiv
	bipush 12
	iadd
	iload 5
	imul
	iload_1
	swap
	idiv
	bipush 53
	imul
	bipush 32
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	iload 5
	isub
	iload_1
	bipush 23
	isub
	isub
	bipush 11
	iconst_3
	isub
	imul
	iconst_2
	iconst_2
	isub
	bipush 35
	imul
	iload 7
	imul
	bipush 23
	idiv
	iadd
	bipush 8
	idiv
	iload 4
	bipush 74
	idiv
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 58
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	iadd
	bipush 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload_2
	idiv
	iload_1
	idiv
	iload_0
	imul
	iload_1
	imul
	bipush 11
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	bipush 30
	iadd
	bipush 14
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 85
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload 4
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iload_1
	imul
	iload 4
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 68
	iload 6
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 69
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	bipush 19
	idiv
	iload_3
	swap
	idiv
	iload 4
	iadd
	iload_0
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	bipush 83
	imul
	bipush 36
	idiv
	bipush 57
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 67
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 74
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	iconst_3
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
