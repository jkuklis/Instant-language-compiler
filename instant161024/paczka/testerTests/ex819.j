.class public ex819
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 34
.limit stack 5
	bipush 17
	bipush 87
	imul
	iconst_2
	idiv
	istore_0
	iload_0
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload_0
	isub
	iload_0
	iload_0
	iadd
	iadd
	iload_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 26
	bipush 17
	imul
	iload_0
	bipush 31
	isub
	imul
	iload_0
	bipush 29
	iadd
	iload_1
	imul
	bipush 82
	idiv
	iload_0
	swap
	isub
	bipush 44
	iadd
	iload_1
	imul
	iadd
	iload_1
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 22
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 61
	iload_1
	imul
	iload 9
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload 9
	istore 8
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	iload_0
	idiv
	iconst_1
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	bipush 46
	imul
	iconst_0
	iload 8
	iadd
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 43
	bipush 61
	iadd
	iload 9
	bipush 43
	isub
	bipush 38
	imul
	bipush 48
	isub
	imul
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 37
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 17
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 78
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	bipush 47
	imul
	iload 4
	imul
	iload 5
	iadd
	iload 5
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	iload 5
	imul
	bipush 69
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	bipush 104
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	iconst_1
	iadd
	bipush 64
	iadd
	istore_2
	iconst_4
	bipush 8
	isub
	iload 9
	swap
	idiv
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	bipush 21
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 103
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	iload_0
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 6
	iload 7
	imul
	bipush 75
	swap
	isub
	bipush 22
	swap
	idiv
	bipush 12
	iload_2
	imul
	iadd
	iload_0
	bipush 20
	idiv
	isub
	istore 24
	iload 9
	istore_3
	bipush 32
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 92
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	bipush 44
	iadd
	bipush 52
	iload_3
	isub
	isub
	iload_0
	swap
	isub
	iload_1
	iload_1
	imul
	iload 4
	isub
	imul
	iload 12
	bipush 11
	imul
	imul
	iload_2
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 45
	istore 6
	iload_2
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 59
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	bipush 16
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	iload 7
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	bipush 43
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 68
	bipush 62
	isub
	iload_0
	imul
	istore 23
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	iload 13
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload 8
	imul
	iconst_2
	bipush 19
	imul
	isub
	iload 4
	imul
	iload_3
	iload_1
	isub
	iload 4
	iconst_2
	isub
	iadd
	idiv
	iload_0
	swap
	idiv
	bipush 14
	iadd
	iload 13
	imul
	bipush 70
	imul
	iload 5
	iload_2
	isub
	iload 7
	isub
	imul
	iload 6
	isub
	bipush 29
	swap
	idiv
	bipush 23
	isub
	iload 8
	iload 10
	idiv
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	invokevirtual java/io/PrintStream/println(I)V
	bipush 37
	iload_2
	idiv
	istore 29
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 49
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 24
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	istore 11
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 49
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	invokevirtual java/io/PrintStream/println(I)V
	bipush 37
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	iload 10
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	iload 12
	istore 15
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload 5
	iload 6
	idiv
	istore 14
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload 11
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload 6
	imul
	bipush 19
	bipush 9
	imul
	iload 23
	imul
	isub
	iload_1
	bipush 69
	imul
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	iload 14
	istore 28
	bipush 56
	istore 22
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 21
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 49
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	iload_0
	isub
	iload_3
	bipush 11
	isub
	iadd
	iload_2
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 64
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload 11
	bipush 18
	isub
	istore 20
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 27
	invokevirtual java/io/PrintStream/println(I)V
	bipush 52
	iload 8
	isub
	istore 27
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 19
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	bipush 75
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 26
	invokevirtual java/io/PrintStream/println(I)V
	iconst_5
	istore 33
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	bipush 60
	istore 18
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	bipush 10
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	bipush 72
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	iload_2
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	bipush 9
	istore 32
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 19
	iload 9
	iadd
	iload_0
	bipush 43
	isub
	isub
	bipush 25
	imul
	bipush 15
	imul
	bipush 50
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 23
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	iload 4
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 78
	invokevirtual java/io/PrintStream/println(I)V
	iload 11
	istore 31
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 25
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_0
	invokevirtual java/io/PrintStream/println(I)V
	iload 22
	istore 30
	bipush 21
	istore 25
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	bipush 24
	idiv
	iload 8
	iadd
	iload 12
	iload 25
	iadd
	imul
	bipush 22
	swap
	isub
	iload 19
	iload 7
	idiv
	imul
	bipush 16
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
