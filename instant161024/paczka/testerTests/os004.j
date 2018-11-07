.class public os004
.super java/lang/Object

; standard initializer
.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 65
.limit stack 5
	bipush 13
	istore_0
	bipush 12
	iload_0
	iadd
	iconst_1
	isub
	iload_0
	swap
	idiv
	iload_0
	imul
	istore 12
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 9
	iload_0
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	bipush 26
	istore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iload 12
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	invokevirtual java/io/PrintStream/println(I)V
	iconst_1
	istore_2
	iconst_1
	bipush 23
	iadd
	bipush 10
	idiv
	istore 18
	bipush 17
	istore 4
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 59
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	iload 4
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	iload_3
	imul
	bipush 52
	isub
	bipush 10
	swap
	isub
	bipush 32
	bipush 7
	isub
	isub
	iload 4
	imul
	istore 6
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 59
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 53
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	iload 18
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	iload_2
	isub
	bipush 19
	imul
	iload_0
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 29
	istore 21
	iload 4
	bipush 15
	iadd
	istore 26
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 93
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	bipush 46
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	iconst_5
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iconst_1
	istore 11
	iload 5
	istore 20
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	invokevirtual java/io/PrintStream/println(I)V
	iload_3
	istore_1
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 45
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	iload_1
	isub
	bipush 32
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload 21
	istore 8
	iload 5
	iload_3
	idiv
	iload_1
	imul
	iload_1
	iload 5
	idiv
	bipush 26
	imul
	iload 5
	isub
	iadd
	bipush 52
	iadd
	istore 10
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iconst_3
	imul
	iload 18
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	bipush 23
	istore 7
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 47
	bipush 8
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 20
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	iload 4
	istore 17
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 51
	iload 20
	imul
	iload_0
	imul
	iload 7
	bipush 46
	imul
	isub
	bipush 31
	isub
	bipush 14
	iload_2
	isub
	idiv
	iload 26
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 30
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	bipush 35
	isub
	iload 10
	iload 7
	idiv
	idiv
	iload 6
	bipush 87
	iadd
	iload_2
	imul
	iadd
	iload 11
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 20
	iconst_2
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	bipush 27
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	iconst_4
	isub
	bipush 8
	imul
	iload 21
	swap
	idiv
	iload_2
	iadd
	bipush 62
	iadd
	iload_3
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	bipush 17
	idiv
	iload 6
	imul
	bipush 14
	swap
	isub
	bipush 36
	iadd
	bipush 54
	imul
	bipush 7
	iadd
	bipush 17
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iconst_5
	istore 29
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	invokevirtual java/io/PrintStream/println(I)V
	bipush 42
	istore 54
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 83
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	iconst_4
	isub
	bipush 10
	imul
	iload_3
	swap
	isub
	iload 4
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	iload 6
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	iload_3
	isub
	invokevirtual java/io/PrintStream/println(I)V
	bipush 25
	iload 21
	iadd
	istore 25
	iload 4
	istore 16
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 13
	istore 19
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 36
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	bipush 14
	istore 15
	bipush 32
	istore 45
	bipush 7
	istore 24
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 38
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 45
	iconst_4
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	bipush 6
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	iload 11
	isub
	bipush 34
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	bipush 30
	isub
	iload 25
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload 25
	istore 44
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 75
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	iload 7
	imul
	bipush 21
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iconst_3
	istore 14
	bipush 7
	istore 43
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 45
	invokevirtual java/io/PrintStream/println(I)V
	bipush 9
	iload 14
	iadd
	iload 30
	swap
	idiv
	istore 42
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 79
	iload_3
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iload_0
	istore 9
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	iload 29
	istore 13
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 62
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 67
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	bipush 27
	idiv
	bipush 16
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 60
	iload 11
	isub
	iconst_4
	imul
	bipush 23
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 55
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 19
	invokevirtual java/io/PrintStream/println(I)V
	iconst_2
	istore 23
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 69
	bipush 40
	idiv
	bipush 46
	bipush 6
	idiv
	iadd
	bipush 8
	imul
	iload_2
	isub
	iload 9
	imul
	iload 6
	bipush 31
	isub
	bipush 33
	iload 8
	iadd
	imul
	imul
	iload 29
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload_1
	istore 22
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 26
	bipush 58
	iadd
	iload_3
	bipush 48
	iadd
	iload_2
	swap
	isub
	isub
	invokevirtual java/io/PrintStream/println(I)V
	iload 11
	istore 41
	bipush 60
	istore 40
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 30
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 74
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	bipush 59
	isub
	bipush 70
	iload 14
	idiv
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 101
	iload 22
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 40
	bipush 43
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_2
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	iload 16
	iadd
	iload 19
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload 14
	bipush 28
	isub
	istore 53
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 12
	iload 11
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 18
	istore 64
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 6
	iload 44
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 22
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	iload 24
	iadd
	bipush 49
	swap
	idiv
	iload 26
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 36
	istore 52
	iconst_4
	iload 7
	iadd
	iload 9
	bipush 49
	idiv
	isub
	bipush 21
	swap
	isub
	iload 17
	iadd
	istore 39
	iload 7
	istore 38
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 52
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 38
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	iload 9
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 17
	bipush 12
	isub
	istore 37
	bipush 21
	istore 51
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 12
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 24
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	iload 43
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 72
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	bipush 8
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	iload 37
	istore 63
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 9
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 42
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	iload 9
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 6
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 15
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	invokevirtual java/io/PrintStream/println(I)V
	bipush 30
	istore 50
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	bipush 125
	istore 36
	iload 25
	istore 35
	iload 52
	istore 49
	bipush 16
	bipush 70
	imul
	istore 34
	bipush 65
	istore 48
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 31
	invokevirtual java/io/PrintStream/println(I)V
	bipush 43
	istore 33
	iconst_3
	iconst_1
	idiv
	istore 28
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_4
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_3
	invokevirtual java/io/PrintStream/println(I)V
	bipush 11
	istore 62
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 29
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 48
	bipush 93
	isub
	iconst_3
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 39
	invokevirtual java/io/PrintStream/println(I)V
	iload 35
	istore 61
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 44
	bipush 74
	idiv
	bipush 10
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 65
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 51
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 87
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 22
	iload 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 41
	invokevirtual java/io/PrintStream/println(I)V
	iload 42
	bipush 61
	isub
	istore 60
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 10
	iload 43
	isub
	iload 8
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 18
	iload 41
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 14
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 23
	iload 34
	iadd
	bipush 64
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	bipush 44
	iconst_3
	iadd
	bipush 10
	swap
	isub
	istore 32
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 54
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 30
	invokevirtual java/io/PrintStream/println(I)V
	iload 40
	istore 47
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 16
	invokevirtual java/io/PrintStream/println(I)V
	bipush 69
	istore 31
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 23
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 14
	invokevirtual java/io/PrintStream/println(I)V
	bipush 75
	bipush 27
	imul
	istore 46
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 33
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	iload 13
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 36
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 5
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 90
	bipush 17
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 100
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 28
	bipush 8
	iadd
	iload 36
	iload 19
	iadd
	bipush 53
	iadd
	isub
	iload 48
	iload 47
	imul
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 49
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 8
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	iload 42
	iadd
	bipush 11
	swap
	idiv
	iload 18
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	iload 53
	isub
	iload 14
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 51
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 13
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 38
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 7
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 31
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 52
	iload 39
	iadd
	iload 13
	swap
	isub
	bipush 30
	iadd
	iload 23
	imul
	invokevirtual java/io/PrintStream/println(I)V
	iconst_4
	iload 13
	isub
	iload 7
	imul
	bipush 11
	iload 17
	idiv
	iload 28
	iadd
	iload 11
	idiv
	iload_0
	swap
	idiv
	idiv
	iload 32
	imul
	istore 27
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 39
	bipush 8
	isub
	iload 10
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 59
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 18
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 27
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	bipush 8
	isub
	bipush 11
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 32
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_2
	iload 40
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	iload 37
	isub
	iconst_3
	iload 9
	isub
	bipush 34
	imul
	iload_3
	imul
	iload 44
	swap
	idiv
	iload 50
	isub
	iadd
	bipush 37
	iadd
	iconst_5
	iadd
	bipush 58
	bipush 41
	idiv
	bipush 92
	idiv
	iload 19
	iadd
	bipush 16
	iadd
	iadd
	iload_2
	iload 23
	imul
	swap
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 27
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 46
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 35
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 17
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 21
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 111
	iload 6
	imul
	invokevirtual java/io/PrintStream/println(I)V
	bipush 12
	istore 59
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_1
	iload 5
	imul
	bipush 29
	iload 13
	iadd
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 58
	iload 22
	isub
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 80
	iload 15
	imul
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 50
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 36
	iload 27
	iadd
	iload_2
	swap
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 28
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 11
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 15
	invokevirtual java/io/PrintStream/println(I)V
	iload 20
	istore 58
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 16
	iload 35
	isub
	iload 8
	idiv
	iconst_2
	iadd
	invokevirtual java/io/PrintStream/println(I)V
	iload 24
	iload_1
	imul
	istore 57
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 22
	invokevirtual java/io/PrintStream/println(I)V
	bipush 31
	istore 56
	bipush 66
	istore 55
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 28
	iload 10
	isub
	bipush 30
	iload 34
	isub
	iload 9
	imul
	imul
	bipush 26
	idiv
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 34
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
