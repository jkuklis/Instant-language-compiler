.source sumto.j
.class  public Sumto
.super  java/lang/Object

; standard initializer
.method public <init>()V
   aload_0
 
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method static public sumto(I)I
.limit locals 3
.limit stack 2
     iconst_0        
     istore_1        
     iconst_0        
     istore_2        
L1: 
     iload_1         
     iload_0         
     if_icmpge L2
     iload_1         
     iconst_1        
     iadd            
     istore_1        
     iload_2        
     iload_1        
     iadd            
     istore_2        
     goto  L1
 L2: 
     iload_2         
     ireturn         
.end method


.method public static main([Ljava/lang/String;)V
.limit stack 5
	getstatic java/lang/System/out Ljava/io/PrintStream;
	bipush 10
	invokestatic Sumto/sumto(I)I
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method
