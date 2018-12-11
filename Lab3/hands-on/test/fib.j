;; BEGIN HEADER

.class public fib
.super java/lang/Object

.method public <init>()V
  .limit locals 1

  aload_0
  invokespecial java/lang/Object/<init>()V
  return

.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 1
  .limit stack  1

  invokestatic fib/main()I
  pop
  return

.end method

;; END HEADER

.method public static main()I
  .limit locals 5
  .limit stack  3

  ;; int n = 10;

  bipush 10
  istore_0

  ;; int i = 0;

  iconst_0
  istore_1

  ;; int j = 1;

  iconst_1
  istore_2

  ;; int k = 0;

  iconst_0
  istore_3

  ;; while (k int.< n)

  L0:
  iload_3
  iload_0
  if_icmplt L2
  iconst_0
  goto L3
  L2:
  iconst_1
  L3:
  ifeq L1

  ;; int h = j;

  iload_2
  istore 4

  ;; (int) j = j int.+ i;

  iload_2
  iload_1
  iadd
  istore_2
  iload_2
  pop

  ;; (int) i = h;

  iload 4
  istore_1
  iload_1
  pop

  ;; (int) k = k int.+ 1;

  iload_3
  iconst_1
  iadd
  istore_3
  iload_3
  pop
  goto L0
  L1:

  ;; (void) printInt (i);

  iload_1
  invokestatic Runtime/printInt(I)V

  ;; return (int) 0;

  iconst_0
  ireturn

.end method
