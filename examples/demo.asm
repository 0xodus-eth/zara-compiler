section .data
    str_0: db "Hot!", 0
    str_1: db "Warm", 0
    str_2: db "Cold", 0
    str_3: db "Hello, ", 0
    str_4: db "Enter a number:", 0
    str_5: db "Compilation complete!", 0
    fmt_int:    db "%d", 10, 0
    fmt_float:  db "%f", 10, 0
    fmt_string: db "%s", 10, 0

section .bss
    input_buf:  resb 256

section .text
    global _start
    extern printf, scanf, malloc, free

_start:
    push rbp
    mov rbp, rsp
    ; count = 0
    mov qword [rbp-8], 0
    ; max = 100
    mov qword [rbp-16], 100
    ; pi = 3.14159
    mov rax, 3.14159
    mov [rbp-24], rax
    ; temperature = 98.6
    mov rax, 98.6
    mov [rbp-32], rax
    ; greeting = "Hello, Zara!"
    mov rax, [rbp-48]
    mov [rbp-40], rax
    ; name = "Compiler"
    mov rax, [rbp-64]
    mov [rbp-56], rax
    ; isRunning = 1
    mov qword [rbp-72], 1
    ; isComplete = 0
    mov qword [rbp-80], 0
    ; t0 = new array[10]
    mov rdi, 80
    call malloc
    mov [rbp-88], rax
    ; numbers = t0
    mov rax, [rbp-88]
    mov [rbp-96], rax
    ; t1 = new array[5]
    mov rdi, 40
    call malloc
    mov [rbp-104], rax
    ; names = t1
    mov rax, [rbp-104]
    mov [rbp-112], rax
    ; t2 = new array[0]
    mov rdi, 0
    call malloc
    mov [rbp-120], rax
    ; operands = t2
    mov rax, [rbp-120]
    mov [rbp-128], rax
    ; t3 = 0 < 100
    mov rax, 0
    mov rbx, 100
    cmp rax, rbx
    setl al
    movzx rax, al
    mov [rbp-136], rax
    ; ifFalse t3 goto L0
    mov rax, [rbp-136]
    test rax, rax
    jz L0
    ; t4 = 0 + 1
    mov rax, 0
    mov rbx, 1
    add rax, rbx
    mov [rbp-144], rax
    ; count = t4
    mov rax, [rbp-144]
    mov [rbp-8], rax
    ; goto L1
    jmp L1
    ; L0:
L0:
    ; count = 0
    mov qword [rbp-8], 0
    ; L1:
L1:
    ; t5 = 98.6 > 100
    mov rax, 98.6
    mov rbx, 100
    cmp rax, rbx
    setg al
    movzx rax, al
    mov [rbp-152], rax
    ; ifFalse t5 goto L2
    mov rax, [rbp-152]
    test rax, rax
    jz L2
    ; print "Hot!"
    lea rsi, [str_0]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf
    ; goto L3
    jmp L3
    ; L2:
L2:
    ; t6 = 98.6 > 70
    mov rax, 98.6
    mov rbx, 70
    cmp rax, rbx
    setg al
    movzx rax, al
    mov [rbp-160], rax
    ; ifFalse t6 goto L4
    mov rax, [rbp-160]
    test rax, rax
    jz L4
    ; print "Warm"
    lea rsi, [str_1]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf
    ; goto L5
    jmp L5
    ; L4:
L4:
    ; print "Cold"
    lea rsi, [str_2]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf
    ; L5:
L5:
    ; L3:
L3:
    ; L6:
L6:
    ; t7 = 0 < 10
    mov rax, 0
    mov rbx, 10
    cmp rax, rbx
    setl al
    movzx rax, al
    mov [rbp-168], rax
    ; ifFalse t7 goto L7
    mov rax, [rbp-168]
    test rax, rax
    jz L7
    ; t8 = 0 * 2
    mov rax, 0
    mov rbx, 2
    imul rax, rbx
    mov [rbp-176], rax
    ; numbers[count] = t8
    mov rax, [rbp-96]
    mov rbx, [rbp-8]
    mov rcx, [rbp-176]
    mov [rax + rbx*8], rcx
    ; t9 = t4
    mov rax, [rbp-144]
    mov [rbp-184], rax
    ; count = t9
    mov rax, [rbp-184]
    mov [rbp-8], rax
    ; goto L6
    jmp L6
    ; L7:
L7:
    ; i = 0
    mov qword [rbp-192], 0
    ; L8:
L8:
    ; t10 = t4
    mov rax, [rbp-144]
    mov [rbp-200], rax
    ; i = t10
    mov rax, [rbp-200]
    mov [rbp-192], rax
    ; t11 = t10 < 5
    mov rax, [rbp-200]
    mov rbx, 5
    cmp rax, rbx
    setl al
    movzx rax, al
    mov [rbp-208], rax
    ; if t11 goto L8
    mov rax, [rbp-208]
    test rax, rax
    jnz L8
    ; j = 0
    mov qword [rbp-216], 0
    ; L9:
L9:
    ; t12 = t7
    mov rax, [rbp-168]
    mov [rbp-224], rax
    ; ifFalse t12 goto L10
    mov rax, [rbp-224]
    test rax, rax
    jz L10
    ; print j
    mov rsi, [rbp-216]
    lea rdi, [fmt_int]
    xor rax, rax
    call printf
    ; t13 = t4
    mov rax, [rbp-144]
    mov [rbp-232], rax
    ; j = t13
    mov rax, [rbp-232]
    mov [rbp-216], rax
    ; goto L9
    jmp L9
    ; L10:
L10:
    ; func_begin add

add:
    push rbp
    mov rbp, rsp
    ; // param a
    ; param a
    ; // param b
    ; param b
    ; t14 = a + b
    mov rax, [rbp-16]
    mov rbx, [rbp-24]
    add rax, rbx
    mov [rbp-8], rax
    ; return t14
    mov rax, [rbp-8]
    mov rsp, rbp
    pop rbp
    ret
    ; func_end add
    ; func_begin multiply

multiply:
    push rbp
    mov rbp, rsp
    ; // param x
    ; param x
    ; // param y
    ; param y
    ; t15 = x * y
    mov rax, [rbp-16]
    mov rbx, [rbp-24]
    imul rax, rbx
    mov [rbp-8], rax
    ; return t15
    mov rax, [rbp-8]
    mov rsp, rbp
    pop rbp
    ret
    ; func_end multiply
    ; func_begin greet

greet:
    push rbp
    mov rbp, rsp
    ; // param personName
    ; param personName
    ; print "Hello, "
    lea rsi, [str_3]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf
    ; print personName
    mov rsi, [rbp-8]
    lea rdi, [fmt_int]
    xor rax, rax
    call printf
    ; func_end greet
    ; func_begin factorial

factorial:
    push rbp
    mov rbp, rsp
    ; // param n
    ; param n
    ; t16 = n <= 1
    mov rax, [rbp-16]
    mov rbx, 1
    cmp rax, rbx
    setle al
    movzx rax, al
    mov [rbp-8], rax
    ; ifFalse t16 goto L11
    mov rax, [rbp-8]
    test rax, rax
    jz L11
    ; return 1
    mov rax, 1
    mov rsp, rbp
    pop rbp
    ret
    ; L11:
L11:
    ; t17 = n - 1
    mov rax, [rbp-16]
    mov rbx, 1
    sub rax, rbx
    mov [rbp-24], rax
    ; t18 = call factorial(t17)
    mov rdi, [rbp-24]
    call factorial
    mov [rbp-32], rax
    ; t19 = n * t18
    mov rax, [rbp-16]
    mov rbx, [rbp-32]
    imul rax, rbx
    mov [rbp-40], rax
    ; return t19
    mov rax, [rbp-40]
    mov rsp, rbp
    pop rbp
    ret
    ; func_end factorial
    ; t20 = call add(5, 3)
    mov rdi, 5
    mov rsi, 3
    call add
    mov [rbp-48], rax
    ; result = t20
    mov rax, [rbp-48]
    mov [rbp-56], rax
    ; t21 = call multiply(2.5, 4)
    mov rdi, 2.5
    mov rsi, 4
    call multiply
    mov [rbp-64], rax
    ; product = t21
    mov rax, [rbp-64]
    mov [rbp-72], rax
    ; t22 = call factorial(5)
    mov rdi, 5
    call factorial
    mov [rbp-80], rax
    ; fact = t22
    mov rax, [rbp-80]
    mov [rbp-88], rax
    ; // class Point
    ; class Point
    ; func_begin Point_constructor

Point_constructor:
    push rbp
    mov rbp, rsp
    ; // param initX
    ; param initX
    ; // param initY
    ; param initY
    ; this.x = initX
    ; this.x = initX
    ; this.y = initY
    ; this.y = initY
    ; func_end Point_constructor
    ; func_begin Point_move

Point_move:
    push rbp
    mov rbp, rsp
    ; // param dx
    ; param dx
    ; // param dy
    ; param dy
    ; t23 = this.x
    ; t23 = this.x
    ; this.x = t24
    ; this.x = t24
    ; t25 = this.y
    ; t25 = this.y
    ; this.y = t26
    ; this.y = t26
    ; func_end Point_move
    ; func_begin Point_distance

Point_distance:
    push rbp
    mov rbp, rsp
    ; t27 = this.x
    ; t27 = this.x
    ; t28 = this.x
    ; t28 = this.x
    ; t29 = t27 * t28
    mov rax, [rbp-16]
    mov rbx, [rbp-24]
    imul rax, rbx
    mov [rbp-8], rax
    ; t30 = this.y
    ; t30 = this.y
    ; t31 = this.y
    ; t31 = this.y
    ; t32 = t30 * t31
    mov rax, [rbp-40]
    mov rbx, [rbp-48]
    imul rax, rbx
    mov [rbp-32], rax
    ; t33 = t29 + t32
    mov rax, [rbp-8]
    mov rbx, [rbp-32]
    add rax, rbx
    mov [rbp-56], rax
    ; return t33
    mov rax, [rbp-56]
    mov rsp, rbp
    pop rbp
    ret
    ; func_end Point_distance
    ; // class ColoredPoint
    ; class ColoredPoint
    ; func_begin ColoredPoint_setColor

ColoredPoint_setColor:
    push rbp
    mov rbp, rsp
    ; // param c
    ; param c
    ; this.color = c
    ; this.color = c
    ; func_end ColoredPoint_setColor
    ; t34 = new Point
    mov rdi, 64
    call malloc
    mov [rbp-8], rax
    ; call Point_constructor(t34, 0, 0)
    mov rdi, [rbp-8]
    mov rsi, 0
    mov rdx, 0
    call Point_constructor
    ; origin = t34
    mov rax, [rbp-8]
    mov [rbp-16], rax
    ; t35 = new Point
    mov rdi, 64
    call malloc
    mov [rbp-24], rax
    ; call Point_constructor(t35, 10, 20)
    mov rdi, [rbp-24]
    mov rsi, 10
    mov rdx, 20
    call Point_constructor
    ; myPoint = t35
    mov rax, [rbp-24]
    mov [rbp-32], rax
    ; t36 = - 3
    mov rax, 3
    neg rax
    mov [rbp-40], rax
    ; t37 = call move(myPoint, 5, t36)
    mov rdi, [rbp-32]
    mov rsi, 5
    mov rdx, [rbp-40]
    call move
    mov [rbp-48], rax
    ; t38 = new ColoredPoint
    mov rdi, 64
    call malloc
    mov [rbp-56], rax
    ; call ColoredPoint_constructor(t38, 1, 1)
    mov rdi, [rbp-56]
    mov rsi, 1
    mov rdx, 1
    call ColoredPoint_constructor
    ; colorPoint = t38
    mov rax, [rbp-56]
    mov [rbp-64], rax
    ; t39 = call setColor(colorPoint, "red")
    mov rdi, [rbp-64]
    mov rsi, [rbp-72]
    call setColor
    mov [rbp-80], rax
    ; t41 = 1 + 6
    mov rax, 1
    mov rbx, 6
    add rax, rbx
    mov [rbp-88], rax
    ; expr1 = t41
    mov rax, [rbp-88]
    mov [rbp-96], rax
    ; t43 = 3 * 3
    mov rax, 3
    mov rbx, 3
    imul rax, rbx
    mov [rbp-104], rax
    ; expr2 = t43
    mov rax, [rbp-104]
    mov [rbp-112], rax
    ; t44 = t9 > 0
    mov rax, [rbp-128]
    mov rbx, 0
    cmp rax, rbx
    setg al
    movzx rax, al
    mov [rbp-120], rax
    ; t45 = t44 && 1
    mov rax, [rbp-120]
    mov rbx, 1
    and rax, rbx
    mov [rbp-136], rax
    ; expr3 = t45
    mov rax, [rbp-136]
    mov [rbp-144], rax
    ; t46 = ! isComplete
    mov rax, [rbp-160]
    test rax, rax
    sete al
    movzx rax, al
    mov [rbp-152], rax
    ; t47 = t9 == 100
    mov rax, [rbp-128]
    mov rbx, 100
    cmp rax, rbx
    sete al
    movzx rax, al
    mov [rbp-168], rax
    ; t48 = t46 || t47
    mov rax, [rbp-152]
    mov rbx, [rbp-168]
    or rax, rbx
    mov [rbp-176], rax
    ; expr4 = t48
    mov rax, [rbp-176]
    mov [rbp-184], rax
    ; print "Enter a number:"
    lea rsi, [str_4]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf
    ; input count
    lea rdi, [fmt_int]
    lea rsi, [rbp-192]
    xor rax, rax
    call scanf
    ; print count
    mov rsi, [rbp-192]
    lea rdi, [fmt_int]
    xor rax, rax
    call printf
    ; print "Compilation complete!"
    lea rsi, [str_5]
    lea rdi, [fmt_string]
    xor rax, rax
    call printf

    ; Exit program
    mov rsp, rbp
    pop rbp
    mov rax, 60
    xor rdi, rdi
    syscall