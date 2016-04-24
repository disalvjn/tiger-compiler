print:
        move $a0, $v1
        li $v0, 4
        syscall
        jr $ra

printInt:
        move $a0, $v1
        li $v0, 1
        syscall
        jr $ra

readStr:

readInt:

size:

substring:

concat:

exit:

malloc:
        move $a0, $v1
        li $v0, 9
        syscall
        jr $ra

stringEq:

initArray:
        # v1 is init, a0 is size
        li $a2, 4 # wordsize
        mul $a0, $a0, $a2
        li $v0, 9
        syscall # v0 is array base addr
        addi $a0, $a0, -4
initArrayFill:
        add $a1, $a0, $v0 # a1 elem location
        sw $v1, 0($a1)
        addi $a0, $a0, -4
        bge $a0, $0, initArrayFill
        jr $ra
