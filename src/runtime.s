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

stringEq:

initArray:
