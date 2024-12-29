        .ORIG x3000         ; Program starts at memory location x3000
        LEA R1, MSG         ; Load the starting address of the string into R1

PRINT_LOOP
        LDR R0, R1, #0      ; Load the character pointed by R1 into R0
        BRz  END            ; If the character is zero (end of string), branch to END
        TRAP x21            ; Output the character in R0
        ADD R1, R1, #1      ; Increment R1 to point to the next character
        BR   PRINT_LOOP     ; Repeat the loop

END
        HALT                ; Halt the program

MSG     .STRINGZ "Hello, World!" ; Null-terminated string to be printed

        .END
