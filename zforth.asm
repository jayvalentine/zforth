    org         $0000
    jp      hello

    org         $0100
hello:
    ld      HL, str     ; HL holds destination of character
    ld      B, 6        ; B holds loop count.
loop:
    ld      A, (HL)     ; Load character and output to port 0.
    out     (0), A
    inc     HL          ; Increment address and loop.
    djnz    loop
done:
    halt                ; Done.

    org         $1000
str:
    text    "Hello!"
    