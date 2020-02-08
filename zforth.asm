
; Register allocation.
; FORTH requires some registers to be allocated for specific functions.

; IP - the Interpreter Pointer.
; IP always points to the address of the next subroutine to be executed.
; In our case, the IP is mapped onto DE.

; W - the Working register.
; W is used for holding the address of the subroutine when we execute it.
; W is mapped onto HL.

; Some macros.

; These macros are general-purpose.

; INC2.
; Increment twice the given 16-bit register pair.
    macro INC2, regpair
    inc     \regpair
    inc     \regpair
    endmacro

; DEC2.
; Decrement twice the given 16-bit register pair.
    macro DEC2, regpair
    dec     \regpair
    dec     \regpair
    endmacro

; These macros are directly FORTH-related.

; Pushing and Popping from the Return Stack.
; Push goes High-Low so Pop goes Low-High.
    macro   PUSHRSP
    inc     IX
    ld      (IX), D
    inc     IX
    ld      (IX), E
    endmacro

    macro   POPRSP
    ld      E, (IX)
    dec     IX
    ld      D, (IX)
    dec     IX
    endmacro

; NEXT.
; NEXT loads the address of the next subroutine to be executed,
; which is pointed to by IP, and jumps to it. It also advances the IP
; to the next codeword to be executed.
    macro   NEXT
    ld      A, (DE)     ; Load next address to be executed.
    ld      L, A
    inc     DE
    ld      A, (DE)
    ld      H, A
    inc     DE

    jp      (HL)        ; Execute the subroutine!
    endmacro

; DOCOL - "Do Colon"
; This is Forth's interpreter.
; Its job is to save the 'old' IP value and set IP
; to the address of the next codeword to be executed.
    macro   DOCOL
    PUSHRSP             ; Push IP onto the stack.

    ld      DE, $18
    adc     HL, DE      ; HL should contain the start of this DOCOL
                        ; so adding the size of DOCOL gives the first data word.
    
    ld      D, H
    ld      E, L
    NEXT                ; Load the address into IP and NEXT!
    endmacro

; Reset vector.
    org     $0000
    jp      start

; Program start.
    org     $0100
start:
    ; Set up the stacks. SP points to the Parameter Stack,
    ; which grows down from 0xFFFD.
    ; IX points to the Return Stack, which grows upwards from
    ; 0xF000.
    ;
    ; This means that we have 4094 bytes available to be shared
    ; between them.
    ; Having one grow up and the other down allows them to share the space
    ; more efficiently - space not used by one stack can be used
    ; by the other.
    ld      SP, $FFFD
    ld      IX, $F000

    ld      DE, cold_start
    NEXT

cold_start:
    addr    QUIT
    addr    GOODBYE

; Let's set up the dictionary.

; A dictionary entry looks like this:

;   2       address of previous word.
;   1       length <n>.
;   <n>     name, up to 256 characters.
;   2       codeword
;   2 * n   pointer to codeword of subsequent words.

; A special assembler symbol to keep track of the last word we defined.
LINK set 0

; Let's write a macro to do this.
    macro   DEFWORD, name, length, label
name_\label:
    addr    LINK
LINK set name_\label
    byte    \length
    text    \name
\label:
    DOCOL
    endmacro

; Let's also write a macro to define a word in assembler.
    macro   DEFCODE, name, length, label
name_\label:
    addr    LINK
LINK set name_\label
    byte    \length
    text    \name
\label:
    endmacro

; Now we can actually define some primitives.
; These are some simple ones, defined in assembler.

; Stack manipulations.

    DEFCODE "DROP", 4, DROP
    pop     HL          ; Drop the top of the parameter stack.
    NEXT

    DEFCODE "SWAP", 4, SWAP
    pop     HL
    pop     BC
    push    HL
    push    BC
    NEXT

    DEFCODE "DUP", 3, DUP
    pop     HL
    push    HL
    push    HL
    NEXT

; Arithmetic words.

    DEFCODE "1+", 2, INCR
    pop     HL
    inc     HL
    push    HL
    NEXT

    DEFCODE "1-", 2, DECR
    pop     HL
    dec     HL
    push    HL
    NEXT

    DEFCODE "2+", 2, INCR2
    pop     HL
    INC2    HL
    push    HL
    NEXT

    DEFCODE "2-", 2, DECR2
    pop     HL
    DEC2    HL
    push    HL
    NEXT

    DEFCODE "+", 1, ADD
    pop     HL
    pop     BC
    adc     HL, BC
    push    HL
    NEXT

    DEFCODE "-", 1, SUB
    pop     HL
    pop     BC
    sbc     HL, BC
    push    HL
    NEXT

; Memory operations.

    DEFCODE "!", 1, STORE
    pop     IY              ; TOS is address to store at.
    pop     HL              ; Next is data to store.
    ld      (IY), L
    ld      (IY+1), H
    NEXT

    DEFCODE "@", 1, FETCH
    pop     IY
    ld      L, (IY)
    ld      H, (IY+1)
    push    HL
    NEXT

; Input/Output.

    ; KEY gets a character from the serial line.
    DEFCODE "KEY", 3, KEY
    call    _KEY            ; The key pressed is returned in A.
    ld      L, A            ; Put it in the lower half of HL.
    ld      H, 0
    push    HL              ; Push onto the stack.
    NEXT

; Simple subroutine to get a key from the serial line, returned in A.
_KEY:
    ; Wait for ready.
_KEY_not_ready:
    in      A, (1)          ; Polling loop until we can read a character.
    cp      1
    jp      nz, _KEY_not_ready
    
    in      A, (0)          ; Get the character.
    out     (0), A          ; Echo it to the user.
    ret

    ; EMIT writes a character to the serial line.
    DEFCODE "EMIT", 4, EMIT
    pop     HL
    ld      A, L
    out     (0), A
    NEXT

    ; DOT ('.') prints the top of the stack.
    DEFCODE ".", 1, DOT
    pop     HL
    call    _DOT
    NEXT

_DOT:
    ; Print top half of H.
    ld      A, H
    srl     A
    srl     A
    srl     A
    srl     A

    call    print_hex_single

    ; Print bottom half of H.
    ld      A, H
    and     A, $0f

    call    print_hex_single

    ; Print top half of L.
    ld      A, L
    srl     A
    srl     A
    srl     A
    srl     A

    call    print_hex_single

    ; Print bottom half of L.
    ld      A, L
    and     A, $0f

    call    print_hex_single

    ret

; Prints a single hexadecimal digit.
; Expects the value to print to be in A.
; Behaviour is undefined if the digit is above 15.
print_hex_single:
    ; Is the value <= 9?
    cp      10
    jp      m, print_hex_single_digit

    ; We didn't branch, so it's between 10 and 15.
    add     $37
    out     (0), A
    ret

print_hex_single_digit:
    add     A, $30
    out     (0), A
    ret

    ; PRINT writes a string to the serial line.
    ; The TOS is the address of the string to write,
    ; and NOS is the size.
    DEFCODE "PRINT", 5, PRINT
    pop     HL
    pop     BC
    ld      B, C

_PRINT_loop:
    ld      A, (HL)
    out     (0), A
    inc     HL
    djnz    _PRINT_loop

_PRINT_done:
    NEXT

    DEFCODE "NEWLINE", 7, NEWLINE
    call    _NEWLINE
    NEXT

_NEWLINE:
    ld      A, $0d
    out     (0), A
    ld      A, $0a
    out     (0), A
    ret

    DEFCODE "DUMPSTACK", 9, DUMPSTACK
    ld      ($8800), SP
    ld      IY, ($8800)

    ld      L, (IY)
    ld      H, (IY+1)

    call    _DOT
    call    _NEWLINE

    ld      L, (IY+2)
    ld      H, (IY+3)

    call    _DOT
    call    _NEWLINE

    ld      L, (IY+4)
    ld      H, (IY+5)

    call    _DOT
    call    _NEWLINE

    NEXT

    ; WORD gets a space-delimited word from the input stream.
    DEFCODE "WORD", 4, WORD
    call    _WORD           ; Call the WORD subroutine.
                            ; HL will hold the base address, and B the size.

    ld      C, B            ; Push size in lower half of BC.
    ld      B, 0
    push    BC

    push    HL              ; Push base address.
    NEXT

_WORD:
_WORD_skip:
    call    _KEY
    cp      ' '             ; Is this a space?
    jp      z, _WORD_skip   ; Loop until it's not.

    ld      HL, _WORD_buf   ; Load start address of buffer
                            ; into HL.
    
    ld      (HL), A         ; Store first character in the buffer.
    inc     HL

    ld      B, 1            ; Initialize B, which will hold the size.
                            ; We initialize to 1 because we've already consumed
                            ; one character of the word.

_WORD_get:
    call    _KEY
    cp      ' '             ; Is this a space?
    jp      z, _WORD_done   ; If so, we're done.

    ld      (HL), A         ; If not, store the character
    inc     HL              ; and move onto the next location.
    inc     B

    jp      _WORD_get       ; Repeat.

_WORD_done:
    ld      HL, _WORD_buf
    ret

    ; NUMBER parses a number out of the given string.
    DEFCODE "NUMBER", 6, NUMBER
    pop     BC              ; Size.
    pop     HL              ; Base address of string.
    ld      B, C

    call    _NUMBER

    push    HL              ; Returned value.
    NEXT

_NUMBER:
    push    DE              ; Save DE so we don't trash it.
    ld      D, 0            ; Initialize to 0.
    ld      E, 0

    ; Make sure that B is not greater than 4.
    ld      A, B
    cp      5
    jp      m, _NUMBER_size_ok
    ld      B, 4

    jp      _NUMBER_loop_start

_NUMBER_size_ok:
_NUMBER_loop:
    ; We've read in a character, so we want to shift
    ; DE left four times to make space for the next character.
    ; Unfortunately the Z80 has no 16-bit shift so we have to implement
    ; it ourselves!

    ld      A, D

    ; Do the shift four times.
    repeat  4

    ; Shift A left.
    sla     A

    ; Shift E left. Top bit will be in carry flag.
    sla     E

    ; Add carry flag to A.
    adc     A, 0

    endrepeat

    ld      D, A

_NUMBER_loop_start
    ; Read in a character and convert into a number in the range
    ; 0-15.
    ld      A, (HL)
    inc     HL
    call    _NUMBER_get_digit

    ; OR with E. Move result back into E.
    or      A, E
    ld      E, A

    djnz    _NUMBER_loop    ; Repeat if we've not read all the characters.

    ; At this point our number should be in DE.
    ; Move it into HL and restore DE.
    ld      H, D
    ld      L, E

    pop     DE
    ret

    ; Given a hexadecimal digit in A,
    ; return its value in A.
_NUMBER_get_digit:
    cp      $3a
    jp      m, _NUMBER_get_digit_1
    cp      $47
    jp      m, _NUMBER_get_digit_2

    ; In range 'a'-'f'. Subtract $57.
    sub     A, $57
    ret

_NUMBER_get_digit_1:
    ; In range '0'-'9'. Subtract $30.
    sub     A, $30
    ret

_NUMBER_get_digit_2:
    ; In range 'A'-'F'. Subtract $37.
    sub     A, $37
    ret

    DEFCODE "STRING", 6, STRING
    call    _STRING
    ld      C, B
    ld      B, 0
    push    BC
    push    HL
    NEXT

_STRING:
    ld      HL, $8100
    ld      B, 0
_STRING_start:
    call    _KEY
    cp      '"'
    jp      nz, _STRING_start

    ; Now the string has started.
_STRING_get:
    call    _KEY

    ; Is this the end of the string?
    cp      '"'
    jp      z, _STRING_done

    ; If not, store this character.
    ld      (HL), A
    inc     HL
    inc     B

    jp      _STRING_get

_STRING_done:
    ld      HL, $8100
    ret


    DEFCODE "FIND", 4, FIND
    pop     BC              ; Size
    pop     HL              ; Start address.

    ld      B, C

    call    _FIND

    push    HL

    NEXT

_FIND:
    push    DE              ; Save DE so we don't trash it.
    push    HL              ; Save HL.
    push    BC              ; Save B.

    ld      IY, (LATEST)    ; Load address of latest entry in dictionary.

_FIND_loop:
    push    IY
    pop     HL

    pop     BC
    push    BC

    ld      A, (IY+2)       ; Size is 2 bytes into the entry.
    cp      B               ; Compare this size to the one given in B.

    jp      z, _FIND_test   ; If they're equal, we've potentially found something.
    jp      _FIND_next      ; Otherwise skip onto the next one.

_FIND_test:
    pop     BC              ; Restore B and HL and save them again.
    pop     HL
    push    HL
    push    BC

    push    IY
    pop     DE
    INC2    DE              ; This is IY+3.
    inc     DE

_FIND_test_loop:
    ld      A, (DE)         ; Load the character of the name into A.
    ld      C, (HL)         ; Load the character of the test string into D.
    cp      C               ; Compare.

    jp      nz, _FIND_next

    inc     DE
    inc     HL

    djnz    _FIND_test_loop

    ; If we get here then we've tested each character of the name
    ; and found them to be the same.
    ; This means we've found the word!
    ;
    ; At this point DE should point to the codeword.
_FIND_found:
    ; First restore the stuff on the stack.
    pop     BC

    ; We don't want to actually restore HL,
    ; just increment the SP.
    ;
    ; Move the codeword into HL.
    pop     HL

    ld      H, D
    ld      L, E

    pop     DE
    
    ret

_FIND_next:
    ld      L, (IY)         ; Load address of next word in little-endian format.
    ld      H, (IY+1)

    ; Fail if the address is 0.
    ld      A, L
    cp      H
    jp      nz, _FIND_not_zero
    cp      0
    jp      nz, _FIND_not_zero

    jp      _FIND_not_found

_FIND_not_zero:

    push    HL
    pop     IY

    jp      _FIND_loop

_FIND_not_found:
    pop     BC
    pop     HL
    pop     DE

    ld      H, 0
    ld      L, 0
    ret

    DEFCODE "LIT", 3, LIT
    ; At this point DE points to the literal to be pushed.
    push    DE
    pop     IY

    ld      L, (IY)
    ld      H, (IY+1)

    push    HL

    ; Skip over the literal so we don't execute it!
    INC2    DE

    NEXT

    DEFCODE "BRANCH", 6, BRANCH
    ; At this point DE points 2 ahead of the codeword
    ; of BRANCH, handily to the branch offset.
    ;
    ; Move it into IY so we can load the branch offset.
    push    DE
    pop     IY

    ld      L, (IY)
    ld      H, (IY+1)

    ; Decrement DE twice because it previously pointed
    ; two bytes ahead of the BRANCH.
    DEC2    DE

    ; Now add the offset.
    adc     HL, DE
    ld      D, H
    ld      E, L

    ; Execute next word, now that we've updated DE.
    NEXT

    DEFCODE "EXIT", 4, EXIT
    POPRSP
    NEXT

    DEFCODE "INTERPRET", 9, INTERPRET
    ; Pop string address and size 
    pop     HL
    pop     BC
    ld      B, C

    ; Save HL because it will be overwritten by FIND.
    push    HL

    call    _FIND

    ld      A, L
    cp      0
    jp      nz, _INTERPRET_found
    ld      A, H
    cp      0
    jp      nz, _INTERPRET_found

_INTERPRET_number:
    ; Restore HL.
    pop     HL

    call    _NUMBER

    push    HL
    NEXT

_INTERPRET_found:
    ; We can discard the saved value of HL.
    INC2    SP

    ; Execute the found word!
    jp      (HL)

    DEFCODE "END", 3, END
    halt

    DEFWORD "GOODBYE", 7, GOODBYE
    addr    LIT
    addr    8
    addr    LIT
    addr    str_GOODBYE

    addr    PRINT
    addr    NEWLINE

    addr    END

    DEFWORD "QUIT", 4, QUIT
    ; Size and address of the prompt string.
    addr    LIT
    addr    8
    addr    LIT
    addr    str_ZFORTH_prompt

    ; Print the prompt.
    addr    PRINT

    ; Get a word and interpret it.
    addr    WORD
    addr    INTERPRET

    ; Newline, print the stack.
    addr    NEWLINE
    addr    DUMPSTACK

    ; And repeat!
    addr    BRANCH
    addr    -18

LATEST:
    addr    LINK

str_ZFORTH_prompt:
    text    "ZFORTH> "
str_GOODBYE:
    text    "GOODBYE!"

; Start of RAM.

    org     $8000
_WORD_buf:
