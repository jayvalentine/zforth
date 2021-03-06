
; Register allocation.
; FORTH requires some registers to be allocated for specific functions.

; IP - the Interpreter Pointer.
; IP always points to the address of the next subroutine to be executed.
; In our case, the IP is mapped onto DE.

; W - the Working register.
; W is used for holding the address of the subroutine when we execute it.
; W is mapped onto HL.

; Some macros.

; System macros.

; Syscall.
    macro zsys, number
    ld      A, \number << 1
    rst     48
    endmacro

SWRITE set 0
SREAD set 1

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
    ex      DE, HL      ; Exchange DE and HL.
    
    ld      E, (HL)     ; Load the address of the next word into DE,
    inc     HL          ; incrementing HL as we go.
    ld      D, (HL)
    inc     HL

    ex      DE, HL      ; Exchange back.
                        ; The codeword to be executed is now in
                        ; HL, and the address of the next one is in DE.

    jp      (HL)        ; Execute the subroutine!
    endmacro

; Program start.
    org     $8000
start:
    ; Save the position of the stack pointer on entry so we can reset it when we exit.
    ld      (_system_stack), SP

    ; IX points to the Return Stack, which grows upwards from
    ; 0xE000.
    ;
    ; SP is the parameter stack, which grows downwards and has already been set up for us
    ; by the system.
    ;
    ; Having one grow up and the other down allows them to share the space
    ; more efficiently - space not used by one stack can be used
    ; by the other.
    ld      IX, $E000
    
    ; No data initialization required - this will have been loaded by the bootloader/OS.

    ; To make things neater, move the cursor up by one.
    ld      HL, str_up_by_one
    ld      B, 3
    call    _PRINT

    ; Return value, assuming it's not explicitly given.
    ld      HL, 0
    push    HL

    ld      DE, cold_start
    NEXT

cold_start:
    addr    QUIT
    addr    GOODBYE

; DOCOL - "Do Colon"
; This is Forth's interpreter.
; Its job is to save the 'old' IP value and set IP
; to the address of the next codeword to be executed.
DOCOL:
    PUSHRSP             ; Push IP onto the stack.

    ; At this point, if DOCOL has been called (and not just jumped to)
    ; the return address will be on the stack.
    ; Handily, this return address is also the address of the first
    ; parameter word of the colon definition! How neat!
    ;
    ; So, to set IP to the address of the first parameter word, we can
    ; just pop DE off the stack!
    pop     DE

    NEXT                ; Execute the first word of this definition!

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
    call    DOCOL
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

    macro   DEFIMMED, name, length, label
name_\label:
    addr    LINK
LINK set name_\label
    byte    $80 | \length
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

    DEFCODE "AND", 3, AND
    ; Load top two items from stack.
    pop     HL
    pop     BC
    
    ; AND top half.
    ld      A, H
    and     B
    ld      H, A

    ; AND bottom half.
    ld      A, L
    and     C
    ld      L, A

    ; Push result to stack.
    push    HL
    NEXT


; Comparisons.
    ; Compares TOS to NOS, returns TRUE iff TOS < NOS.
    DEFCODE "<", 1, LESS
    pop     HL
    pop     BC

    ; Compare top half.
    ld      A, H
    cp      B
    jp      m, LESS_true

    ; Otherwise compare bottom half.
    ld      A, L
    cp      C
    jp      m, LESS_true

    ; Not less-than, so push 0 to stack.
    ld      HL, 0

    jp      LESS_done

LESS_true:
    ld      HL, 1

LESS_done:
    push    HL
    NEXT

    DEFCODE "=", 1, EQ
    pop     HL
    pop     BC

    ; Compare top half.
    ld      A, H
    cp      B
    jp      nz, EQ_false

    ; Compare bottom half.
    ld      A, L
    cp      C
    jp      nz, EQ_false

    ; Operands are equal.
    ld      HL, 1
    jp      EQ_done

EQ_false:
    ld      HL, 0

EQ_done:
    push    HL
    NEXT

    DEFCODE "0=", 2, EQ0
    pop     HL

    ; First compare H and L. If they aren't equal
    ; then this obviously can't be 0.
    ld      A, H
    cp      L
    jp      nz, EQ0_false

    ; Now compare H to 0.
    cp      0
    jp      nz, EQ0_false

    ld      HL, 1
    jp      EQ0_done

EQ0_false:
    ld      HL, 0

EQ0_done:
    push    HL
    NEXT

; Random number generation.
    DEFCODE "RAND", 4, RAND
    ld      HL, (var_RAND)
    push    HL

    ld      A, H
    rra

    ld      A, L
    rra

    xor     H

    ld      H, A
    ld      A, L
    rra

    ld      A, H
    rra

    xor     L

    ld      L, A

    xor     H
    ld      H, A

    ; Now store back into RAND.
    ld      (var_RAND), HL

    scf
    ccf

    NEXT

    ; CHOOSE selects one of the two items on the top of the stack,
    ; at random.
    DEFWORD "CHOOSE", 6, CHOOSE
    addr    RAND            ; Generate a random number.

    addr    LIT             ; Which half is the random number in?
    addr    $0001
    addr    AND

    addr    BRANCH0         ; Either swap or don't swap, depending on
    addr    CHOOSE_skip     ; the value in the lowest bit of RAND.
                            ; This should have a uniform distribution.
    addr    SWAP
CHOOSE_skip:

    addr    DROP
    addr    EXIT

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
    push    HL
    push    BC

    ld      HL, (var_KEY_HEAD)
    ld      BC, (var_KEY_TAIL)

    ; Do we have characters in the buffer?
    ; If so skip straight to reading them.
    ld      A, H
    cp      B
    jp      nz, _KEY_read_buffer
    ld      A, L
    cp      C
    jp      nz, _KEY_read_buffer

    ; Otherwise we fill the buffer with characters from the serial line
    ; until we hit a LF.
    call    _NEWLINE
    ld      HL, str_ZFORTH_prompt
    ld      B, 8
    call    _PRINT

    ; Reset head and tail.
    ld      HL, _mem_KEY
    ld      BC, _mem_KEY

    ; Wait for ready.
_KEY_not_ready:
    zsys    SREAD           ; Read from serial port.

    cp      $0d             ; Is this a newline?

    ; If so, we're done.
    jp      z, _KEY_fill_done

    cp      $7f             ; Is this a backspace or delete?
    jp      z, _KEY_backsp  ; Check both because some terminal emulators
    cp      $08             ; seem to use one, and others use the other.
    jp      z, _KEY_backsp

    jp      _KEY_store

_KEY_backsp:

    ; If we're already at the start of the buffer then we can't
    ; go back any further.
    ;
    ; Luckily for us, at this point HL holds the start of the
    ; buffer, so we can use that for comparison.
    ld      A, B
    cp      H
    jp      nz, _KEY_not_at_start

    ; At this point we've checked the top half and found them to be equal.
    ld      A, C
    cp      L
    jp      z, _KEY_not_ready

_KEY_not_at_start:
    call    _KEY_ANSI_backspace

    ; Emit a space to cover up what was in this space before.
    push    HL
    ld      L, $20
    zsys    SWRITE
    pop     HL
    
    call    _KEY_ANSI_backspace

    dec     BC

    jp      _KEY_not_ready

    ; Otherwise store the character in the buffer.
_KEY_store:
    cp      $1b             ; Don't emit or buffer ESC,
    jp      z, _KEY_not_ready

    push    HL
    ld      (BC), A
    ld      L, A
    zsys    SWRITE
    pop     HL

    inc     BC              ; Increment the tail pointer.

    jp      _KEY_not_ready

_KEY_fill_done:
    ld      A, ' '          ; Store a terminating space.
    ld      (BC), A

    push    HL
    ld      L, A
    zsys    SWRITE
    pop     HL

    inc     BC

    ; Store the updated tail pointer.
    ld      (var_KEY_TAIL), BC

_KEY_read_buffer:
    ld      A, (HL)         ; Get the character.

    ; Increment the head pointer and store.
    inc     HL
    ld      (var_KEY_HEAD), HL

    pop     BC
    pop     HL

    ret

; Emit the ANSI code to move cursor back.
_KEY_ANSI_backspace:
    push    HL
    ld      L, $1b
    zsys    SWRITE
    ld      L, $5b
    zsys    SWRITE
    ld      L, $44
    zsys    SWRITE
    pop     HL

    ret

    ; EMIT writes a character to the serial line.
    DEFCODE "EMIT", 4, EMIT
    pop     HL
    zsys    SWRITE
    NEXT

    ; DOT ('.') prints the top of the stack.
    DEFCODE ".", 1, DOT
    pop     HL
    call    _DOT
    NEXT

_DOT:
    ; Print a '$', indicating that this is a hexadecimal number.
    push    HL
    ld      L, '$'
    zsys    SWRITE
    pop     HL

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
    
    push    HL
    ld      L, A
    zsys    SWRITE
    pop     HL

    ret

print_hex_single_digit:
    add     A, $30
    
    push    HL
    ld      L, A
    zsys    SWRITE
    pop     HL

    ret

    ; PRINT writes a string to the serial line.
    ; The TOS is the address of the string to write,
    ; and NOS is the size.
    DEFCODE "PRINT", 5, PRINT
    pop     HL
    pop     BC
    ld      B, C
    call    _PRINT
    NEXT

_PRINT:
_PRINT_loop:
    ld      A, (HL)

    push    HL
    ld      L, A
    zsys    SWRITE
    pop     HL

    inc     HL
    djnz    _PRINT_loop

_PRINT_done:
    ret

    DEFCODE "CR", 2, CR
    call    _NEWLINE
    NEXT

_NEWLINE:
    push    HL
    ld      L, $0d
    zsys    SWRITE
    ld      L, $0a
    zsys    SWRITE
    pop     HL
    ret

    DEFCODE "DUMPSTACK", 9, DUMPSTACK
    ld      (_mem_SCRATCH), SP
    ld      IY, (_mem_SCRATCH)

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
    cp      $0a
    jp      z, _WORD_skip

    ld      HL, _mem_WORD   ; Load start address of buffer
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
    cp      $0a
    jp      z, _WORD_done

    ld      (HL), A         ; If not, store the character
    inc     HL              ; and move onto the next location.
    inc     B

    jp      _WORD_get       ; Repeat.

_WORD_done:
    ld      HL, _mem_WORD
    ret

    ; NUMBER parses a number out of the given string.
    DEFCODE "NUMBER", 6, NUMBER
    pop     HL              ; Base address of string.
    pop     BC              ; Size.
    ld      B, C

    call    _NUMBER

    push    HL              ; Returned value.
    NEXT

_NUMBER:
    push    DE              ; Save DE, BC so we don't trash them.
    push    BC

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

    cp      $ff
    jp      nz, _NUMBER_valid

    pop     BC
    pop     DE

    ld      A, $ff
    ret

_NUMBER_valid

    ; OR with E. Move result back into E.
    or      A, E
    ld      E, A

    djnz    _NUMBER_loop    ; Repeat if we've not read all the characters.

    ; At this point our number should be in DE.
    ; Move it into HL and restore DE.
    ld      H, D
    ld      L, E

    pop     BC
    pop     DE

    ld      A, $00
    ret

    ; Given a hexadecimal digit in A,
    ; return its value in A.
_NUMBER_get_digit:
    cp      $3a
    jp      m, _NUMBER_get_digit_1
    cp      $47
    jp      m, _NUMBER_get_digit_2

    cp      $61
    jp      m, _NUMBER_get_digit_invalid
    ; In range 'a'-'f'. Subtract $57.
    sub     A, $57
    ret

_NUMBER_get_digit_1:
    cp      $30
    jp      m, _NUMBER_get_digit_invalid
    ; In range '0'-'9'. Subtract $30.
    sub     A, $30
    ret

_NUMBER_get_digit_2:
    cp      $41
    jp      m, _NUMBER_get_digit_invalid
    ; In range 'A'-'F'. Subtract $37.
    sub     A, $37
    ret

_NUMBER_get_digit_invalid:
    ld      A, $ff
    ret

    ; STRING allows the user to create a string.
    ; If interpreting, it places the string onto the stack.
    ; If compiling, it compiles a special word, LITSTRING, followed
    ; by the string into the definition.
    DEFIMMED "STRING", 6, STRING
    call    _STRING

    ; Figure out what mode we're in.
    ld      A, (var_STATE)
    bit     0, A
    jp      z, STRING_interpret

STRING_compile:
    push    DE              ; Save DE because we're going to use it.

    push    HL              ; Save BC and HL because they're
    push    BC              ; going to get trashed by COMMA.
    
    ld      BC, LITSTRING   ; Compile LITSTRING.
    call    _COMMA

    pop     BC              ; Restore BC. B holds the size of the string.
    ld      A, B

    ld      DE, (var_HERE)  ; Now compile the size into the definition.
    ld      (DE), A
    inc     DE

    pop     HL              ; Restore HL, which holds the address of the
                            ; string that we read from the user.

    ld      C, A            ; Load the size into BC as a 16-bit byte count,
    ld      B, 0            ; so we can use LDIR.

    ldir                    ; Copy the string into the definition.

    ld      (var_HERE), DE  ; Save the updated HERE.

    pop     DE              ; Restore DE and finish.
    NEXT

STRING_interpret:
    ld      C, B
    ld      B, 0
    push    BC
    push    HL
    NEXT

_STRING:
    ld      HL, _mem_STRING
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
    ld      HL, _mem_STRING
    ret

; These are compilation-related words.
    DEFCODE ":", 1, COLON
    ; Put the interpreter into COMPILE mode.
    ld      HL, var_STATE
    set     0, (HL)

    ; Save DE because we're going to use it.
    push    DE

    ; Set up the dictionary link pointer.
    ld      HL, (var_HERE)
    ld      DE, (var_LATEST)

    ; Save the new link pointer.
    ld      (var_LATEST), HL

    ; Store the old link pointer in the first two bytes
    ; of this new word.
    ld      (HL), E
    inc     HL
    ld      (HL), D
    inc     HL

    ; Save HL because it'll be trashed by WORD.
    push    HL

    ; Get the word name and size.
    call    _WORD

    ; Restore the old value of HL, which points to HERE.
    pop     DE

    ; Store the size.
    ld      A, B
    ld      (DE), A
    inc     DE

    ; At this point, DE points to the location
    ; of the first character of the name, and HL
    ; points to the WORD buffer. B holds the size.
    ; Zero B and we can use a block transfer instruction.
    ld      C, B
    ld      B, 0
    ldir

    ; Now DE points to the codeword.
    ld      HL, _COLON_CODE
    ld      C, _COLON_CODE_END - _COLON_CODE
    ldir

    ld      (var_HERE), DE

    pop     DE

    NEXT

_COLON_CODE:
    call    DOCOL
_COLON_CODE_END:

    DEFCODE ",", 1, COMMA
    pop     BC
    call    _COMMA
    NEXT

_COMMA:
    ; Load HERE into HL.
    ld      HL, (var_HERE)
    ld      (HL), C
    inc     HL
    ld      (HL), B
    inc     HL
    ld      (var_HERE), HL
    ret

    DEFIMMED ";", 1, ENDCOLON
    ; Append EXIT onto the currently-defining word.
    ld      BC, EXIT
    call    _COMMA

    ; Move back into INTERPRET mode.
    ld      HL, var_STATE
    res     0, (HL)

    ld      HL, str_DEFINED
    ld      B, 9
    call    _PRINT

    ld      HL, (var_LATEST)
    INC2    HL
    ld      B, (HL)
    inc     HL
    call    _PRINT
    NEXT

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

    ; Load address of latest entry in dictionary.
    ld      IY, (var_LATEST)

_FIND_loop:
    push    IY
    pop     HL

    pop     BC
    push    BC

    ld      A, (IY+2)       ; Size is 2 bytes into the entry.
    and     A, $7f          ; Mask out the immediate flag, otherwise the comparison
                            ; will fail.
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
    ; Move the address of this word (in IY) into HL.
    INC2    SP
    push    IY
    pop     HL

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
    ; Move it into HL for faster access.
    ex      DE, HL

    ld      C, (HL)
    inc     HL
    ld      B, (HL)
    inc     HL

    push    BC

    ex      DE, HL

    NEXT

    ; LITSTRING pushes a literal string, embedded in a definition,
    ; onto the stack.
    ; TOS becomes the string address, and NOS becomes the size.
    DEFCODE "LITSTRING", 9, LITSTRING
    ex      DE, HL      ; Exchange DE and HL for speed.

    ; At this point HL points to the size.
    ld      A, (HL)
    inc     HL

    ld      C, A
    ld      B, 0

    push    BC

    ; Now that we've loaded the size and incremented DE, it now
    ; points to the string itself.
    push    HL

    ; Skip the string.
    add     HL, BC

    ; Now HL points to the byte after the end of the string.
    ; This is the location of the next word in this definition.
    ; Exchange DE/HL and do NEXT.
    ex      DE, HL
    NEXT

    DEFCODE "BRANCH", 6, BRANCH
    ; At this point DE points 2 ahead of the codeword
    ; of BRANCH, handily to the branch address.
    ;
    ; Move it into IY so we can load the branch address.
    push    DE
    pop     IY

    ld      E, (IY)
    ld      D, (IY+1)

    ; Execute next word, now that we've updated DE.
    NEXT

    ; BRANCH0 takes a branch if the TOS is == 0.
    DEFCODE "BRANCH0", 7, BRANCH0
    ; Get the top of the stack. If 0 then we're
    ; going to take this branch.
    pop     BC

    ; Little hack, but just compare the lower half of BC.
    ld      A, C
    cp      $00
    jp      z, BRANCH0_take_branch

    ; We're not taking the branch, so increment DE again and jump.
    INC2    DE
    NEXT

BRANCH0_take_branch:
    push    DE
    pop     IY

    ld      E, (IY)
    ld      D, (IY+1)

    ; Execute the word now that we've changed DE.
    NEXT

    ; BRANCH1 takes a branch if the TOS is != 0.
    DEFCODE "BRANCH1", 7, BRANCH1
    ; Get the top of the stack. If != 0 then we're
    ; going to take this branch.
    pop     BC

    ; Little hack, but just compare the lower half of BC.
    ld      A, C
    cp      $00
    jp      nz, BRANCH1_take_branch

    ; We're not taking the branch, so increment DE again and jump.
    INC2    DE
    NEXT

BRANCH1_take_branch:
    push    DE
    pop     IY

    ld      E, (IY)
    ld      D, (IY+1)

    ; Execute the word now that we've changed DE.
    NEXT

    ; IF can be combined with THEN to allow for conditional
    ; execution in defined words.
    DEFIMMED "IF", 2, IF
    ; Compile a BRANCH0.
    ld      BC, BRANCH0
    call    _COMMA

    ; push HERE to the stack. HERE will point to the location
    ; of the branch offset that needs to be fixed up.
    ld      HL, (var_HERE)
    push    HL

    ; Compile a dummy offset. This will need to be updated by THEN.
    ld      BC, $ffff
    call    _COMMA
    NEXT

    ; THEN terminates an IF. It fetches the address of the branch offset
    ; and fixes it up to point to this HERE.
    DEFIMMED "THEN", 4, THEN
    ; Load HERE.
    ld      HL, (var_HERE)

    ; Load the address of the address to be fixed up.
    pop     IY

    ld      (IY), L
    ld      (IY+1), H

    NEXT

    ; BEGIN starts a loop construct.
    DEFIMMED "BEGIN", 5, BEGIN
    ; Push HERE onto the stack.
    ld      HL, (var_HERE)
    push    HL
    NEXT

    ; WHILE will compile a BRANCH1 to the location
    ; marked by the most recent BEGIN.
    DEFIMMED "WHILE", 5, WHILE
    ld      BC, BRANCH1     ; Compile a BRANCH1.
    call    _COMMA

    pop     BC              ; Compile the branch offset.
    call    _COMMA

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
    push    HL

    call    _NUMBER

    cp      $00
    jp      z, _INTERPRET_number_valid

    ; NUMBER has returned an error code, so this isn't
    ; a number but an undefined word.
    push    HL
    ld      L, '?'
    zsys    SWRITE
    pop     HL

    pop     HL
    call    _PRINT

    push    HL
    ld      L, ' '
    zsys    SWRITE
    pop     HL
    
    NEXT

_INTERPRET_number_valid:
    INC2    SP

    ; Figure out if we're in COMPILE or INTERPRET
    ; mode. If compile then bit 0 of STATE
    ; will be set.
    ld      A, (var_STATE)
    bit     0, A
    jp      z, _INTERPRET_number_push

    ; We're in compile mode, so compile LIT followed by this number.
    push    HL

    ld      BC, LIT
    call    _COMMA

    pop     BC
    call    _COMMA

    NEXT

_INTERPRET_number_push:
    push    HL
    NEXT

_INTERPRET_found:
    ; We can discard the saved value of HL.
    INC2    SP

    ; Get the size of the word.
    INC2    HL
    ld      A, (HL)
    ld      C, A

    ; Mask out the immediate flag.
    ; If we don't do this then the size will be wrong.
    and     A, $7f

    ; Load size into B.
    ld      B, A

_INTERPRET_skip_name:
    inc     HL
    djnz    _INTERPRET_skip_name

    inc     HL

    ; If the top bit of A is set, always execute this word,
    ; because it's immediate.
    bit     7, C
    jp      nz, _INTERPRET_found_execute

    ; Figure out if we're in COMPILE or INTERPRET
    ; mode. If compile then bit 0 of STATE
    ; will be set.
    ld      A, (var_STATE)
    bit     0, A
    jp      z, _INTERPRET_found_execute

    ; We're compiling, so move the word into BC
    ; and call COMMA.
    ld      B, H
    ld      C, L
    call    _COMMA
    NEXT

_INTERPRET_found_execute:
    ; Execute the found word!
    jp      (HL)

    ; Exit ZFORTH.
    DEFCODE "END", 3, END
    ; Return value is TOS.
    pop     HL
    
    ; Restore system stack pointer.
    ld      SP, (_system_stack)

    ; Return. Assumes that a return address is on the stack.
    ; I.e. that the ZFORTH entry point has been called like a subroutine.
    ret

    DEFWORD "GOODBYE", 7, GOODBYE
    addr    LIT
    addr    8
    addr    LIT
    addr    str_GOODBYE

    addr    PRINT
    addr    CR

    addr    END

    DEFWORD "QUIT", 4, QUIT
    ; Get a word and interpret it.
QUIT_loop:
    addr    WORD
    addr    INTERPRET

    ; And repeat!
    addr    BRANCH
    addr    QUIT_loop

; Start of readonly data.
str_ZFORTH_prompt:
    text    "ZFORTH> "
str_GOODBYE:
    text    "GOODBYE!"
str_DEFINED:
    text    "DEFINED: "
str_up_by_one:
    byte    $1b
    text    "[A"

_system_stack:
    blk     2

; Initialized variables.
var_LATEST:
    addr    LINK
var_KEY_HEAD:
    addr    _mem_KEY
var_KEY_TAIL:
    addr    _mem_KEY
var_STATE:
    byte    $00
var_HERE:
    addr    FREE_START
var_RAND:
    addr    DEF_RAND_SEED

; Uninitialized data.
_mem_KEY:
    blk     256
_mem_KEY_END:
_mem_WORD:
    blk     256
_mem_SCRATCH:
    blk     256
_mem_STRING:
    blk     256

FREE_START: